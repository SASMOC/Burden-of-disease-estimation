# =============================================================================
# 10_auto_update_bod.R — Self-updating burden-of-disease engine, ALL conditions
# =============================================================================
# One call, update_bod(new_data), re-estimates every condition in Kenya with
# SEVEN methods and ALL burden metrics:
#   Methods : Unadjusted, M1_Facility, M2_Catchment, M3_BedCapacity,
#             M1b_Level4_6, M5_MOH718   (same definitions as the disease scripts)
#             + Bayesian (INLA BYM2, nbinomial, M1-adjusted counts, offset log pop)
#   Metrics : cases, deaths, YLL, YLD, DALYs, prevalence / mortality / DALY
#             rate per 100,000   (annual averages over the estimation window)
# Condition = first LEVEL_CHARS characters of Final_Code (default 4, e.g. "5a11").
#
# Each call: (1) appends new records to Cache/bod_store.rds (de-duplicated on
# Event id), (2) re-derives the window from the data (latest N complete years),
# (3) recomputes the six classical methods for every condition, (4) refits the
# Bayesian model ONLY for conditions whose inputs changed, (5) writes
# Outputs/bod_estimates_latest.csv (long) + bod_estimates_wide.csv and appends
# to Cache/bod_estimates_history.rds with change vs the previous run.
#
# YLD = cases x disability weight (config/disability_weights.csv, prefix match).
# Conditions without a weight get YLD/DALY = NA (never a guessed value) unless
# default_dw is supplied. YLL = deaths x life expectancy of the county-sex-age-year
# stratum (CEMA life-expectancy table).
# =============================================================================

BOD_STORE   <- "Cache/bod_store.rds"
BOD_HISTORY <- "Cache/bod_estimates_history.rds"
BOD_BAYES   <- "Cache/bod_bayes_cache.rds"
BOD_LATEST  <- "Outputs/bod_estimates_latest.csv"
BOD_WIDE    <- "Outputs/bod_estimates_wide.csv"
BOD_DW      <- "config/disability_weights.csv"
BOD_ADJ     <- "Cache/county_gadm41.adj"
BOD_SHP     <- "kenya_shape_files/gadm41_KEN_1.shp"
BOD_METHODS <- c("Unadjusted", "M1_Facility", "M2_Catchment", "M3_BedCapacity",
                 "M1b_Level4_6", "M5_MOH718", "Bayesian")

.ckey <- function(x) stringr::str_to_lower(stringr::str_replace_all(x, "[^A-Za-z]", ""))
.age_band <- function(a) dplyr::case_when(
  a %in% c("0 - 1", "1-4")            ~ "00-04", a %in% c("5-9", "10-14") ~ "05-14",
  a %in% c("15-19", "20-24", "25-29") ~ "15-29", a %in% c("30-34", "35-39", "40-44") ~ "30-44",
  a %in% c("45-49", "50-54", "55-59") ~ "45-59", !is.na(a) ~ "60+")

# --- 0. Standardise a cleaned tracker extract into the lean store schema ----
bod_standardise <- function(df, facilities_df) {
  if (!"Organisation unit code" %in% names(df)) return(df)      # already lean
  fac <- facilities_df |>
    dplyr::mutate(keph_level_name = dplyr::if_else(code == 13939, "Level 6", keph_level_name)) |>
    dplyr::select(code, county, keph_level_name)
  # Records with no reporting mode are classified from their own fields:
  # death certification fields -> Mortality; discharge-alive fields -> Morbidity;
  # both or neither -> left unclassified (excluded from estimates).
  col <- function(n) if (n %in% names(df)) !is.na(df[[n]]) else rep(FALSE, nrow(df))
  dead  <- col("M&MCCoD Manner of death") | col("M&MCCoD  Date and time of Death") |
    col("M&MCCoD_Birth/Death Notification Number") |
    col("M&MCCoD Report disease or condition directly leading to death on line a (Code)")
  alive <- col("M&MCCoD_Alive_Mode of discharge") | col("M&MCCoD_Alive_Date of Discharge")
  mode0 <- standardise_reporting_mode(df$`M&MCCoD  Reporting Mode`)
  miss  <- is.na(mode0)
  mode0[miss & dead & !alive] <- "Mortality"
  mode0[miss & alive & !dead] <- "Morbidity"
  message("[mode] missing reporting mode: ", sum(miss), " -> Mortality ", sum(miss & dead & !alive),
          ", Morbidity ", sum(miss & alive & !dead), ", unclassified ", sum(is.na(mode0)))
  df$.mode <- mode0
  df |>
    dplyr::filter(!is.na(Final_Code)) |>
    dplyr::transmute(Event, Final_Code = stringr::str_to_lower(Final_Code), Year, DateTime,
                     mode = .mode,
                     gender, age_group = fix_age_groups(`Age years categorical`),
                     org_code = as.character(`Organisation unit code`)) |>
    dplyr::left_join(dplyr::mutate(fac, code = as.character(code)), by = c("org_code" = "code")) |>
    dplyr::mutate(county = fix_county_names(county)) |>
    dplyr::select(-org_code)
}

# --- 1. Ingest: append + de-duplicate --------------------------------------
bod_ingest <- function(new_data, facilities_df) {
  store <- if (file.exists(BOD_STORE)) readRDS(BOD_STORE) else NULL
  if (!is.null(new_data)) {
    new <- bod_standardise(new_data, facilities_df)
    n0 <- if (is.null(store)) 0L else nrow(store)
    store <- dplyr::bind_rows(store, new)
    store <- if ("Event" %in% names(store)) dplyr::distinct(store, Event, .keep_all = TRUE) else dplyr::distinct(store)
    message("[ingest] +", nrow(store) - n0, " new records (store now ", nrow(store), ")")
    saveRDS(store, BOD_STORE)
  }
  if (is.null(store)) stop("No data yet: pass new_data on the first call.")
  store
}

# --- 2. Estimation window from the data ------------------------------------
bod_window <- function(store, n_years = 4, include_partial = FALSE) {
  yrs <- store |> dplyr::filter(!is.na(DateTime)) |> dplyr::group_by(Year) |>
    dplyr::summarise(months = dplyr::n_distinct(format(DateTime, "%m")), .groups = "drop") |>
    dplyr::arrange(Year)
  last <- max(yrs$Year); keep <- yrs$Year
  if (!include_partial && yrs$months[yrs$Year == last] < 12) {
    message("[window] ", last, " has only ", yrs$months[yrs$Year == last],
            " months -> excluded (include_partial = TRUE to keep)")
    keep <- setdiff(keep, last)
  }
  utils::tail(sort(keep), n_years)
}

# --- 3. Reference inputs (weights, population, life expectancy, DW) ---------
bod_reference <- function(years, default_dw = NA_real_) {
  ref <- load_reference_data()
  carry <- function(d, ycol) {            # years beyond the table reuse the latest year
    have <- sort(unique(d[[ycol]]))
    for (y in setdiff(years, have)) {
      src <- max(have[have < y], min(have)); message("[reference] ", ycol, ": ", y, " -> using ", src)
      d <- dplyr::bind_rows(d, dplyr::filter(d, .data[[ycol]] == src) |> dplyr::mutate(!!ycol := y))
    }
    d
  }
  popraw <- ref$population |>
    dplyr::mutate(county = fix_county_names(county),
                  # Excel turned 1-4, 5-9, 10-14 into dates in the source table
                  age_group = fix_age_groups(dplyr::recode(age_group, "4-Jan" = "1-4", "9-May" = "5-9", "14-Oct" = "10-14")))
  le <- popraw |> dplyr::group_by(county, Year = year_le, gender = sex, age_group) |>
    dplyr::summarise(le = stats::weighted.mean(life_expectancy, population, na.rm = TRUE), .groups = "drop") |>
    carry("Year")
  le_nat <- le |> dplyr::group_by(Year, gender, age_group) |>
    dplyr::summarise(le_nat = mean(le, na.rm = TRUE), .groups = "drop")
  popcell <- popraw |>
    dplyr::mutate(age_band = .age_band(age_group), ck = .ckey(county)) |>
    dplyr::filter(!is.na(age_band)) |>
    dplyr::group_by(ck, Year = year_le, age_band) |>
    dplyr::summarise(le = stats::weighted.mean(life_expectancy, population, na.rm = TRUE),
                     population = sum(population, na.rm = TRUE), .groups = "drop") |>
    carry("Year")
  natpop <- popcell |> dplyr::group_by(Year) |> dplyr::summarise(p = sum(population))
  # moh 718: carry the last reported year forward
  moh <- ref$moh_718_rate |> dplyr::select(year, county, rate1)
  for (y in setdiff(years, unique(moh$year))) {
    src <- max(moh$year[moh$year < y], min(moh$year))
    message("[reference] MOH 718 rate: ", y, " -> using ", src)
    moh <- dplyr::bind_rows(moh, dplyr::filter(moh, year == src) |> dplyr::mutate(year = y))
  }
  w <- ref$weightings
  dw <- readr::read_csv(BOD_DW, show_col_types = FALSE) |>
    dplyr::mutate(code_prefix = stringr::str_to_lower(code_prefix))
  list(le = le, le_nat = le_nat, popcell = popcell, avg_pop = mean(natpop$p[natpop$Year %in% years]),
       moh = moh, dw = dw, default_dw = default_dw,
       w_fac = w[, c("county", "Keph level", "weights_by_facility")],
       w_cat = w[!is.na(weight_by_catchment_pop), c("county", "Keph level", "weight_by_catchment_pop")],
       w_bed = w[!is.na(weght_by_bedcapacity), c("county", "weght_by_bedcapacity")],
       w_l46 = w[!is.na(`weights by_level_4_6`), c("county", "weights by_level_4_6")])
}

bod_dw_for <- function(conds, r) {
  vapply(conds, function(cd) {
    hit <- r$dw$dw[startsWith(cd, r$dw$code_prefix)]       # prefix no longer than the condition
    if (length(hit)) hit[which.max(nchar(r$dw$code_prefix[startsWith(cd, r$dw$code_prefix)]))] else r$default_dw
  }, numeric(1))
}

# --- 4. Six classical methods, every condition at once ----------------------
bod_classical <- function(rec, r, years) {
  data.table::setDT(rec)
  a <- rec[, .(cases = .N), by = .(cond, county, Year, gender, keph_level_name, age_group, mode)]
  a <- merge(a, data.table::as.data.table(r$le), by.x = c("county", "Year", "gender", "age_group"),
             by.y = c("county", "Year", "gender", "age_group"), all.x = TRUE)
  a <- merge(a, data.table::as.data.table(r$le_nat), by = c("Year", "gender", "age_group"), all.x = TRUE)
  a[, le := data.table::fcoalesce(le, le_nat)]
  mk <- function(method, w) {
    out <- copy(a); out[, w := w]; out[, method := method]; out
  }
  kl <- match(paste(a$county, a$keph_level_name),
              paste(r$w_fac$county, r$w_fac$`Keph level`))
  kc <- match(paste(a$county, a$keph_level_name), paste(r$w_cat$county, r$w_cat$`Keph level`))
  w_fac <- r$w_fac$weights_by_facility[kl]
  w_cat <- r$w_cat$weight_by_catchment_pop[kc]
  w_bed <- r$w_bed$weght_by_bedcapacity[match(a$county, r$w_bed$county)]
  w_l46 <- r$w_l46$`weights by_level_4_6`[match(a$county, r$w_l46$county)]
  w_718 <- r$moh$rate1[match(paste(a$Year, a$county), paste(r$moh$year, r$moh$county))] / 100
  all <- data.table::rbindlist(list(
    mk("Unadjusted", 1), mk("M1_Facility", w_fac), mk("M2_Catchment", w_cat),
    mk("M3_BedCapacity", w_bed), mk("M1b_Level4_6", w_l46), mk("M5_MOH718", w_718)))
  all[, v := data.table::fifelse(is.na(w) | w == 0, 0, round(cases / w))]
  all[, yll := data.table::fifelse(mode == "Mortality", v * data.table::fcoalesce(le, 0), 0)]
  agg <- all[mode %in% c("Morbidity", "Mortality"), .(cases = sum(v[mode == "Morbidity"]), deaths = sum(v[mode == "Mortality"]),
                 yll = sum(yll)), by = .(cond, Year, method)]
  agg <- agg[, .(cases = sum(cases) / length(years), deaths = sum(deaths) / length(years),
                 yll = sum(yll) / length(years)), by = .(cond, method)]
  tibble::as_tibble(agg)
}

# --- 5. Bayesian model for one condition (cases + deaths -> all metrics) ----
bod_bayes_one <- function(cd, inp, r, dw, n_samp = 500) {
  years <- inp$years
  fit_side <- function(side) {
    rec <- inp$morb_or_mort[[side]]
    rec <- rec[startsWith(rec$Final_Code, cd), ]
    dat <- inp$grid |>
      dplyr::left_join(rec |> dplyr::group_by(ck, Year, age_band) |>
                         dplyr::summarise(y = round(sum(w_case)), .groups = "drop"), by = c("ck", "Year", "age_band")) |>
      dplyr::mutate(y = tidyr::replace_na(y, 0), year_factor = factor(Year), log_pop = log(population))
    fml <- y ~ age_band + year_factor +
      f(idx, model = "bym2", graph = BOD_ADJ, scale.model = TRUE,
        hyper = list(prec = list(prior = "pc.prec", param = c(0.76, 0.01)),
                     phi  = list(prior = "pc",      param = c(0.5, 0.5))))
    fit <- tryCatch(INLA::inla(fml, family = "nbinomial", data = dat, offset = log_pop,
                               control.fixed = list(mean = 0, prec = 1e-4, mean.intercept = 0, prec.intercept = 1e-4),
                               control.predictor = list(compute = TRUE, link = 1),
                               control.compute = list(config = TRUE), num.threads = "4:1", verbose = FALSE),
                    error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    smp <- INLA::inla.posterior.sample(n_samp, fit, selection = list(Predictor = 0), num.threads = "4:1")
    list(dat = dat, mat = vapply(smp, function(s) exp(s$latent[, 1]), numeric(nrow(dat))))
  }
  cs <- fit_side("morb"); ds <- fit_side("mort")
  if (is.null(cs)) return(NULL)
  N <- length(years)
  cases <- colSums(cs$mat) / N
  deaths <- if (is.null(ds)) rep(NA_real_, n_samp) else colSums(ds$mat) / N
  yll <- if (is.null(ds)) rep(NA_real_, n_samp) else colSums(ds$mat * ds$dat$le) / N
  yld <- if (is.na(dw)) rep(NA_real_, n_samp) else cases * dw
  S <- list(cases = cases, deaths = deaths, yll = yll, yld = yld, dalys = yld + yll,
            prevalence_per_100k = cases / r$avg_pop * 1e5, mortality_per_100k = deaths / r$avg_pop * 1e5,
            daly_rate_per_100k = (yld + yll) / r$avg_pop * 1e5)
  fp <- rlang::hash(list(cs$dat[, c("ck", "Year", "age_band", "y")], if (!is.null(ds)) ds$dat[, c("ck", "Year", "age_band", "y")],
                         dw, r$avg_pop, years))
  tibble::tibble(metric = names(S), estimate = vapply(S, mean, 0),
                 lower = vapply(S, quantile, 0, 0.025), upper = vapply(S, quantile, 0, 0.975),
                 fingerprint = fp)
}

# --- 6. Entry point ---------------------------------------------------------
update_bod <- function(new_data = NULL, n_years = 4, include_partial = FALSE, level_chars = 4,
                       claims_path = "claims data /SHA3.parquet",
                       util_path = "claims data /Aggregated_utilization_data_Jul2025-Jun2026-services.xlsx",
                       min_cases_bayes = 100, default_dw = NA_real_, only = NULL, force = FALSE) {
  dir.create("Cache", showWarnings = FALSE); dir.create("Outputs", showWarnings = FALSE)
  facilities_df <- readxl::read_excel("Geocodes_AllHealthFacilitiesInKenya.xlsx")
  store <- bod_ingest(new_data, facilities_df)
  years <- bod_window(store, n_years, include_partial)
  cat("[update_bod] window:", min(years), "-", max(years), "\n")
  r <- bod_reference(years, default_dw)

  rec <- store |> dplyr::filter(Year %in% years, !is.na(county), !is.na(Final_Code)) |>
    dplyr::mutate(cond = substr(Final_Code, 1, level_chars))
  if (!is.null(only)) rec <- dplyr::filter(rec, cond %in% only)
  conds <- sort(unique(rec$cond)); cat("[update_bod] conditions:", length(conds), "\n")

  # classical methods
  cl <- bod_classical(data.table::copy(rec), r, years)
  dwv <- stats::setNames(bod_dw_for(conds, r), conds)
  popn <- r$avg_pop
  long_cl <- cl |>
    dplyr::mutate(yld = cases * dwv[cond], dalys = yld + yll,
                  prevalence_per_100k = cases / popn * 1e5, mortality_per_100k = deaths / popn * 1e5,
                  daly_rate_per_100k = dalys / popn * 1e5) |>
    tidyr::pivot_longer(c(cases, deaths, yll, yld, dalys, prevalence_per_100k, mortality_per_100k, daly_rate_per_100k),
                        names_to = "metric", values_to = "estimate") |>
    dplyr::mutate(lower = NA_real_, upper = NA_real_)

  # Bayesian (cached by input fingerprint)
  shp <- sf::st_read(BOD_SHP, quiet = TRUE) |> dplyr::mutate(ck = .ckey(NAME_1), idx = dplyr::row_number())
  spdep::nb2INLA(BOD_ADJ, spdep::poly2nb(shp))
  grid <- tidyr::expand_grid(ck = shp$ck, Year = years, age_band = sort(unique(r$popcell$age_band))) |>
    dplyr::left_join(sf::st_drop_geometry(shp)[, c("ck", "idx")], by = "ck") |>
    dplyr::left_join(r$popcell, by = c("ck", "Year", "age_band")) |> dplyr::filter(!is.na(population), population > 0)
  rec_w <- rec |> dplyr::left_join(r$w_fac, by = c("county", "keph_level_name" = "Keph level")) |>
    dplyr::mutate(w_case = safe_div(1, weights_by_facility), ck = .ckey(county), age_band = .age_band(age_group)) |>
    dplyr::filter(!is.na(age_band), ck %in% shp$ck)
  inp <- list(years = years, grid = grid,
              morb_or_mort = list(morb = dplyr::filter(rec_w, mode == "Morbidity"),
                                  mort = dplyr::filter(rec_w, mode == "Mortality")))
  ncase <- table(inp$morb_or_mort$morb$cond)
  todo <- names(ncase)[ncase >= min_cases_bayes]
  cache <- if (file.exists(BOD_BAYES) && !force) readRDS(BOD_BAYES) else tibble::tibble()
  window <- paste(min(years), max(years), sep = "-")
  cat("[update_bod] Bayesian candidates (>=", min_cases_bayes, "cases):", length(todo), "\n")
  done <- 0; t0 <- Sys.time()
  for (cd in todo) {
    old <- cache[cache$cond == cd & cache$window == window, ]
    sub <- list(Final_Code = NULL)
    inp_cd <- inp; inp_cd$morb_or_mort <- lapply(inp$morb_or_mort, function(d) d[d$cond == cd, ])
    res <- tryCatch(bod_bayes_one(cd, inp_cd, r, dwv[[cd]]), error = function(e) NULL)
    if (!is.null(res) && nrow(old) && identical(unique(old$fingerprint), unique(res$fingerprint))) next
    cache <- cache[!(cache$cond == cd), ]
    if (!is.null(res)) cache <- dplyr::bind_rows(cache, dplyr::mutate(res, cond = cd, window = window))
    done <- done + 1
    if (done %% 25 == 0) { saveRDS(cache, BOD_BAYES)
      cat("   ... refit", done, "| elapsed", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n") }
  }
  saveRDS(cache, BOD_BAYES)
  long_b <- cache |> dplyr::filter(window == !!window, cond %in% conds) |>
    dplyr::mutate(method = "Bayesian") |> dplyr::select(cond, method, metric, estimate, lower, upper)

  # assemble + names + change vs previous run
  nm <- r_names(conds)
  out <- dplyr::bind_rows(long_cl, long_b) |>
    dplyr::mutate(condition = nm[cond], disability_weight = unname(dwv[cond]),
                  window = window, run_time = Sys.time()) |>
    dplyr::rename(icd_code = cond) |>
    dplyr::select(icd_code, condition, method, metric, estimate, lower, upper, disability_weight, window, run_time)
  hist <- if (file.exists(BOD_HISTORY)) readRDS(BOD_HISTORY) else NULL
  if (!is.null(hist)) {
    prev <- hist |> dplyr::group_by(icd_code, method, metric) |> dplyr::slice_max(run_time, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |> dplyr::transmute(icd_code, method, metric, prev_estimate = estimate, prev_window = window)
    out <- out |> dplyr::left_join(prev, by = c("icd_code", "method", "metric")) |>
      dplyr::mutate(change_pct = round(100 * (estimate - prev_estimate) / prev_estimate, 1))
  }
  saveRDS(dplyr::bind_rows(hist, dplyr::select(out, icd_code, condition, method, metric, estimate, lower, upper,
                                               disability_weight, window, run_time)), BOD_HISTORY)
  readr::write_csv(out |> dplyr::mutate(dplyr::across(c(estimate, lower, upper), ~ round(.x, 2))) |>
                     dplyr::select(-run_time), BOD_LATEST)
  wide <- out |> dplyr::select(icd_code, condition, disability_weight, method, metric, estimate) |>
    tidyr::pivot_wider(names_from = c(metric, method), values_from = estimate, names_sep = "__")
  readr::write_csv(wide, BOD_WIDE)
  tri <- bod_triangulate(out, bod_claims(claims_path, level_chars), bod_utilization(util_path, level_chars))
  if (!is.null(tri)) {
    readr::write_csv(dplyr::mutate(tri, dplyr::across(where(is.numeric), ~ round(.x, 2))), "Outputs/bod_triangulation_claims.csv")
    cat("[update_bod] claims triangulation:", sum(tri$claims_patients_annual > 0 | tri$util_patients_lower > 0), "conditions with claims/utilization;",
        sum(grepl("exceed ALL", tri$claims_check)), "where claims exceed every method\n")
  }
  cat("[update_bod] saved", BOD_LATEST, "(", nrow(out), "rows ) and", BOD_WIDE, "\n")
  invisible(out)
}

# --- 7. Claims triangulation (SHA claims, ICD-11 coded) ----------------------
# Distinct patients and claims per condition, annualised over the months with
# full coverage. SHA claims cover insured, claimed-for care only, so claimed
# patients are a LOWER BOUND on the treated population; they are compared with
# each method rather than used to rescale it. Cached; rebuilt only when the
# parquet file changes.
BOD_CLAIMS_CACHE <- "Cache/claims_by_condition.rds"
bod_claims <- function(path = "claims data /SHA3.parquet", level_chars = 4) {
  if (!file.exists(path)) { message("[claims] ", path, " not found -> triangulation skipped"); return(NULL) }
  stamp <- paste(file.mtime(path), level_chars)
  if (file.exists(BOD_CLAIMS_CACHE)) { c0 <- readRDS(BOD_CLAIMS_CACHE); if (identical(c0$stamp, stamp)) return(c0$tab) }
  message("[claims] summarising ", path, " (cached afterwards)")
  # Aggregated inside arrow (never loads the 3 GB table into R); ~30 min once, then cached.
  ds <- arrow::open_dataset(path)
  ms <- sort(dplyr::collect(dplyr::distinct(dplyr::mutate(ds, ym = substr(as.character(admission_date), 1, 7)), ym))$ym)
  keep <- ms[-c(1, length(ms))]       # first/last calendar months of the extract are partial
  tab <- ds |>
    dplyr::filter(!is.na(icd_codes), icd_codes != "") |>
    dplyr::mutate(cond = substr(tolower(icd_codes), 1, !!level_chars),
                  ym = substr(as.character(admission_date), 1, 7)) |>
    dplyr::filter(ym >= !!min(keep), ym <= !!max(keep)) |>
    dplyr::group_by(cond) |>
    dplyr::summarise(claims_patients = dplyr::n_distinct(national_id),
                     claims_episodes = dplyr::n_distinct(claim_id)) |>
    dplyr::collect() |>
    dplyr::mutate(claims_patients_annual = claims_patients * 12 / length(keep),
                  claims_episodes_annual = claims_episodes * 12 / length(keep),
                  claims_months = length(keep), claims_period = paste(min(keep), max(keep), sep = " to "))
  saveRDS(list(stamp = stamp, tab = tab), BOD_CLAIMS_CACHE); tab
}

# Aggregated utilization (12-month unique patients / encounters per ICD-11 code x
# intervention). Within a condition, patients can overlap across interventions, so
# the LOWER bound is the largest single code-intervention figure and the SUM
# (over codes, best intervention each) is an upper bound that may double count.
bod_utilization <- function(path = "claims data /Aggregated_utilization_data_Jul2025-Jun2026-services.xlsx",
                            level_chars = 4) {
  if (!file.exists(path)) { message("[utilization] ", path, " not found -> skipped"); return(NULL) }
  u <- readxl::read_excel(path)
  names(u) <- c("icd", "intervention", "patients", "encounters")
  u |> dplyr::filter(!is.na(icd)) |>
    dplyr::mutate(icd = stringr::str_to_lower(stringr::str_trim(icd)), cond = substr(icd, 1, level_chars)) |>
    dplyr::group_by(cond, icd) |>
    dplyr::summarise(p = max(patients, na.rm = TRUE), e = sum(encounters, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(cond) |>
    dplyr::summarise(util_patients_lower = max(p), util_patients_sum = sum(p),
                     util_encounters = sum(e), .groups = "drop")
}

bod_triangulate <- function(out, claims, util = NULL) {
  if (is.null(claims) && is.null(util)) return(NULL)
  if (is.null(claims)) claims <- tibble::tibble(cond = character(), claims_patients_annual = numeric())
  if (!is.null(util)) claims <- dplyr::full_join(claims, util, by = "cond")
  if (is.null(util)) claims$util_patients_lower <- NA_real_
  adj <- c("M1_Facility", "M2_Catchment", "M3_BedCapacity", "M1b_Level4_6", "M5_MOH718")
  est <- out |> dplyr::filter(metric == "cases") |>
    dplyr::select(icd_code, condition, method, estimate, lower, upper)
  wide <- est |> dplyr::select(-lower, -upper) |>
    tidyr::pivot_wider(names_from = method, values_from = estimate)
  have <- intersect(adj, names(wide))
  wide <- wide |> dplyr::rowwise() |>
    dplyr::mutate(adjusted_median = stats::median(dplyr::c_across(dplyr::all_of(have)), na.rm = TRUE),
                  adjusted_min = min(dplyr::c_across(dplyr::all_of(have)), na.rm = TRUE),
                  adjusted_max = max(dplyr::c_across(dplyr::all_of(have)), na.rm = TRUE)) |> dplyr::ungroup()
  b <- est |> dplyr::filter(method == "Bayesian") |>
    dplyr::select(icd_code, bayes_lower = lower, bayes_upper = upper)
  wide |> dplyr::left_join(b, by = "icd_code") |>
    dplyr::left_join(claims, by = c("icd_code" = "cond")) |>
    dplyr::mutate(
      claims_patients_annual = tidyr::replace_na(claims_patients_annual, 0),
      claims_to_unadjusted = claims_patients_annual / Unadjusted,
      claims_to_adjusted_median = claims_patients_annual / adjusted_median,
      claims_to_bayesian = claims_patients_annual / Bayesian,
      claims_check = dplyr::case_when(
        claims_patients_annual == 0                           ~ "No claims for this condition",
        claims_patients_annual > adjusted_max                 ~ "Claims exceed ALL methods - tracker is inpatient-only, so outpatient-managed condition or under-estimate (claims are a lower bound)",
        claims_patients_annual > adjusted_median              ~ "Claims above adjusted median - upper methods favoured",
        claims_patients_annual > Unadjusted                   ~ "Claims between unadjusted and adjusted median - consistent",
        TRUE                                                  ~ "Claims below unadjusted tracker count - partial SHA capture, consistent"),
      util_patients_lower = tidyr::replace_na(util_patients_lower, 0),
      util_check = dplyr::case_when(
        util_patients_lower == 0                 ~ "No utilization records",
        util_patients_lower > adjusted_max       ~ "Utilization exceeds ALL methods - tracker is inpatient-only, so outpatient-managed condition or under-estimate",
        util_patients_lower > adjusted_median    ~ "Utilization above adjusted median - upper methods favoured",
        util_patients_lower > Unadjusted         ~ "Utilization between unadjusted and adjusted median - consistent",
        TRUE                                     ~ "Utilization below unadjusted tracker count - consistent"),
      source_floor = pmax(claims_patients_annual, util_patients_lower),
      methods_at_or_above_floor = rowSums(dplyr::across(dplyr::all_of(c("Unadjusted", have, "Bayesian")), ~ .x >= source_floor), na.rm = TRUE),
      claims_supported_range = dplyr::if_else(claims_patients_annual > 0,
        paste0(round(pmax(claims_patients_annual, adjusted_min)), " - ", round(adjusted_max)), NA_character_))
}

# ICD-11 titles for condition codes (4-char level); falls back to the code
r_names <- function(conds) {
  icd <- tryCatch(readxl::read_excel("icd11_cleaned.xlsx"), error = function(e) NULL)
  if (is.null(icd)) return(stats::setNames(conds, conds))
  icd <- icd |> dplyr::mutate(code = stringr::str_to_lower(Code),
                              title = dplyr::coalesce(Level6, Level5, Level4, Level3, Level2, Level1))
  t <- icd$title[match(conds, icd$code)]
  stats::setNames(dplyr::coalesce(t, conds), conds)
}

message("[10_auto_update_bod.R] update_bod() ready.")
