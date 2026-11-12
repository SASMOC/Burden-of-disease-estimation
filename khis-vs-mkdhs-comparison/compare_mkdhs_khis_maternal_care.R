# =============================================================================
# Mini KDHS 2025-26 vs KHIS: antenatal, delivery and postnatal care indicators
# National and county level
#
# Same approach as compare_mkdhs_khis_penta3_avg12m.R. Run interactively in RStudio.
# =============================================================================

# =============================================================================
# METHODS IMPLEMENTED BY THIS SCRIPT
#
# QUESTION  Do KHIS routine data and the Mini KDHS 2025-26 give the same coverage
#           of maternal-care services, nationally and by county?
#
# INDICATORS (survey indicator  <->  KHIS indicator)
#   1+ ANC      "Received ANC from a skilled provider"  <-> % attending at least 1 ANC visit
#   4+ ANC      "4+ ANC visits"                         <-> % attending at least 4 ANC visits
#   8+ ANC      "8+ ANC visits"                         <-> MOH 711 clients completing 8th ANC contact (coverage)
#   Skilled birth attendance  "Delivered by a skilled provider"  <-> skilled deliveries / live births
#   Facility delivery         "Delivered in a health facility"   <-> deliveries in facilities / live births
#                             (same count: in KHIS skilled deliveries = deliveries in facilities)
#   C-section   "Delivered by C-section"                <-> caesarean sections / live births
#   PNC mother  "Women with postnatal check, first 2 days"    <-> mothers with PNC within 48 hours / total deliveries
#   PNC newborn "Newborns with postnatal check, first 2 days" <-> infants with PNC within 48 hours / live births
#   RATES ARE COMPUTED FROM COUNTS. The denominator for every delivery and PNC rate is the
#   total expected births in the population (estimated live births = estimated deliveries),
#   so it covers all births like the survey does (home births included). The monthly expected
#   births are the denominator KHIS itself uses for the skilled-delivery indicator
#   (skilled deliveries / skilled delivery %).
#   Infant PNC count is not exported by KHIS, only the % (which KHIS calculates as infants with
#   PNC / MOH 711 live births), so the count = % x MOH 711 live births (rounded).
#
# STEP 1  Mini KDHS (section 2): weighted proportion, SE, unweighted n from
#         Appendix B (extracted by extract_mkdhs_tables_B.R).
#         Population: women 15-49 with a live birth in the 2 years before the survey.
#
# STEP 2  KHIS (section 3): the DHIS2 coverage indicators by county and month.
#         Window = the 24 complete months Nov 2023 - Oct 2025, i.e. the 2 years
#         before the survey started (5 Nov 2025). KHIS coverage = simple average of
#         the monthly values (months with no value are skipped).
#         ADJUSTED FOR REPORTING RATE: every monthly value is divided by the MOH 711
#         reporting rate of that county and month, so that facilities that did not
#         report are accounted for. This is the reporting rate of the whole MOH 711
#         summary report (all its summaries / data elements), not of each indicator;
#         ANC, deliveries, caesarean sections and PNC are all MOH 711 data elements.
#         The unadjusted (as reported) KHIS value is kept in the tables for comparison.
#         National = the national KHIS file for the ANC and delivery counts; for infant PNC
#         (count rebuilt from the county data) national = sum of the 47 counties.
#         The KHIS indicator as KHIS calculates it (PNC divided by facility live births / deliveries)
#         is kept in the tables as a separate column.
#
# STEP 3  Compare (section 4): difference = KDHS - KHIS (percentage points);
#         two-proportion z-test (prop.test) for the 95% CI and p-value
#           KDHS : weighted proportion x women interviewed (unweighted n)
#           KHIS : coverage x annual population (pregnant women, or expected births)
#         plus a design-based 95% CI that uses the KDHS standard error.
#
# STEP 4  Classify (section 5): |difference| < 5 pp minimal, 5 to <10 moderate, 10+ high.
#
# STEP 5  Outputs (sections 6-8): tables (csv + xlsx), one map and one difference
#         plot per indicator, and a combined map.
# =============================================================================

# ---- 0. CONFIG --------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(sf)
library(openxlsx)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/khis-vs-mkdhs-comparison")
invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))

khis_county_file   <- "ANC-PNC-deliveries-county-level-data.xls"
khis_national_file <- "pregnancies-anc-pnc-khis-agg-data-national.xls"
infants_file       <- "data-added-anc-pnc-khis aggregate.xls"     # county: PNC infants % and MOH 711 live births
dhs_file           <- "MKDHS_2025-26_appendixB_all_indicators.csv"
shape_file         <- "../kenya_shape_files/gadm41_KEN_1.shp"
out_dir            <- "outputs/maternal_care"
dir.create(file.path(out_dir, "figures"), recursive = TRUE, showWarnings = FALSE)

colors_cema <- c(blue = "#27aae1", orange = "#fe7501", navy = "#084887",
                 yellow = "#efca08", green = "#497048", teal = "#006d77")
# shades of the CEMA orange: light = <5 pp, medium = 5-10 pp, deep = 10+ pp
band_colors <- c("<5%" = "#fdc08a", "5-10%" = colors_cema[["orange"]], "10+%" = "#d03a00")

clean_name <- function(x) gsub("county|city|[^a-z]", "", tolower(x))

# KHIS columns used (short name = column name in the KHIS export)
khis_columns <- c(
  anc1       = "Proportion of pregnant women who attended at least one ANC visit during pregnancy",
  anc4       = "Percentage of pregnant women attending at least 4 ANC visits",
  anc8       = "MOH 711 Rev 2020_No. of clients completed 8th ANC Contact coverage",
  sba_pct    = "Percentage of deliveries conducted by skilled health attendant in the facility Scorecard",
  sba_n      = "Total Skilled Deliveries",
  cs_n       = "MOH 711 Caesarian Sections",
  pnc_mother_n = "MOH 711 Rev 2020_Mothers receiving Postpartum Care within 48 hours",
  pnc_mother = "PNC within 48hours (Mother)",      # KHIS own %: mothers / deliveries in facilities
  pnc_infant = "PNC within 48hours (Infants)",     # KHIS own %: infants / MOH 711 live births
  est_preg   = "Estimated Number of Pregnant Women",
  est_births = "Estimated live births",
  rep_rate   = "MOH 711 Integrated Summary Report: Reproductive & Child Health, Medical & Rehabilitation Services Rev 2020  - Reporting rate")

# Indicators to compare: which survey indicator, which KHIS series, which population
indicators <- tribble(
  ~id,           ~label,                       ~dhs_indicator,                                      ~khis_var,         ~pop_var,       ~own_var,
  "anc1",        "1+ ANC (skilled provider)",  "Received ANC from a skilled provider",              "anc1",            "pop_pregnant", NA,
  "anc4",        "4+ ANC visits",              "4+ ANC visits",                                     "anc4",            "pop_pregnant", NA,
  "anc8",        "8+ ANC visits",              "8+ ANC visits",                                     "anc8",            "pop_pregnant", NA,
  "sba",         "Skilled birth attendance",   "Delivered by a skilled provider (live births)",     "sba_pct",         "pop_births",   NA,
  "facility",    "Facility delivery",          "Delivered in a health facility (live births)",      "sba_pct",         "pop_births",   NA,
  "csection",    "Caesarean section",          "Delivered by C-section (live births)",              "cs_pct",          "pop_births",   NA,
  "pnc_mother",  "PNC for mother, 48 hours",   "Women with postnatal check during first 2 days",    "pnc_mother_rate", "pop_births",   "pnc_mother",
  "pnc_newborn", "PNC for newborn, 48 hours",  "Newborns with postnatal check during first 2 days", "pnc_infant_rate", "pop_births",   "pnc_infant"
)

# ---- 1. Helper: two-proportion z-test ---------------------------------------
# x = number with the service, n = number of women; 1 = KDHS, 2 = KHIS
prop_test_results <- function(x1, n1, x2, n2) {
  t <- suppressWarnings(prop.test(c(x1, x2), c(n1, n2)))   # warning only flags small expected counts
  data.frame(estimate_diff = 100 * unname(t$estimate[1] - t$estimate[2]),
             diff_lower    = 100 * t$conf.int[1],
             diff_upper    = 100 * t$conf.int[2],
             p_value       = t$p.value)
}

# ---- 2. Mini KDHS 2025-26 ---------------------------------------------------
dhs <- read.csv(dhs_file) |>
  filter(Indicator %in% indicators$dhs_indicator, !Geography %in% c("Urban", "Rural")) |>
  transmute(dhs_indicator = Indicator,
            county = ifelse(Geography == "Total", "Kenya (national)", Geography),
            key    = ifelse(Geography == "Total", "total", clean_name(Geography)),
            dhs_cov = 100 * Value, dhs_lower = 100 * Lower, dhs_upper = 100 * Upper,
            dhs_se = 100 * SE, dhs_n = N_unweighted)

# ---- 3. KHIS: average of the 24 monthly values ------------------------------
read_khis <- function(file, level) {
  x <- suppressMessages(read_excel(file)) |>
    rename(month = periodname, unit = organisationunitname, all_of(khis_columns)) |>
    select(month, unit, all_of(names(khis_columns)))
  x$key <- if (level == "national") "total" else clean_name(x$unit)
  x
}

# Infant PNC count: KHIS only exports the % (infants with PNC / MOH 711 live births), so
# count = % x MOH 711 live births. National = sum of the 47 counties.
infants_county <- suppressMessages(read_excel(infants_file)) |>
  transmute(month = periodname, key = clean_name(organisationunitname),
            pnc_infant_n = round(`PNC within 48hours (Infants)` / 100 * `MOH 711 Live birth`))
infants <- bind_rows(infants_county,
                     infants_county |> group_by(month) |>
                       summarise(pnc_infant_n = sum(pnc_infant_n), key = "total"))

khis_monthly <- bind_rows(read_khis(khis_national_file, "national"),
                          read_khis(khis_county_file, "county")) |>
  left_join(infants, by = c("month", "key")) |>
  mutate(# expected births in the month implied by the KHIS skilled-delivery indicator
         # (= expected deliveries = expected live births)
         expected_births_month = sba_n / (sba_pct / 100),
         # rates computed from counts, all over expected births
         cs_pct          = 100 * cs_n / expected_births_month,
         pnc_mother_rate = 100 * pnc_mother_n / expected_births_month,
         pnc_infant_rate = 100 * pnc_infant_n / expected_births_month)

stopifnot(sum(khis_monthly$key == "total") == 24,
          n_distinct(khis_monthly$key) == 48)         # 47 counties + national

# Adjust for the reporting rate: each monthly value is divided by the MOH 711 reporting
# rate of that county-month, i.e. we scale up to the facilities that did not report
# (assumes non-reporting facilities perform like reporting ones).
#   adjusted value = reported value / (reporting rate / 100)
# average coverage of each KHIS series (national and county)
khis_long <- khis_monthly |>
  pivot_longer(c(anc1, anc4, anc8, sba_pct, cs_pct, pnc_mother_rate, pnc_infant_rate, pnc_mother, pnc_infant),
               names_to = "khis_var", values_to = "value") |>
  mutate(value_adjusted = value / (rep_rate / 100)) |>
  group_by(key, khis_var) |>
  summarise(khis_reported = mean(value, na.rm = TRUE),             # as reported, no adjustment
            khis_cov = mean(value_adjusted, na.rm = TRUE),         # adjusted for reporting rate (used below)
            khis_min = min(value_adjusted, na.rm = TRUE), khis_max = max(value_adjusted, na.rm = TRUE),
            mean_reporting_rate = mean(rep_rate), months_used = sum(!is.na(value)), .groups = "drop")

# populations used as n for the KHIS side (annual figures)
pops_long <- khis_monthly |>
  group_by(key) |>
  summarise(pop_pregnant = mean(est_preg), pop_births = mean(est_births)) |>
  pivot_longer(-key, names_to = "pop_var", values_to = "khis_pop")

# ---- 4. Compare KDHS and KHIS -----------------------------------------------
results <- indicators |>
  inner_join(dhs, by = "dhs_indicator") |>
  left_join(khis_long, by = c("khis_var", "key")) |>
  left_join(khis_long |> select(key, own_var = khis_var, khis_own = khis_reported), by = c("own_var", "key")) |>
  left_join(pops_long, by = c("pop_var", "key")) |>
  mutate(khis_cov_capped = pmin(khis_cov, 100),           # a proportion cannot exceed 100%
         dhs_x  = dhs_cov / 100 * dhs_n,
         khis_x = khis_cov_capped / 100 * khis_pop) |>
  rowwise() |>
  mutate(test = list(prop_test_results(dhs_x, dhs_n, khis_x, khis_pop))) |>
  unnest_wider(test) |>
  ungroup() |>
  mutate(# sensitivity: survey standard error (design-based) + binomial SE of KHIS
         khis_se = 100 * sqrt(khis_cov_capped / 100 * (1 - khis_cov_capped / 100) / khis_pop),
         diff_se_design = sqrt(dhs_se^2 + khis_se^2),
         diff_lower_design = estimate_diff - 1.96 * diff_se_design,
         diff_upper_design = estimate_diff + 1.96 * diff_se_design,
         p_value_design = 2 * pnorm(-abs(estimate_diff / diff_se_design)))

# ---- 5. Classify differences ------------------------------------------------
results <- results |>
  mutate(band = cut(abs(estimate_diff), breaks = c(-Inf, 5, 10, Inf), right = FALSE,
                    labels = c("<5%", "5-10%", "10+%")),
         label = factor(label, levels = indicators$label))

national <- filter(results, key == "total")
counties <- filter(results, key != "total")

# ---- 6. Tables --------------------------------------------------------------
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

# Table 1: national comparison, all indicators
table1 <- national |>
  arrange(label) |>
  transmute(Indicator = as.character(label),
            `KDHS 2025-26 (%)` = round(dhs_cov, 1), `KDHS 95% CI` = sprintf("%.1f-%.1f", dhs_lower, dhs_upper),
            `KHIS as reported (%)` = round(khis_reported, 1),
            `KHIS reporting rate (%)` = round(mean_reporting_rate, 1),
            `KHIS adjusted for reporting rate (%)` = round(khis_cov, 1),
            `KHIS own indicator, facility denominator (%)` = round(khis_own, 1),
            `Difference % (95% CI)` = sprintf("%.1f (%.1f, %.1f)", estimate_diff, diff_lower, diff_upper),
            `P-value` = fmt_p(p_value), Band = as.character(band))

# Table 2: number of counties in each band, by indicator
table2 <- counties |>
  count(label, band, .drop = FALSE) |>
  group_by(label) |>
  mutate(txt = sprintf("%d (%.1f%%)", n, 100 * n / sum(n))) |>
  ungroup() |>
  select(label, band, txt) |>
  pivot_wider(names_from = label, values_from = txt) |>
  rename(`Absolute difference` = band)

# Supplementary table: every county and indicator, difference with 95% CI
supp_table <- counties |>
  arrange(label, county) |>
  transmute(Indicator = as.character(label), County = county,
            `KDHS (%)` = round(dhs_cov, 1), `KDHS 95% CI` = sprintf("%.1f-%.1f", dhs_lower, dhs_upper),
            `KDHS n` = dhs_n, `KHIS as reported (%)` = round(khis_reported, 1),
            `KHIS reporting rate (%)` = round(mean_reporting_rate, 1),
            `KHIS adjusted (%)` = round(khis_cov, 1),
            `KHIS own indicator, facility denominator (%)` = round(khis_own, 1),
            `Difference (KDHS-KHIS adjusted), pp` = round(estimate_diff, 1),
            `95% CI of difference` = sprintf("%.1f, %.1f", diff_lower, diff_upper),
            `P-value` = fmt_p(p_value),
            `95% CI (design-based)` = sprintf("%.1f, %.1f", diff_lower_design, diff_upper_design),
            `P-value (design-based)` = fmt_p(p_value_design),
            Band = as.character(band), `KHIS months used` = months_used)

# numeric table, national + counties, all indicators
all_results <- results |>
  arrange(label, key != "total", county) |>
  transmute(Indicator = as.character(label), Geography = county,
            KDHS_pct = round(dhs_cov, 2), KDHS_lower = round(dhs_lower, 2), KDHS_upper = round(dhs_upper, 2),
            KDHS_n = dhs_n, KHIS_reported_pct = round(khis_reported, 2),
            KHIS_reporting_rate_pct = round(mean_reporting_rate, 2), KHIS_adjusted_pct = round(khis_cov, 2),
            KHIS_own_indicator_facility_denominator_pct = round(khis_own, 2),
            Difference_pp = round(estimate_diff, 2),
            Diff_lower = round(diff_lower, 2), Diff_upper = round(diff_upper, 2), P_value = signif(p_value, 3),
            Diff_lower_design = round(diff_lower_design, 2), Diff_upper_design = round(diff_upper_design, 2),
            P_value_design = signif(p_value_design, 3), Band = as.character(band))

# county x indicator matrix of differences
diff_wide <- counties |>
  transmute(County = county, label, diff = round(estimate_diff, 1)) |>
  pivot_wider(names_from = label, values_from = diff) |>
  arrange(County)

write.csv(table1, file.path(out_dir, "Table1_national.csv"), row.names = FALSE)
write.csv(table2, file.path(out_dir, "Table2_county_categories.csv"), row.names = FALSE)
write.csv(supp_table, file.path(out_dir, "SuppTable_county_diff.csv"), row.names = FALSE)
write.csv(all_results, file.path(out_dir, "maternal_care_difference_with_95CI_all_geographies.csv"), row.names = FALSE)

write.xlsx(list(Table1_national = table1, Table2_county_categories = table2,
                SuppTable_county_diff = supp_table, Diff_with_95CI_numeric = all_results,
                County_diff_matrix = diff_wide),
           file.path(out_dir, "MKDHS_vs_KHIS_maternal_care.xlsx"))

# ---- 7. Maps: one per indicator, plus a combined map ------------------------
counties_sf <- st_read(shape_file, quiet = TRUE) |> mutate(key = clean_name(NAME_1))

map_plot <- function(data, title = NULL) {
  ggplot(data) +
    geom_sf(aes(fill = band), colour = "white", linewidth = 0.2) +
    scale_fill_manual(values = band_colors, name = "Absolute difference\n(KDHS vs KHIS)", drop = FALSE) +
    theme_void(base_size = 12) + labs(title = title)
}

for (i in seq_len(nrow(indicators))) {
  d <- left_join(counties_sf, filter(counties, id == indicators$id[i]) |> select(key, band), by = "key")
  p <- map_plot(d, paste0(indicators$label[i], ": Mini KDHS 2025-26 vs KHIS (Nov 2023 - Oct 2025)"))
  ggsave(file.path(out_dir, "figures", paste0("Map_", indicators$id[i], ".png")), p,
         width = 8.5, height = 7, dpi = 300, bg = "white")
}

all_maps <- counties_sf |> select(key) |>
  inner_join(select(counties, key, label, band), by = "key", relationship = "many-to-many")
fig_all <- map_plot(all_maps, "Absolute difference between Mini KDHS 2025-26 and KHIS (mean Nov 2023 - Oct 2025)") +
  facet_wrap(~label, ncol = 4)
ggsave(file.path(out_dir, "figures", "Map_all_indicators.png"), fig_all, width = 16, height = 9, dpi = 300, bg = "white")

# ---- 8. Difference and 95% CI plots: national and county --------------------
for (i in seq_len(nrow(indicators))) {
  d <- results |>
    filter(id == indicators$id[i]) |>
    mutate(county = ifelse(key == "total", "KENYA (NATIONAL)", county), is_national = key == "total",
           county = reorder(county, ifelse(is_national, -Inf, estimate_diff)))
  p <- ggplot(d, aes(x = estimate_diff, y = county, colour = band)) +
    geom_vline(xintercept = 0, colour = "grey50") +
    geom_errorbar(aes(xmin = diff_lower, xmax = diff_upper), width = 0, alpha = 0.6, orientation = "y") +
    geom_point(aes(size = is_national)) +
    scale_size_manual(values = c(`FALSE` = 1.8, `TRUE` = 3.5), guide = "none") +
    scale_colour_manual(values = band_colors, name = "Absolute difference", drop = FALSE) +
    labs(x = "Difference in coverage, KDHS minus KHIS (percentage points, 95% CI)", y = NULL,
         title = paste0(indicators$label[i], " by county: Mini KDHS 2025-26 vs KHIS")) +
    theme_minimal(base_size = 10) + theme(legend.position = "bottom")
  ggsave(file.path(out_dir, "figures", paste0("Difference_", indicators$id[i], ".png")), p,
         width = 8, height = 10, dpi = 300, bg = "white")
}

# ---- 9. Monthly trends, Nov 2023 - Oct 2025 ---------------------------------
# All trend plots use a fixed 0-100% axis. KHIS values are adjusted for the reporting
# rate; the few monthly values above 100% (some counties) run off the top of the plot.
trend_data <- khis_monthly |>
  pivot_longer(c(anc1, anc4, anc8, sba_pct, cs_pct, pnc_mother_rate, pnc_infant_rate, pnc_mother, pnc_infant),
               names_to = "khis_var", values_to = "reported") |>
  mutate(adjusted = reported / (rep_rate / 100),
         date = as.Date(paste("1", month), "%d %B %Y")) |>
  inner_join(indicators, by = "khis_var", relationship = "many-to-many") |>   # sba_pct feeds 2 indicators
  inner_join(select(results, id, key, county, dhs_cov, dhs_lower, dhs_upper), by = c("id", "key")) |>
  mutate(label = factor(label, levels = indicators$label))

write.csv(select(trend_data, id, label, geography = county, date, reported, reporting_rate = rep_rate, adjusted,
                 kdhs_pct = dhs_cov, kdhs_lower = dhs_lower, kdhs_upper = dhs_upper),
          file.path(out_dir, "khis_monthly_trend_data.csv"), row.names = FALSE)

# ---- 9a. National: all indicators in ONE plot --------------------------------
# Lines = KHIS monthly coverage (adjusted for reporting rate).
# Points with bars at the right = Mini KDHS 2025-26 estimate and its 95% CI.
# Skilled birth attendance and facility delivery share one KHIS line (see methods).
series_labels <- c(anc1 = "1+ ANC", anc4 = "4+ ANC", anc8 = "8+ ANC",
                   sba_pct = "Skilled birth attendance / facility delivery",
                   cs_pct = "Caesarean section", pnc_mother_rate = "PNC mother, 48 hours",
                   pnc_infant_rate = "PNC newborn, 48 hours")
series_colors <- setNames(c(colors_cema[["blue"]], colors_cema[["navy"]], colors_cema[["teal"]],
                            colors_cema[["orange"]], colors_cema[["yellow"]],
                            colors_cema[["green"]], colors_cema[["green"]]), series_labels)
series_lines  <- setNames(c("solid", "solid", "solid", "solid", "solid", "solid", "dashed"), series_labels)

national_lines <- trend_data |>
  filter(key == "total", id != "facility") |>                      # facility delivery = same KHIS line as sba
  mutate(series = factor(series_labels[khis_var], levels = series_labels))

last_month <- max(national_lines$date)
national_survey <- results |>
  filter(key == "total") |>
  mutate(series = factor(series_labels[khis_var], levels = series_labels),
         x = last_month + 45 + 12 * (row_number() - 1),            # spread the survey points to the right
         is_facility = id == "facility")

fig_trend_national <- ggplot() +
  geom_line(data = national_lines, aes(x = date, y = adjusted, colour = series, linetype = series), linewidth = 0.9) +
  geom_errorbar(data = national_survey, aes(x = x, ymin = dhs_lower, ymax = dhs_upper, colour = series),
                width = 8, linewidth = 0.6, show.legend = FALSE) +
  geom_point(data = national_survey, aes(x = x, y = dhs_cov, colour = series, shape = is_facility),
             size = 2.6, show.legend = FALSE) +
  scale_colour_manual(name = NULL, values = series_colors) +
  scale_linetype_manual(name = NULL, values = series_lines) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = NULL, y = "Coverage (%)",
       title = "Monthly KHIS coverage vs Mini KDHS 2025-26, Kenya (Nov 2023 - Oct 2025)",
       subtitle = "Lines = KHIS monthly, adjusted for reporting rate. Points with bars at the right = Mini KDHS 2025-26\nestimate and 95% CI (triangle = facility delivery)") +
  theme_minimal(base_size = 18) +   # large text so the plot is easy to read
  theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(size = 14), legend.text = element_text(size = 16)) +
  guides(colour = guide_legend(), linetype = guide_legend())

ggsave(file.path(out_dir, "figures", "Trend_national_all_indicators.png"), fig_trend_national,
       width = 17, height = 9, dpi = 300, bg = "white")

# ---- 9b. County: one figure per indicator, one panel per county --------------
# KHIS as reported (dashed) and adjusted (solid) against the Mini KDHS estimate (line) and 95% CI (band)
trend_plot <- function(data) {
  ggplot(data, aes(x = date)) +
    geom_ribbon(aes(ymin = dhs_lower, ymax = dhs_upper, group = 1),
                fill = colors_cema[["navy"]], alpha = 0.15) +
    geom_hline(aes(yintercept = dhs_cov, colour = "Mini KDHS 2025-26"), linewidth = 0.7) +
    geom_line(aes(y = reported, colour = "KHIS as reported"), linetype = "dashed", linewidth = 0.6) +
    geom_line(aes(y = adjusted, colour = "KHIS adjusted for reporting rate"), linewidth = 0.8) +
    scale_colour_manual(name = NULL, values = c("Mini KDHS 2025-26" = colors_cema[["navy"]],
                                                "KHIS as reported" = colors_cema[["blue"]],
                                                "KHIS adjusted for reporting rate" = colors_cema[["orange"]])) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = c(0, 50, 100)) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(x = NULL, y = "Coverage (%)") +
    theme_minimal(base_size = 16) +   # large text so the panels are easy to read
    theme(legend.position = "bottom", plot.subtitle = element_text(size = 14))
}

for (i in seq_len(nrow(indicators))) {
  d <- filter(trend_data, key != "total", id == indicators$id[i])
  p <- trend_plot(d) +
    facet_wrap(~county, ncol = 7) +
    labs(title = paste0(indicators$label[i], ": monthly KHIS coverage vs Mini KDHS 2025-26 by county"),
         subtitle = "Shaded band = Mini KDHS 95% CI")
  ggsave(file.path(out_dir, "figures", paste0("Trend_county_", indicators$id[i], ".png")), p,
         width = 28, height = 22, dpi = 200, bg = "white")
}

# ---- 10. Checks -------------------------------------------------------------
print(as.data.frame(table1))
print(as.data.frame(table2))
cat("Counties compared per indicator:\n"); print(table(counties$label))
cat("Counties with KHIS mean above 100%:\n"); print(tapply(counties$khis_cov > 100, counties$label, sum))
