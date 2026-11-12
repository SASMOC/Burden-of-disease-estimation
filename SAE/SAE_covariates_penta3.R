# =============================================================================
# Penta3 (DPT3) coverage, children 12-23 months, Kenya DHS 2022
# Small-area estimation with SUMMER / surveyPrev -- do area-level covariates help?
#
#   Step 1  Rebuild the survey data from the saved model objects and validate it
#   Step 2  Direct estimates                      directEST()    admin 1, admin 2
#   Step 3  Fay-Herriot (area-level, BYM2)        fhModel()      admin 1, admin 2
#   Step 4  Unit-level (cluster beta-binomial)    clusterModel() admin 1, admin 2
#           each with and without covariates; admin 2 also nested
#           (county IID + sub-county BYM2)
#   Step 5  Compare: covariate effects, WAIC, precision, shrinkage, rankings, maps
#
# Inputs : the 3 DHS-app .rds model objects + 2 covariate templates (this folder),
#          GADM shapefiles in ../kenya_shape_files
# Outputs: outputs_covariates/  (CSV tables, PNG figures, model .rds objects)
# =============================================================================
suppressPackageStartupMessages({
  library(surveyPrev); library(SUMMER); library(INLA)
  library(dplyr); library(tidyr); library(readr); library(ggplot2); library(sf); library(patchwork)
})
set.seed(2022)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/SAE")
out <- "outputs_covariates"; dir.create(out, showWarnings = FALSE)
`%||%` <- function(a, b) if (is.null(a)) b else a
colors_cema <- c("#27aae1", "#fe7501", "#084887", "#efca08", "#497048", "#006d77")
theme_set(theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"),
                                                legend.position = "bottom"))
sv <- function(p, f, w = 11, h = 6.5) ggsave(file.path(out, f), p, width = w, height = h, dpi = 200, bg = "white")
cn <- c("education_yrs", "anc4plus", "access_difficulty", "facility_delivery")
cov_labels <- c(education_yrs = "Mean years of education", anc4plus = "4+ ANC visits",
                access_difficulty = "Difficulty accessing facility", facility_delivery = "Facility delivery")
ctrl <- list(waic = TRUE)

# ---- STEP 1. Survey data + covariates ---------------------------------------
dir1_saved <- readRDS("Kenya_2022_CH_VACC_C_DP3_Direct_Admin_1_model_object.rds")
fh1_saved  <- readRDS("Kenya_2022_CH_VACC_C_DP3_FH_Admin_1_model_object.rds")
un2_saved  <- readRDS("Kenya_2022_CH_VACC_C_DP3_Unit_Admin_2_model_object.rds")
ai1 <- fh1_saved$admin.info; ai2 <- un2_saved$admin.info

# cluster-level data stored inside the unit-level INLA object (300 trailing NA rows are prediction rows)
cl <- as.data.frame(un2_saved$inla$.args$data) |> select(-any_of("geometry")) |> filter(!is.na(value))
stopifnot(nrow(cl) == 1448, sum(cl$value) == 3148, sum(cl$n) == 3679)   # matches the DHS-app report
ind <- cl[rep(seq_len(nrow(cl)), cl$n), c("cluster", "householdID", "v022", "v023", "v024", "weight", "strata")]
ind$value <- unlist(Map(function(v, n) c(rep(1, v), rep(0, n - v)), cl$value, cl$n))
cluster.info <- list(data = cl |> distinct(cluster, LONGNUM, LATNUM, admin1.name, admin2.name, admin2.name.full))

c1 <- read_csv("Kenya_Admin_1_cov_template.csv", show_col_types = FALSE); names(c1) <- c("admin1.name", cn)
c2 <- read_csv("Kenya_Admin_2_cov_template1.csv", show_col_types = FALSE); names(c2) <- c("admin2.name.full", cn)
miss_cov <- c2$admin2.name.full[!complete.cases(c2)]
c2 <- c2 |> mutate(a1 = sub("_.*$", "", admin2.name.full)) |> group_by(a1) |>
  mutate(across(all_of(cn), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))) |> ungroup() |> select(-a1)
zs <- function(x) as.numeric(scale(x))
c1z <- c1 |> mutate(across(all_of(cn), zs)) |> as.data.frame()   # effects are per 1 SD across areas
c2z <- c2 |> mutate(across(all_of(cn), zs)) |> as.data.frame()
sel <- function(X, v) if (length(v)) X[, c(names(X)[1], v), drop = FALSE] else NULL

# ---- STEP 2. Direct estimates -----------------------------------------------
dir1 <- directEST(data = ind, cluster.info = cluster.info, admin = 1, admin.info = ai1)
dir2 <- directEST(data = ind, cluster.info = cluster.info, admin = 2, admin.info = ai2)
cat("Validation: max |direct admin1 - saved DHS-app direct| =",
    signif(max(abs(dir1$res.admin1$direct.est - dir1_saved$res.admin1$direct.est)), 3), "\n")
national <- list(app = 0.8918, dhs_report = 0.892, n_clusters = 1448, n_children = 3679, n_events = 3148,
                 pct_admin2_no_data = 2.67)

# ---- STEP 3/4. Model fitting helpers ----------------------------------------
subsets_all <- unlist(lapply(0:4, function(k) if (k == 0) list(character(0)) else combn(cn, k, simplify = FALSE)), recursive = FALSE)
subsets_few <- c(list(character(0)), as.list(cn), list(cn))            # none / each / all
lab <- function(v) if (length(v)) paste(v, collapse = " + ") else "No covariates"
timeit <- function(expr) { t <- Sys.time(); r <- force(expr); cat(sprintf("   %.0fs\n", as.numeric(difftime(Sys.time(), t, units = "secs")))); r }

fit_family <- function(family, level, vs, nested = FALSE) {
  lapply(vs, function(v) {
    cat(sprintf("%s admin %d%s: %s", family, level, if (nested) " (nested)" else "", lab(v)))
    X <- sel(if (level == 1) c1z else c2z, v)
    timeit(switch(family,
      FH   = fhModel(data = ind, cluster.info = cluster.info, admin.info = if (level == 1) ai1 else ai2,
                     X = X, admin = level, model = "bym2", nested = nested, var.fix = (level == 2), control.compute = ctrl),
      UNIT = clusterModel(data = ind, cluster.info = cluster.info, admin.info = if (level == 1) ai1 else ai2,
                          X = X, admin = level, model = "bym2", nested = nested, stratification = FALSE,
                          CI = 0.95, control.compute = ctrl)))
  })
}
get_res <- function(f, level) as.data.frame(if (level == 1) f$res.admin1 else f$res.admin2)
get_inla <- function(f) if (!is.null(f$inla)) f$inla else f$model$fit
fh_waic <- function(m) {   # SUMMER::smoothSurvey does not request WAIC, so refit the same INLA call with WAIC on
  a <- m$model$fit$.args
  e <- new.env(parent = globalenv()); e$Amat <- m$model$Amat
  e$hyperpc2 <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)), phi = list(prior = "pc", param = c(0.5, 2/3)))
  environment(a$formula) <- e; a$control.compute <- list(waic = TRUE)
  a <- a[!names(a) %in% c("inla.call", ".parent.frame", "keep", "working.directory")]
  tryCatch(do.call(INLA::inla, a)$waic$waic, error = function(e) NA_real_)
}
get_waic <- function(f) { w <- tryCatch(get_inla(f)$waic$waic, error = function(e) NULL); if (is.null(w)) w <- fh_waic(f); w }
mlab <- function(spec, nested) paste0(if (nested) "Nested: " else "", lab(spec))
tab_row <- function(f, level, family, spec, nested = FALSE) {
  r <- get_res(f, level)
  tibble(family = family, level = paste("Admin", level), model = mlab(spec, nested),
         n_cov = length(spec), nested = nested, waic = get_waic(f), mean_sd = mean(r$sd),
         mean_cv = mean(r$cv), mean_width = mean(r$upper - r$lower))
}
coef_rows <- function(f, family, level, spec, nested = FALSE) {
  if (!length(spec)) return(NULL)
  fx <- get_inla(f)$summary.fixed; fx <- fx[rownames(fx) %in% cn, , drop = FALSE]
  tibble(family = family, level = paste("Admin", level), model = mlab(spec, nested), n_cov = length(spec),
         term = rownames(fx), mean = fx$mean, lower = fx$`0.025quant`, upper = fx$`0.975quant`,
         excludes_zero = fx$`0.025quant` > 0 | fx$`0.975quant` < 0)
}
hyper_rows <- function(f, family, level, spec, nested = FALSE) {
  h <- get_inla(f)$summary.hyperpar
  tibble(family = family, level = paste("Admin", level), model = mlab(spec, nested), hyper = rownames(h),
         mean = h$mean, lower = h$`0.025quant`, upper = h$`0.975quant`)
}

# ---- fit everything ---------------------------------------------------------
cache <- "/tmp/w/fits_cache.rds"
if (file.exists(cache)) { fits <- readRDS(cache) } else {
fits <- list()
fits$FH1  <- fit_family("FH", 1, subsets_all);   fits$FH1_specs  <- subsets_all
fits$UN1  <- fit_family("UNIT", 1, subsets_few); fits$UN1_specs  <- subsets_few
fits$FH2  <- fit_family("FH", 2, subsets_few);   fits$FH2_specs  <- subsets_few
fits$UN2  <- fit_family("UNIT", 2, subsets_few); fits$UN2_specs  <- subsets_few
fits$UN2n <- fit_family("UNIT", 2, list(character(0), cn), nested = TRUE); fits$UN2n_specs <- list(character(0), cn)
fits$FH2n <- tryCatch(fit_family("FH", 2, list(character(0), cn), nested = TRUE), error = function(e) { message("FH nested failed: ", conditionMessage(e)); NULL })
fits$FH2n_specs <- list(character(0), cn)
saveRDS(fits, cache) }
saveRDS(lapply(fits, function(x) if (is.list(x) && length(x) && inherits(x[[1]], c("fhModel", "clusterModel"))) NULL else x), file.path(out, "fit_specs.rds"))

build_tab <- function(key, family, level, nested = FALSE) {
  fl <- fits[[key]]; if (is.null(fl)) return(NULL)
  bind_rows(Map(function(f, s) tab_row(f, level, family, s, nested), fl, fits[[paste0(key, "_specs")]]))
}
tabs <- bind_rows(build_tab("FH1", "Fay-Herriot", 1), build_tab("UN1", "Unit-level", 1),
                  build_tab("FH2", "Fay-Herriot", 2), build_tab("UN2", "Unit-level", 2),
                  build_tab("UN2n", "Unit-level", 2, TRUE), build_tab("FH2n", "Fay-Herriot", 2, TRUE)) |>
  group_by(family, level, nested) |> mutate(d_waic = waic - waic[model %in% c("No covariates", "Nested: No covariates")][1]) |> ungroup()
write_csv(tabs, file.path(out, "model_comparison.csv")); print(as.data.frame(tabs), digits = 4)

cbind_ <- function(key, family, level, nested = FALSE) bind_rows(Map(function(f, s) coef_rows(f, family, level, s, nested), fits[[key]], fits[[paste0(key, "_specs")]]))
coefs <- bind_rows(cbind_("FH1", "Fay-Herriot", 1), cbind_("UN1", "Unit-level", 1), cbind_("FH2", "Fay-Herriot", 2),
                   cbind_("UN2", "Unit-level", 2), cbind_("UN2n", "Unit-level", 2, TRUE))
write_csv(coefs, file.path(out, "covariate_coefficients.csv"))
hypers <- bind_rows(
  hyper_rows(fits$FH1[[1]], "Fay-Herriot", 1, character(0)), hyper_rows(fits$FH1[[length(fits$FH1)]], "Fay-Herriot", 1, cn),
  hyper_rows(fits$UN1[[1]], "Unit-level", 1, character(0)), hyper_rows(fits$UN1[[length(fits$UN1)]], "Unit-level", 1, cn),
  hyper_rows(fits$UN2[[1]], "Unit-level", 2, character(0)), hyper_rows(fits$UN2[[length(fits$UN2)]], "Unit-level", 2, cn),
  hyper_rows(fits$UN2n[[1]], "Unit-level", 2, character(0), TRUE), hyper_rows(fits$UN2n[[2]], "Unit-level", 2, cn, TRUE))
write_csv(hypers, file.path(out, "hyperparameters.csv"))

pick <- function(key) {
  fl <- fits[[key]]; sp <- fits[[paste0(key, "_specs")]]; i <- which.min(sapply(fl, get_waic))
  list(fit = fl[[i]], spec = sp[[i]], none = fl[[1]], all = fl[[length(fl)]])
}
B <- list(FH1 = pick("FH1"), UN1 = pick("UN1"), FH2 = pick("FH2"), UN2 = pick("UN2"))
for (k in names(B)) cat(k, "lowest-WAIC covariate set:", lab(B[[k]]$spec), "\n")
# headline comparisons use the full four-covariate model (consistent question: do covariates help?)
H <- list(FH1 = B$FH1$all, UN1 = B$UN1$all, FH2 = B$FH2$all, UN2 = B$UN2$all, UN2n = fits$UN2n[[2]],
          FH1_0 = B$FH1$none, UN1_0 = B$UN1$none, FH2_0 = B$FH2$none, UN2_0 = B$UN2$none, UN2n_0 = fits$UN2n[[1]])

# ---- STEP 5. Assemble estimates ---------------------------------------------
est <- function(f, level, tag) {
  r <- get_res(f, level); key <- if (level == 1) "admin1.name" else "admin2.name.full"
  r |> transmute(!!key := .data[[key]], !!paste0(tag, "_mean") := mean, !!paste0(tag, "_sd") := sd,
                 !!paste0(tag, "_lo") := lower, !!paste0(tag, "_hi") := upper, !!paste0(tag, "_cv") := cv)
}
a1 <- dir1$res.admin1 |> transmute(admin1.name, direct_mean = direct.est, direct_sd = direct.se,
                                   direct_lo = direct.lower, direct_hi = direct.upper, direct_cv = cv)
for (t in c("FH1_0", "FH1", "UN1_0", "UN1")) a1 <- left_join(a1, est(H[[t]], 1, t), by = "admin1.name")
a1 <- left_join(a1, c1, by = "admin1.name")
a2 <- as.data.frame(ai2$data) |> select(admin1.name, admin2.name, admin2.name.full) |>
  left_join(dir2$res.admin2 |> transmute(admin2.name.full, direct_mean = direct.est, direct_sd = direct.se,
                                         direct_lo = direct.lower, direct_hi = direct.upper, direct_cv = cv), by = "admin2.name.full")
for (t in c("FH2_0", "FH2", "UN2_0", "UN2", "UN2n_0", "UN2n")) a2 <- left_join(a2, est(H[[t]], 2, t), by = "admin2.name.full")
a2 <- left_join(a2, c2, by = "admin2.name.full")
smp <- cl |> group_by(admin2.name.full) |> summarise(n_clusters = n_distinct(cluster), n_children = sum(n), .groups = "drop")
a2 <- left_join(a2, smp, by = "admin2.name.full") |> mutate(across(c(n_clusters, n_children), ~ replace_na(.x, 0)))
a2$direct_usable <- !is.na(a2$direct_mean) & is.finite(a2$direct_sd) & a2$direct_sd > 0 & a2$direct_mean < 1
write_csv(a1, file.path(out, "admin1_all_estimates.csv")); write_csv(a2, file.path(out, "admin2_all_estimates.csv"))

cat("Validation FH admin1  :", round(cor(a1$FH1_0_mean, fh1_saved$res.admin1$mean), 5), "\n")
cat("Validation unit admin2:", round(cor(a2$UN2_0_mean, un2_saved$res.admin2$mean), 5), "\n")
cat("Admin-2: no sample =", sum(a2$n_children == 0), "| direct unusable =", sum(!a2$direct_usable), "\n")

assoc <- function(d, lvl) {
  d <- d[d$direct_usable %||% rep(TRUE, nrow(d)), ]
  y <- qlogis(d$direct_mean); w <- rep(1, length(y))
  bind_rows(lapply(cn, function(v) { m <- lm(y ~ d[[v]], weights = w)
    tibble(level = lvl, covariate = v, r = cor(d[[v]], y), R2 = summary(m)$r.squared, p = summary(m)$coefficients[2, 4]) }))
}
a1$direct_usable <- TRUE
assoc_tab <- bind_rows(assoc(a1, "Admin 1"), assoc(a2, "Admin 2")); write_csv(assoc_tab, file.path(out, "covariate_vs_direct.csv"))
cov_cor <- list(`Admin 1` = cor(a1[cn]), `Admin 2` = cor(a2[cn]))

shrink <- function(d, tag, lvl, nm) {
  ok <- !is.na(d$direct_mean) & !is.na(d[[paste0(tag, "_mean")]]); m <- d[[paste0(tag, "_mean")]][ok]; dm <- d$direct_mean[ok]
  tibble(level = lvl, model = nm, n_compared = sum(ok), var_ratio = var(m) / var(dm), slope = unname(coef(lm(m ~ dm))[2]),
         mean_abs_shift = mean(abs(m - dm)), mean_se_ratio = mean((d[[paste0(tag, "_sd")]] / d$direct_sd)[ok & is.finite(d$direct_sd) & d$direct_sd > 0]))
}
shr <- bind_rows(shrink(a1, "FH1_0", "Admin 1", "Fay-Herriot"), shrink(a1, "FH1", "Admin 1", "Fay-Herriot + covariates"),
                 shrink(a1, "UN1_0", "Admin 1", "Unit-level"), shrink(a1, "UN1", "Admin 1", "Unit-level + covariates"),
                 shrink(a2, "FH2_0", "Admin 2", "Fay-Herriot"), shrink(a2, "FH2", "Admin 2", "Fay-Herriot + covariates"),
                 shrink(a2, "UN2_0", "Admin 2", "Unit-level"), shrink(a2, "UN2", "Admin 2", "Unit-level + covariates"),
                 shrink(a2, "UN2n_0", "Admin 2", "Unit-level nested"), shrink(a2, "UN2n", "Admin 2", "Unit-level nested + covariates"))
write_csv(shr, file.path(out, "shrinkage_summary.csv")); print(as.data.frame(shr), digits = 3)

nos <- a2 |> filter(n_children == 0) |> select(admin2.name.full, FH2_0_mean, FH2_mean, UN2_0_mean, UN2_mean, UN2n_mean, UN2_sd, UN2_lo, UN2_hi)
nos$county_mean_unit <- sapply(nos$admin2.name.full, function(n) mean(a2$UN2_0_mean[a2$admin1.name == a2$admin1.name[a2$admin2.name.full == n] & a2$n_children > 0]))
write_csv(nos, file.path(out, "admin2_areas_without_data.csv")); print(as.data.frame(nos), digits = 3)

# ---- Figures ----------------------------------------------------------------
s1 <- st_read("../kenya_shape_files/gadm41_KEN_1.shp", quiet = TRUE)
s2 <- st_read("../kenya_shape_files/gadm41_KEN_2.shp", quiet = TRUE)
pal <- c("#efca08", "#27aae1", "#084887")
mp <- function(res, shp, value, ttl, rng) suppressMessages(suppressWarnings(
  prevMap(res.obj = res, poly.shp = shp, value.to.plot = value, map.title = ttl, legend.label = "Coverage", value.range = rng,
          color.palette = pal, color.reverse = FALSE) + theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 11, hjust = .5))))
rng1 <- range(unlist(a1[paste0(c("direct", "FH1_0", "FH1", "UN1_0", "UN1"), "_mean")]))
rng2 <- range(unlist(a2[paste0(c("direct", "FH2", "UN2_0", "UN2", "UN2n"), "_mean")]), na.rm = TRUE)
sv(mp(dir1, s1, "mean", "Direct", rng1) + mp(H$FH1_0, s1, "mean", "Fay-Herriot", rng1) + mp(H$FH1, s1, "mean", "Fay-Herriot + covariates", rng1) +
     plot_layout(guides = "collect") & theme(legend.position = "bottom"), "fig_admin1_maps.png", 13, 5.5)
sv(mp(H$UN1_0, s1, "mean", "Unit-level", rng1) + mp(H$UN1, s1, "mean", "Unit-level + covariates", rng1) +
     plot_layout(guides = "collect") & theme(legend.position = "bottom"), "fig_admin1_maps_unit.png", 9, 5.5)
sv(mp(dir2, s2, "mean", "Direct", rng2) + mp(H$UN2_0, s2, "mean", "Unit-level", rng2) + mp(H$UN2, s2, "mean", "Unit-level + covariates", rng2) +
     mp(H$UN2n, s2, "mean", "Unit-level nested + covariates", rng2) + plot_layout(nrow = 1, guides = "collect") & theme(legend.position = "bottom"),
   "fig_admin2_maps.png", 16, 5.5)
cvr <- range(a2$UN2_0_cv, a2$UN2_cv, a2$UN2n_cv, na.rm = TRUE)
sv(mp(H$UN2_0, s2, "cv", "Unit-level: CV", cvr) + mp(H$UN2, s2, "cv", "Unit-level + covariates: CV", cvr) +
     mp(H$UN2n, s2, "cv", "Nested + covariates: CV", cvr) + plot_layout(nrow = 1, guides = "collect") & theme(legend.position = "bottom"),
   "fig_admin2_cv_maps.png", 14, 5.5)

mm <- s2 |> mutate(admin2.name.full = paste0(NAME_1, "_", NAME_2)) |> left_join(a2 |> select(admin2.name.full, n_children, direct_usable), by = "admin2.name.full") |>
  mutate(status = case_when(n_children == 0 ~ "No sample", !direct_usable ~ "Sampled, direct unusable (100% or zero variance)", TRUE ~ "Usable direct estimate"))
sv(ggplot(mm) + geom_sf(aes(fill = status), colour = "white", linewidth = .05) +
     scale_fill_manual(values = c("No sample" = colors_cema[2], "Sampled, direct unusable (100% or zero variance)" = colors_cema[4],
                                  "Usable direct estimate" = colors_cema[3]), name = NULL) + theme_void() +
     theme(legend.position = "bottom", legend.direction = "vertical") + labs(title = "Sub-county data availability"), "fig_admin2_missing.png", 6.5, 7)

sc <- function(d, y, lvl, ylab) d |> pivot_longer(all_of(cn), names_to = "cov", values_to = "x") |>
  mutate(cov = factor(cov_labels[cov], levels = cov_labels)) |>
  ggplot(aes(x, .data[[y]])) + geom_point(colour = colors_cema[3], alpha = .6, size = 1.8) +
  geom_smooth(method = "lm", colour = colors_cema[2], fill = colors_cema[2], alpha = .15, formula = y ~ x) +
  facet_wrap(~cov, nrow = 1, scales = "free_x") + scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = ylab, title = paste0(lvl, ": covariates vs ", tolower(ylab)))
sv(sc(a1, "direct_mean", "Counties", "Direct estimate"), "fig_admin1_cov_scatter.png", 13, 4.3)
sv(sc(a2, "UN2_0_mean", "Sub-counties", "Unit-level estimate (no covariates)"), "fig_admin2_cov_scatter.png", 13, 4.3)

cf <- coefs |> filter(n_cov == 1 | n_cov == 4) |>
  mutate(spec = ifelse(n_cov == 1, "One covariate at a time", ifelse(grepl("Nested", model), "All four (nested)", "All four together")),
         term = factor(cov_labels[term], levels = rev(cov_labels)), panel = paste(level, family, sep = ": "))
sv(ggplot(cf, aes(mean, term, colour = spec, shape = spec)) + geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
     geom_pointrange(aes(xmin = lower, xmax = upper), position = position_dodge(.6), size = .4) + facet_wrap(~panel, nrow = 2) +
     scale_colour_manual(values = colors_cema[c(1, 3, 2)], name = NULL) + scale_shape_discrete(name = NULL) +
     labs(x = "Effect on logit Penta3 coverage per 1 SD of covariate (95% credible interval)", y = NULL, title = "Covariate effects"),
   "fig_coefficients.png", 12, 7)

l1 <- a1 |> arrange(direct_mean) |> mutate(admin1.name = factor(admin1.name, levels = admin1.name))
lp <- bind_rows(lapply(list(c("direct", "Direct"), c("FH1_0", "Fay-Herriot"), c("FH1", "Fay-Herriot + covariates"),
                            c("UN1_0", "Unit-level"), c("UN1", "Unit-level + covariates")), function(p)
  tibble(area = l1$admin1.name, m = l1[[paste0(p[1], "_mean")]], lo = l1[[paste0(p[1], "_lo")]], hi = l1[[paste0(p[1], "_hi")]], model = p[2]))) |>
  mutate(model = factor(model, levels = c("Direct", "Fay-Herriot", "Fay-Herriot + covariates", "Unit-level", "Unit-level + covariates")))
sv(ggplot(lp, aes(area, m, colour = model)) + geom_hline(yintercept = national$dhs_report, linetype = 2, colour = "grey40") +
     geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(.8), size = .16, linewidth = .3) +
     scale_colour_manual(values = colors_cema[c(2, 1, 3, 4, 5)], name = NULL) + scale_y_continuous(labels = scales::percent) +
     coord_flip() + labs(x = NULL, y = "Penta3 coverage (95% interval)", title = "Counties: direct vs model-based estimates",
                         subtitle = "Dashed line: national estimate 89.2%") + theme(axis.text.y = element_text(size = 8)),
   "fig_admin1_lineplot.png", 9, 11)

shp <- function(d, ys, labs, ttl, lim) {
  l <- bind_rows(Map(function(y, lb) tibble(direct = d$direct_mean, est = d[[y]], model = lb), ys, labs)) |> filter(!is.na(direct), !is.na(est))
  ggplot(l, aes(direct, est, colour = model)) + geom_abline(linetype = 2, colour = "grey50") + geom_point(alpha = .65, size = 1.8) +
    scale_colour_manual(values = colors_cema, name = NULL) + scale_x_continuous(labels = scales::percent, limits = lim) +
    scale_y_continuous(labels = scales::percent, limits = lim) + coord_equal() + labs(x = "Direct estimate", y = "Model-based estimate", title = ttl)
}
sv(shp(a1, c("FH1_0_mean", "FH1_mean", "UN1_mean"), c("Fay-Herriot", "Fay-Herriot + covariates", "Unit-level + covariates"),
       "Counties: shrinkage", c(.7, 1)), "fig_admin1_shrinkage.png", 7, 6.5)
sv(shp(a2, c("FH2_mean", "UN2_0_mean", "UN2_mean", "UN2n_mean"), c("Fay-Herriot + covariates", "Unit-level", "Unit-level + covariates", "Unit-level nested + covariates"),
       "Sub-counties: shrinkage", c(.5, 1)), "fig_admin2_shrinkage.png", 7.5, 6.5)

pr <- bind_rows(
  tibble(level = "Admin 1", model = "Direct", sd = a1$direct_sd), tibble(level = "Admin 1", model = "Fay-Herriot", sd = a1$FH1_0_sd),
  tibble(level = "Admin 1", model = "Fay-Herriot + covariates", sd = a1$FH1_sd), tibble(level = "Admin 1", model = "Unit-level", sd = a1$UN1_0_sd),
  tibble(level = "Admin 1", model = "Unit-level + covariates", sd = a1$UN1_sd),
  tibble(level = "Admin 2", model = "Direct", sd = a2$direct_sd[a2$direct_usable]), tibble(level = "Admin 2", model = "Fay-Herriot", sd = a2$FH2_0_sd),
  tibble(level = "Admin 2", model = "Fay-Herriot + covariates", sd = a2$FH2_sd), tibble(level = "Admin 2", model = "Unit-level", sd = a2$UN2_0_sd),
  tibble(level = "Admin 2", model = "Unit-level + covariates", sd = a2$UN2_sd), tibble(level = "Admin 2", model = "Unit-level nested + covariates", sd = a2$UN2n_sd)) |>
  mutate(model = factor(model, levels = unique(model)))
sv(ggplot(pr, aes(model, sd, fill = model)) + geom_boxplot(alpha = .85, outlier.alpha = .35) + facet_wrap(~level, scales = "free") +
     scale_fill_manual(values = rep(colors_cema, 2), guide = "none") + scale_y_continuous(labels = scales::percent) + coord_flip() +
     labs(x = NULL, y = "Standard error / posterior SD", title = "Precision of the estimates"), "fig_precision.png", 12, 4.8)

rk <- function(d, tag, nm, ttl, topn, lab_size) {
  x <- d |> transmute(name = .data[[nm]], m = .data[[paste0(tag, "_mean")]], lo = .data[[paste0(tag, "_lo")]], hi = .data[[paste0(tag, "_hi")]]) |>
    arrange(m) |> mutate(rank = row_number(), grp = case_when(hi < national$dhs_report ~ "Interval entirely below national",
                                                              lo > national$dhs_report ~ "Interval entirely above national", TRUE ~ "Overlaps national"))
  ggplot(x, aes(rank, m, colour = grp)) + geom_hline(yintercept = national$dhs_report, linetype = 2, colour = "grey40") +
    geom_pointrange(aes(ymin = lo, ymax = hi), size = .2, linewidth = .3) +
    geom_text(data = bind_rows(head(x, topn), tail(x, topn)), aes(label = name), angle = 90, hjust = 1.1, size = lab_size, colour = "grey20", nudge_y = -.012) +
    scale_colour_manual(values = c("Interval entirely below national" = colors_cema[2], "Interval entirely above national" = colors_cema[3], "Overlaps national" = "grey60"), name = NULL) +
    scale_y_continuous(labels = scales::percent) + labs(x = "Rank (low to high)", y = "Penta3 coverage", title = ttl)
}
sv(rk(a1, "FH1", "admin1.name", "County ranking (Fay-Herriot + covariates)", 6, 3.3), "fig_admin1_ranking.png", 11, 6.5)
sv(rk(a2, "UN2n", "admin2.name.full", "Sub-county ranking (unit-level nested + covariates)", 8, 2.9), "fig_admin2_ranking.png", 12, 6.8)
rank_tab <- bind_rows(
  a1 |> transmute(level = "Admin 1", area = admin1.name, mean = FH1_mean, lo = FH1_lo, hi = FH1_hi),
  a2 |> transmute(level = "Admin 2", area = admin2.name.full, mean = UN2n_mean, lo = UN2n_lo, hi = UN2n_hi)) |>
  group_by(level) |> arrange(mean, .by_group = TRUE) |> mutate(rank = row_number()) |> ungroup()
write_csv(rank_tab, file.path(out, "rankings.csv"))

cc <- bind_rows(lapply(names(cov_cor), function(n) as.data.frame(as.table(cov_cor[[n]])) |> mutate(level = n))) |>
  mutate(Var1 = cov_labels[as.character(Var1)], Var2 = cov_labels[as.character(Var2)])
sv(ggplot(cc, aes(Var1, Var2, fill = Freq)) + geom_tile(colour = "white") + geom_text(aes(label = sprintf("%.2f", Freq)), colour = "white", size = 3.5) +
     facet_wrap(~level) + scale_fill_gradient2(low = colors_cema[2], mid = "grey90", high = colors_cema[3], limits = c(-1, 1), name = "r") +
     scale_x_discrete(labels = function(x) stringr::str_wrap(x, 12)) + scale_y_discrete(labels = function(x) stringr::str_wrap(x, 16)) +
     labs(x = NULL, y = NULL, title = "Correlation between covariates"), "fig_cov_correlation.png", 12, 5)

# ---- Save -------------------------------------------------------------------
saveRDS(dir1, file.path(out, "Kenya_2022_DP3_Direct_Admin_1_object.rds"))
saveRDS(dir2, file.path(out, "Kenya_2022_DP3_Direct_Admin_2_object.rds"))
saveRDS(H$FH1, file.path(out, "Kenya_2022_DP3_FH_Admin_1_covariates_object.rds"))
saveRDS(H$FH2, file.path(out, "Kenya_2022_DP3_FH_Admin_2_covariates_object.rds"))
saveRDS(H$UN1, file.path(out, "Kenya_2022_DP3_Unit_Admin_1_covariates_object.rds"))
saveRDS(H$UN2n, file.path(out, "Kenya_2022_DP3_Unit_Admin_2_nested_covariates_object.rds"))
saveRDS(list(a1 = a1, a2 = a2, tabs = tabs, coefs = coefs, hypers = hypers, assoc = assoc_tab, shrink = shr, national = national,
             miss_cov = miss_cov, cov_cor = cov_cor, nos = nos, best = lapply(B, function(b) b$spec), rank_tab = rank_tab),
        file.path(out, "results.rds"))
cat("Done. Outputs in", out, "\n")
