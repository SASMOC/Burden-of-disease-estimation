# Penta3 coverage (12-23 and 24-35 months), KMDHS 2025-26
# Direct vs Area-level (Fay-Herriot BYM2) vs Unit-level (BYM2) models  [Wakefield group / SUMMER]
# Outputs: mean, CV, 95% CrI, exceedance probability, national mean, maps, scatter, ridge, ranking, interval plots
library(haven); library(dplyr); library(tidyr); library(sf); library(SUMMER); library(readr)
library(ggplot2); library(patchwork); library(ggridges); library(survey)
sf_use_s2(FALSE); options(survey.lonely.psu = "adjust")
set.seed(2026)

THRESH <- 0.90   # exceedance threshold: P(coverage > 90%), the national/IA2030 Penta3 target
NSIM   <- 1000
root <- "~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/"
out  <- paste0(root, "khis-vs-mkdhs-comparison/outputs/penta3_wakefield/"); dir.create(out, recursive = TRUE, showWarnings = FALSE)

# ---- CEMA palette (project rule: always colors_cema) ----
colors_cema <- c(blue = "#27aae1", orange = "#fe7501", navy = "#084887", yellow = "#efca08", green = "#497048", teal = "#006d77")
model_cols  <- c(Direct = unname(colors_cema["blue"]), `Area-level` = unname(colors_cema["orange"]), `Unit-level` = unname(colors_cema["navy"]))
MODELS <- names(model_cols)
theme_cema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "#e6e6e6"),
        plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold"),
        legend.position = "bottom")
theme_map <- theme_void(base_size = 11) + theme(plot.title = element_text(face = "bold", hjust = .5), legend.position = "bottom",
                                                legend.key.width = unit(1.4, "cm"))

# ---- Data ----
kr <- read_sav(paste0(root, "khis-vs-mkdhs-comparison/KE_2025_INTERIMDHS_11102026_131_216785/KEBR91SV/KEBR91FL.sav"))
norm <- function(x) gsub("[^a-z]", "", tolower(x))
d <- kr %>% filter(!is.na(H53), B19 >= 12, B19 <= 35) %>%
  transmute(cluster = as.numeric(V001), strata = as.numeric(V022), weights = V005/1e6,
            age_grp = ifelse(B19 <= 23, "12-23 months", "24-35 months"),
            key = norm(as.character(as_factor(V024))), value = as.integer(H53 %in% 1:3))
geo <- st_read(paste0(root, "kenya_shape_files/gadm41_KEN_1.shp"), quiet = TRUE) %>% st_make_valid()
geo$key <- norm(geo$NAME_1); geo <- geo[match(sort(unique(geo$key)), geo$key), ]
stopifnot(nrow(geo) == 47, all(unique(d$key) %in% geo$key))
Amat <- getAmat(as(geo, "Spatial"), names = geo$key)

# ---- Survey-weighted unit-level model (child-level Bernoulli, BYM2 area effect + cluster effect) ----
# Weights: pseudo-likelihood with weights rescaled to sum to the Kish effective sample size, so the
# posterior reflects the design effect. `area` = index of each child's area in `Amat` (county, sub-county or ward);
# areas with no sampled children are predicted through the spatial (BYM2) effect.
library(INLA)
weighted_unit <- function(dd, area, Amat, nsim = NSIM) {
  n_area <- nrow(Amat); w <- dd$weights
  neff <- sum(w)^2 / sum(w^2)
  df <- data.frame(y = dd$value, area = area, cl = as.integer(factor(dd$cluster)), w = w * neff / sum(w))
  hyper <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)), phi = list(prior = "pc", param = c(0.5, 2/3)))
  fit <- inla(y ~ 1 + f(area, model = "bym2", graph = Amat, scale.model = TRUE, constr = TRUE, hyper = hyper) +
                f(cl, model = "iid", hyper = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))),
              family = "binomial", Ntrials = 1, data = df, weights = df$w,
              control.compute = list(config = TRUE), control.predictor = list(compute = FALSE))
  s <- inla.posterior.sample(nsim, fit, selection = list(`(Intercept)` = 1, area = 1:n_area), num.threads = "1:1")
  plogis(sapply(s, function(z) { l <- z$latent[, 1]; l[grep("^\\(Intercept\\)", names(l))] + l[grep("^area:", names(l))][1:n_area] }))
}

# ---- Fit three approaches, return 47 x NSIM draws on probability scale ----
fit_age <- function(a) {
  dd <- as.data.frame(d %>% filter(age_grp == a) %>% mutate(region = key))
  f <- function(...) smoothSurvey(data = dd, Amat = Amat, response.type = "binary", responseVar = "value",
        strataVar = "strata", weightVar = "weights", regionVar = "region", CI = 0.95,
        save.draws = TRUE, nsim = NSIM, ...)
  ar <- f(clusterVar = "~cluster")
  dr <- ar$direct[match(geo$key, ar$direct$region), ]
  ok <- is.finite(dr$direct.logit.est) & is.finite(dr$direct.logit.var) & dr$direct.logit.var > 0
  dir_draws <- matrix(NA_real_, 47, NSIM)
  for (i in which(ok)) dir_draws[i, ] <- plogis(rnorm(NSIM, dr$direct.logit.est[i], sqrt(dr$direct.logit.var[i])))
  getd <- function(m) { x <- m$draws.est; x <- x[match(geo$key, x$region), grepl("^sample", names(x))]; plogis(as.matrix(x)) }
  wN <- as.numeric(tapply(dd$weights, factor(dd$region, geo$key), sum)); wN[is.na(wN)] <- 0
  # national direct estimate from the survey design
  des <- svydesign(ids = ~cluster, strata = ~strata, weights = ~weights, data = dd, nest = TRUE)
  nat <- svymean(~value, des)
  # weighted unit-level model (INLA BYM2 + cluster effect, survey weights)
  unit_draws <- weighted_unit(dd, area = match(dd$region, geo$key), Amat = Amat)
  message(sprintf("%s weighted unit-level done", a))
  list(direct_se = dr$direct.est*(1-dr$direct.est)*sqrt(dr$direct.logit.var), draws = list(Direct = dir_draws, `Area-level` = getd(ar), `Unit-level` = unit_draws),
       direct_est = dr$direct.est, wN = wN, n = as.integer(table(factor(dd$region, geo$key))),
       nat_direct = c(est = as.numeric(nat), se = as.numeric(SE(nat))))
}
rds <- paste0(out, "penta3_posterior_draws.rds")
if (file.exists(rds)) fits <- readRDS(rds) else {
  fits <- setNames(lapply(c("12-23 months", "24-35 months"), fit_age), c("12-23 months", "24-35 months"))
  saveRDS(fits, rds)
}

# ---- Statistics: mean, CV, 95% CrI, exceedance probability ----
county_stats <- bind_rows(lapply(names(fits), function(a) bind_rows(lapply(MODELS, function(m) {
  P <- fits[[a]]$draws[[m]]
  tibble(age_grp = a, model = m, key = geo$key, county = geo$NAME_1, n_children = fits[[a]]$n,
         mean = if (m == "Direct") fits[[a]]$direct_est else rowMeans(P),
         sd = if (m == "Direct") fits[[a]]$direct_se else apply(P, 1, function(z) if (all(is.na(z))) NA_real_ else sd(z)),  # SE: design-based for Direct, posterior SD for models
         lower = apply(P, 1, function(z) if (all(is.na(z))) NA_real_ else quantile(z, .025)),
         upper = apply(P, 1, function(z) if (all(is.na(z))) NA_real_ else quantile(z, .975)),
         exceed = rowMeans(P > THRESH)) %>%
    mutate(cv = 100 * sd / mean, mean = 100 * mean, lower = 100 * lower, upper = 100 * upper, sd = 100 * sd)
})))) %>% mutate(model = factor(model, MODELS))
# Direct estimates of exactly 0/100% have no usable variance -> NA (shown as grey on maps)
write_csv(county_stats %>% mutate(across(where(is.numeric), ~ round(., 3))), paste0(out, "penta3_county_statistics.csv"))

national <- bind_rows(lapply(names(fits), function(a) {
  f <- fits[[a]]; w <- f$wN
  bind_rows(
    tibble(age_grp = a, model = "Direct", mean = 100*f$nat_direct["est"], cv = 100*f$nat_direct["se"]/f$nat_direct["est"],
           lower = 100*(f$nat_direct["est"] - 1.96*f$nat_direct["se"]), upper = 100*(f$nat_direct["est"] + 1.96*f$nat_direct["se"]),
           exceed = 1 - pnorm((THRESH - f$nat_direct["est"]) / f$nat_direct["se"])),
    bind_rows(lapply(c("Area-level", "Unit-level"), function(m) {
      nd <- colSums(f$draws[[m]] * w) / sum(w)   # population(weight)-weighted national draw
      tibble(age_grp = a, model = m, mean = 100*mean(nd), cv = 100*sd(nd)/mean(nd),
             lower = 100*quantile(nd, .025), upper = 100*quantile(nd, .975), exceed = mean(nd > THRESH))
    })))
})) %>% mutate(across(where(is.numeric), ~ round(., 3)))
write_csv(national, paste0(out, "penta3_national_statistics.csv")); print(as.data.frame(national))

# ---- Helpers ----
mapdat <- function(a, m = NULL) { x <- county_stats %>% filter(age_grp == a); if (!is.null(m)) x <- x %>% filter(model == m); geo %>% select(key) %>% inner_join(x, by = "key") }
seq_blue   <- scale_fill_gradient(low = "#e3f4fb", high = colors_cema[["navy"]], na.value = "#bdbdbd")
stat_info <- list(mean = list(lab = "Mean coverage (%)", lo = "#e3f4fb", hi = colors_cema[["navy"]]),
                  cv = list(lab = "CV (%)", lo = "#fff0e0", hi = colors_cema[["orange"]]),
                  lower = list(lab = "95% CrI lower (%)", lo = "#e3f4fb", hi = colors_cema[["navy"]]),
                  upper = list(lab = "95% CrI upper (%)", lo = "#e3f4fb", hi = colors_cema[["navy"]]),
                  exceed = list(lab = paste0("P(coverage > ", THRESH*100, "%)"), lo = "#e0f0f1", hi = colors_cema[["teal"]]))
one_map <- function(dat, stat, title, lim = NULL) {
  si <- stat_info[[stat]]
  ggplot(dat) + geom_sf(aes(fill = .data[[stat]]), colour = "white", linewidth = .15) +
    scale_fill_gradient(low = si$lo, high = si$hi, na.value = "#bdbdbd", limits = lim, name = si$lab) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = .5)) +
    labs(title = title) + theme_map
}

# ---- 1. Maps: single model, all statistics ----
for (a in names(fits)) for (m in MODELS) {
  dat <- mapdat(a, m)
  ps <- lapply(c("mean", "cv", "lower", "upper", "exceed"), function(s) one_map(dat, s, stat_info[[s]]$lab))
  g <- wrap_plots(ps, ncol = 3) + plot_annotation(title = paste0("Penta3 coverage, ", a, ": ", m, " model"),
        caption = "Grey = no usable estimate (direct estimate of 0 or 100%).")
  ggsave(sprintf("%smap_model_%s_%s.png", out, gsub(" ", "", tolower(m)), gsub(" months", "", a)), g, width = 15, height = 10, dpi = 180, bg = "white")
}
# ---- 2. Maps: same statistic compared across models ----
for (a in names(fits)) for (s in c("mean", "cv", "exceed", "lower", "upper")) {
  lim <- range(county_stats[[s]][county_stats$age_grp == a], na.rm = TRUE)
  ps <- lapply(MODELS, function(m) one_map(mapdat(a, m), s, m, lim))
  g <- wrap_plots(ps, nrow = 1, guides = "collect") & theme(legend.position = "bottom")
  g <- g + plot_annotation(title = paste0("Penta3 ", stat_info[[s]]$lab, ", ", a, ": models compared"))
  ggsave(sprintf("%smap_across_models_%s_%s.png", out, s, gsub(" months", "", a)), g, width = 15, height = 6, dpi = 180, bg = "white")
}

# ---- 3. Scatter plots: model vs model (mean coverage) ----
wide <- county_stats %>% select(age_grp, county, model, mean) %>% pivot_wider(names_from = model, values_from = mean)
pairs <- bind_rows(wide %>% transmute(age_grp, county, pair = "Area-level vs Direct", x = Direct, y = `Area-level`),
                   wide %>% transmute(age_grp, county, pair = "Unit-level vs Direct", x = Direct, y = `Unit-level`),
                   wide %>% transmute(age_grp, county, pair = "Unit-level vs Area-level", x = `Area-level`, y = `Unit-level`)) %>%
  mutate(label = ifelse(abs(y - x) > 8, county, NA))
g <- ggplot(pairs, aes(x, y)) + geom_abline(colour = "grey50", linetype = 2) +
  geom_point(colour = colors_cema[["blue"]], size = 2, alpha = .85) +
  geom_text(aes(label = label), size = 2.8, hjust = -.1, vjust = .4, colour = "grey30", check_overlap = TRUE, na.rm = TRUE) +
  facet_grid(age_grp ~ pair) + coord_equal(xlim = c(55, 101), ylim = c(55, 101)) +
  labs(title = "Penta3 coverage by county: model agreement", x = "Estimate, reference model (%)", y = "Estimate, compared model (%)",
       caption = "Dashed line = perfect agreement. Labelled: counties differing by >8 points.") + theme_cema
ggsave(paste0(out, "scatter_model_agreement.png"), g, width = 13, height = 8.5, dpi = 180, bg = "white")

# ---- 4. Ridge plots: posterior distribution per county ----
for (a in names(fits)) {
  ord <- county_stats %>% filter(age_grp == a, model == "Area-level") %>% arrange(mean) %>% pull(county)
  long <- bind_rows(lapply(MODELS, function(m) {
    P <- fits[[a]]$draws[[m]]; rownames(P) <- geo$NAME_1
    as.data.frame(as.table(P)) %>% transmute(county = Var1, model = m, value = 100 * Freq)
  })) %>% filter(!is.na(value)) %>% mutate(county = factor(county, ord), model = factor(model, MODELS))
  g <- ggplot(long, aes(value, county, fill = model)) +
    geom_density_ridges(scale = 2.2, rel_min_height = .01, colour = "white", linewidth = .2, alpha = .9) +
    geom_vline(xintercept = THRESH*100, linetype = 2, colour = "grey40") +
    scale_fill_manual(values = model_cols, guide = "none") + facet_wrap(~ model, nrow = 1) +
    scale_x_continuous(limits = c(40, 100)) +
    labs(title = paste0("Posterior distribution of Penta3 coverage by county, ", a), x = "Coverage (%)", y = NULL,
         caption = "Counties ordered by area-level mean. Dashed line = 90% target. Direct panel omits counties with no usable variance.") + theme_cema
  ggsave(sprintf("%sridge_%s.png", out, gsub(" months", "", a)), g, width = 14, height = 13, dpi = 160, bg = "white")
}

# ---- 5. Ranking plots (caterpillar), per model ----
for (a in names(fits)) {
  nat <- national %>% filter(age_grp == a)
  r <- county_stats %>% filter(age_grp == a, !is.na(mean)) %>% group_by(model) %>% arrange(mean, .by_group = TRUE) %>%
    mutate(rank = row_number(desc(mean)), ykey = factor(paste(county, model, sep = "___"), levels = unique(paste(county, model, sep = "___")))) %>% ungroup()
  g <- ggplot(r, aes(mean, ykey, colour = model)) +
    geom_vline(xintercept = THRESH*100, linetype = 2, colour = "grey40") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, linewidth = .5) + geom_point(size = 1.8) +
    scale_y_discrete(labels = function(x) sub("___.*", "", x)) + scale_colour_manual(values = model_cols, guide = "none") +
    facet_wrap(~ model, scales = "free_y", nrow = 1) +
    labs(title = paste0("County ranking by Penta3 coverage, ", a), x = "Mean coverage with 95% interval (%)", y = NULL,
         caption = "Ordered by each model's mean (highest at top). Dashed line = 90% target.") + theme_cema + theme(axis.text.y = element_text(size = 7))
  ggsave(sprintf("%sranking_%s.png", out, gsub(" months", "", a)), g, width = 15, height = 12, dpi = 160, bg = "white")
}

# ---- 6. Interval comparison across models ----
for (a in names(fits)) {
  ord <- county_stats %>% filter(age_grp == a, model == "Area-level") %>% arrange(mean) %>% pull(county)
  x <- county_stats %>% filter(age_grp == a, !is.na(mean)) %>% mutate(county = factor(county, ord))
  g <- ggplot(x, aes(mean, county, colour = model)) +
    geom_vline(xintercept = THRESH*100, linetype = 2, colour = "grey40") +
    geom_pointrange(aes(xmin = lower, xmax = upper), position = position_dodge(width = .75), size = .25, linewidth = .45) +
    scale_colour_manual(values = model_cols, name = NULL) + scale_x_continuous(limits = c(25, 100)) +
    labs(title = paste0("95% intervals by county and model, ", a), x = "Penta3 coverage (%)", y = NULL,
         caption = "Counties ordered by area-level mean. Dashed line = 90% target.") + theme_cema + theme(axis.text.y = element_text(size = 7))
  ggsave(sprintf("%sinterval_comparison_%s.png", out, gsub(" months", "", a)), g, width = 9, height = 14, dpi = 160, bg = "white")
}
cat("Done. Outputs in", out, "\n")
