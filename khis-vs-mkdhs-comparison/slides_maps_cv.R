# Slide inputs for the KDHS vs KHIS deck: national CV and 95% CI table, county CV summary,
# difference maps and KDHS coefficient-of-variation maps. Run from the khis-vs-mkdhs-comparison folder.
library(openxlsx); library(dplyr); library(ggplot2); library(sf); library(patchwork); sf_use_s2(FALSE)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/khis-vs-mkdhs-comparison")
colors_cema <- c(blue = "#27aae1", orange = "#fe7501", navy = "#084887", yellow = "#efca08", green = "#497048", teal = "#006d77")
f <- "outputs/KDHS_vs_KHIS_side_by_side.xlsx"
dir.create("outputs/slides", showWarnings = FALSE)

nat <- read.xlsx(f, "National side-by-side")
cty <- read.xlsx(f, "County side-by-side")
names(cty)[3:7] <- c("KDHS", "lo", "hi", "n", "KHIS")
names(cty)[8] <- "Diff"

# CV of the survey estimate: SE from the report's 95% CI (lower side where the CI is cut at 100)
cv_fun <- function(est, lo, hi) {
  se <- ifelse(hi >= 99.95, (est - lo) / 1.96, (hi - lo) / 3.92)
  ifelse(est > 0 & hi - lo > 0, 100 * se / est, NA_real_)
}
cty$CV <- cv_fun(cty$KDHS, cty$lo, cty$hi)

ci <- do.call(rbind, strsplit(nat[["Kenya.Mini.KDHS.95%.CI"]], "-"))
nat$lo <- as.numeric(ci[, 1]); nat$hi <- as.numeric(ci[, 2])
nat$CV <- cv_fun(nat[["Kenya.Mini.KDHS.2025-26.(%)"]], nat$lo, nat$hi)

keep <- !grepl("sensitivity", nat$Indicator)
tab <- data.frame(
  Indicator = nat$Indicator,
  KDHS = nat[["Kenya.Mini.KDHS.2025-26.(%)"]], KDHS_CI = nat[["Kenya.Mini.KDHS.95%.CI"]],
  CV_pct = round(nat$CV, 1),
  KHIS = round(nat[["KHIS.used.for.comparison.(%)"]], 1),
  Gap = round(nat[["Difference.Mini.KDHS.-.KHIS.(pp)"]], 1), Gap_CI = nat[["95%.CI.of.difference"]]
)[keep, ]
cv_cty <- cty %>% group_by(Indicator) %>%
  summarise(median_CV = round(median(CV, na.rm = TRUE), 1), pct_CV_over_30 = round(100 * mean(CV > 30, na.rm = TRUE), 0),
            pct_CV_over_15 = round(100 * mean(CV > 15, na.rm = TRUE), 0), n_counties = sum(!is.na(CV)))
write.csv(tab, "outputs/slides/national_ci_cv.csv", row.names = FALSE)
write.csv(cv_cty, "outputs/slides/county_cv_summary.csv", row.names = FALSE)
print(tab); print(cv_cty)

# ---- maps ----
cl <- function(x) gsub("county|city|[^a-z]", "", tolower(x))
geo <- st_read("../kenya_shape_files/gadm41_KEN_1.shp", quiet = TRUE) %>% st_make_valid() %>% mutate(k = cl(NAME_1))
ord <- c("Penta3 (12-23 months)", "Skilled birth attendance", "Facility delivery rate", "4+ ANC coverage",
         "8+ ANC coverage", "PNC for mother within 48 hours", "PNC for infant within 48 hours")
lab <- c("Penta3", "Skilled birth", "Facility delivery", "4+ ANC", "8+ ANC", "PNC mother", "PNC infant")
d <- cty %>% filter(Indicator %in% ord) %>% mutate(k = cl(County), ind = factor(lab[match(Indicator, ord)], lab))
mk <- function(col, scale, legend_name) {
  ps <- lapply(lab, function(l) {
    m <- geo %>% left_join(d %>% filter(ind == l), by = "k")
    ggplot(m) + geom_sf(aes(fill = .data[[col]]), colour = "white", linewidth = .1) + scale +
      labs(title = l) + theme_void(base_size = 13) +
      theme(plot.title = element_text(face = "bold", hjust = .5, size = 13), legend.position = "none")
  })
  wrap_plots(ps, nrow = 1)
}
sc_diff <- scale_fill_gradient2(low = colors_cema[["blue"]], mid = "white", high = colors_cema[["orange"]], midpoint = 0, limits = c(-40, 40), oob = scales::squish, na.value = "#bdbdbd", name = "pp")
g1 <- mk("Diff", sc_diff) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# one shared legend: rebuild with legend on the last panel
ps1 <- lapply(lab, function(l) {
  m <- geo %>% left_join(d %>% filter(ind == l), by = "k")
  ggplot(m) + geom_sf(aes(fill = Diff), colour = "white", linewidth = .1) + sc_diff + labs(title = l) + theme_void(base_size = 14) +
    theme(plot.title = element_text(face = "bold", hjust = .5, size = 14))
})
g1 <- wrap_plots(ps1, nrow = 1, guides = "collect") & theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))
ggsave("outputs/slides/map_difference.png", g1, width = 16, height = 4.6, dpi = 150, bg = "white")

sc_cv <- scale_fill_gradient(low = "#e3f4fb", high = colors_cema[["navy"]], limits = c(0, 40), oob = scales::squish, na.value = "#bdbdbd", name = "CV (%)")
ps2 <- lapply(lab, function(l) {
  m <- geo %>% left_join(d %>% filter(ind == l), by = "k")
  ggplot(m) + geom_sf(aes(fill = CV), colour = "white", linewidth = .1) + sc_cv + labs(title = l) + theme_void(base_size = 14) +
    theme(plot.title = element_text(face = "bold", hjust = .5, size = 14))
})
g2 <- wrap_plots(ps2, nrow = 1, guides = "collect") & theme(legend.position = "bottom", legend.key.width = unit(2, "cm"))
ggsave("outputs/slides/map_cv.png", g2, width = 16, height = 4.6, dpi = 150, bg = "white")
