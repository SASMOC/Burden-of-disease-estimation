# =============================================================================
# Mini KDHS 2025-26 vs KHIS: pentavalent 3 (DPT-HepB-Hib 3) coverage
# Children aged 12-23 months, national and county level
#
# Same approach as the KDHS 2022 vs KHIS manuscript. Run interactively in RStudio.
# =============================================================================

# =============================================================================
# METHODS IMPLEMENTED BY THIS SCRIPT
#
# QUESTION  Do KHIS routine data and the Mini KDHS 2025-26 give the same
#           pentavalent 3 coverage, nationally and by county?
#
# STEP 1  Mini KDHS coverage (section 2)
#   Taken from Appendix B (sampling-error tables B.2 - B.51) of the report:
#   "Received DPT-HepB-Hib vaccination (3 doses)", children 12-23 months, with
#   the weighted proportion, its standard error and the weighted / unweighted n.
#   These were extracted by extract_mkdhs_tables_B.R.
#
# STEP 2  KHIS coverage (section 3)
#   DHIS2 indicator "Proportion of under 1 year receiving DPT/Hep+HiB3" by county
#   and month. KHIS coverage = simple average of the 12 COMPLETE monthly values,
#   Feb 2024 - Jan 2025 (no proration, no partial months).
#   Why this window: children aged 12-23 months when the survey started (Nov 2025)
#   were born ~Nov 2023 - Nov 2024, and the 3rd dose is given 14 weeks after birth,
#   so their doses were administered ~Feb 2024 - Jan 2025.
#   National KHIS = same indicator calculated from the sum of the 47 counties.
#
# STEP 3  Compare (section 4)
#   Difference = KDHS - KHIS (percentage points). Two-proportion z-test
#   (prop.test) gives the 95% CI of the difference and the p-value:
#     KDHS : weighted proportion x number of children interviewed (unweighted n).
#            The weighted n in Appendix B only reflects county population share
#            (e.g. Lamu 5, Nairobi 256), so it is NOT used as a sample size.
#     KHIS : coverage x surviving infants out of surviving infants (mean of 12 months)
#   Extra (design-based): the same difference with the KDHS standard error from
#   Appendix B (it allows for clustering), shown as a sensitivity analysis.
#
# STEP 4  Classify (section 5)
#   |difference| < 5 pp = minimal; 5 to <10 = moderate; 10 or more = high.
#
# STEP 5  Outputs (section 6): tables (csv + xlsx), map, county difference plot.
# =============================================================================

# ---- 0. CONFIG --------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(openxlsx)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/khis-vs-mkdhs-comparison")
invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))

khis_file  <- "pnt3-coverage.xls"                                  # monthly county Penta3 coverage
dhs_file   <- "MKDHS_2025-26_appendixB_all_indicators.csv"         # from extract_mkdhs_tables_B.R
shape_file <- "../kenya_shape_files/gadm41_KEN_1.shp"              # county boundaries
out_dir    <- "outputs"
dir.create(out_dir, showWarnings = FALSE)

colors_cema <- c(blue = "#27aae1", orange = "#fe7501", navy = "#084887",
                 yellow = "#efca08", green = "#497048", teal = "#006d77")
# shades of the CEMA orange: light = <5 pp, medium = 5-10 pp, deep = 10+ pp
band_colors <- c("<5%" = "#fdc08a", "5-10%" = colors_cema[["orange"]], "10+%" = "#d03a00")

# make county names comparable between KHIS, the survey report and the shapefile
clean_name <- function(x) gsub("county|city|[^a-z]", "", tolower(x))

# ---- 1. Helper: two-proportion z-test ---------------------------------------
# x = number vaccinated, n = number of children; 1 = KDHS, 2 = KHIS
prop_test_results <- function(x1, n1, x2, n2) {
  t <- suppressWarnings(prop.test(c(x1, x2), c(n1, n2)))   # warning only flags small expected counts
  data.frame(estimate_diff = 100 * unname(t$estimate[1] - t$estimate[2]),
             diff_lower    = 100 * t$conf.int[1],
             diff_upper    = 100 * t$conf.int[2],
             p_value       = t$p.value)
}

# ---- 2. Mini KDHS 2025-26 coverage ------------------------------------------
dhs <- read.csv(dhs_file) |>
  filter(Indicator == "Received DPT-HepB-Hib vaccination (3 doses)",
         !Geography %in% c("Urban", "Rural")) |>
  transmute(county = ifelse(Geography == "Total", "Kenya (national)", Geography),
            key    = ifelse(Geography == "Total", "total", clean_name(Geography)),
            dhs_cov = 100 * Value, dhs_lower = 100 * Lower, dhs_upper = 100 * Upper,
            dhs_se = 100 * SE, dhs_n = N_unweighted, dhs_weighted_n = N_weighted)

# ---- 3. KHIS coverage: average of the 12 monthly values ---------------------
khis_monthly <- read_excel(khis_file) |>
  transmute(county = `Organisation unit`, key = clean_name(`Organisation unit`),
            month = Period, coverage = Value_COVERAGE, doses = Numerator,
            surviving_infants = Denominator, factor = Factor)

stopifnot(n_distinct(khis_monthly$month) == 12, n_distinct(khis_monthly$key) == 47)

# county level
khis_county <- khis_monthly |>
  group_by(county, key) |>
  summarise(khis_cov = mean(coverage), khis_min = min(coverage), khis_max = max(coverage),
            doses_12m = sum(doses), khis_pop = mean(surviving_infants), .groups = "drop")

# national level: add up the counties each month, recompute coverage, then average
khis_national <- khis_monthly |>
  group_by(month) |>
  summarise(coverage = sum(doses) / sum(surviving_infants) * first(factor),
            doses = sum(doses), surviving_infants = sum(surviving_infants)) |>
  summarise(county = "Kenya (national)", key = "total", khis_cov = mean(coverage),
            khis_min = min(coverage), khis_max = max(coverage),
            doses_12m = sum(doses), khis_pop = mean(surviving_infants))

khis <- bind_rows(khis_national, khis_county)

# ---- 4. Compare KDHS and KHIS -----------------------------------------------
results <- dhs |>
  inner_join(select(khis, -county), by = "key") |>
  mutate(khis_cov_capped = pmin(khis_cov, 100),                    # a proportion cannot exceed 100%
         dhs_x  = dhs_cov / 100 * dhs_n,                           # KDHS vaccinated (weighted proportion x children interviewed)
         khis_x = khis_cov_capped / 100 * khis_pop) |>              # KHIS equivalent count
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
                    labels = c("<5%", "5-10%", "10+%")))

national <- filter(results, key == "total")
counties <- filter(results, key != "total")

# ---- 6. Tables --------------------------------------------------------------
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

# Table 1: national comparison
table1 <- national |>
  transmute(`Vaccine antigen` = "Pentavalent 3",
            `KDHS 2025-26 coverage (%)` = round(dhs_cov, 2),
            `KDHS 95% CI` = sprintf("%.2f-%.2f", dhs_lower, dhs_upper),
            `KHIS coverage (%)` = round(khis_cov, 2),
            `Difference % (95% CI)` = sprintf("%.2f (%.2f, %.2f)", estimate_diff, diff_lower, diff_upper),
            `P-value` = fmt_p(p_value))

# Table 2: number of counties in each band
table2 <- counties |>
  count(band) |>
  mutate(`Pentavalent 3` = sprintf("%d (%.1f%%)", n, 100 * n / sum(n))) |>
  select(`Absolute difference in vaccine coverage` = band, `Pentavalent 3`)

# Supplementary table: county differences with 95% CI
supp_table <- counties |>
  arrange(county) |>
  transmute(County = county,
            `KDHS (%)` = round(dhs_cov, 1), `KDHS 95% CI` = sprintf("%.1f-%.1f", dhs_lower, dhs_upper),
            `KDHS n` = dhs_n, `KHIS (%)` = round(khis_cov, 1),
            `Difference (KDHS-KHIS), pp` = round(estimate_diff, 1),
            `95% CI of difference` = sprintf("%.1f, %.1f", diff_lower, diff_upper),
            `P-value` = fmt_p(p_value),
            `95% CI (design-based)` = sprintf("%.1f, %.1f", diff_lower_design, diff_upper_design),
            `P-value (design-based)` = fmt_p(p_value_design),
            Band = as.character(band))

# all geographies in one numeric table (national first)
all_results <- results |>
  transmute(Geography = county, KDHS_pct = round(dhs_cov, 2), KDHS_lower = round(dhs_lower, 2),
            KDHS_upper = round(dhs_upper, 2), KDHS_n = dhs_n, KHIS_pct = round(khis_cov, 2),
            Difference_pp = round(estimate_diff, 2), Diff_lower = round(diff_lower, 2),
            Diff_upper = round(diff_upper, 2), P_value = signif(p_value, 3),
            Diff_lower_design = round(diff_lower_design, 2), Diff_upper_design = round(diff_upper_design, 2),
            P_value_design = signif(p_value_design, 3), Band = as.character(band))

write.csv(table1, file.path(out_dir, "Table1_national.csv"), row.names = FALSE)
write.csv(table2, file.path(out_dir, "Table2_county_categories.csv"), row.names = FALSE)
write.csv(supp_table, file.path(out_dir, "SuppTable_county_diff.csv"), row.names = FALSE)
write.csv(all_results, file.path(out_dir, "penta3_difference_with_95CI_all_geographies.csv"), row.names = FALSE)

write.xlsx(list(Table1_national = table1, Table2_county_categories = table2,
                SuppTable_county_diff = supp_table, Diff_with_95CI_numeric = all_results,
                KHIS_monthly_input = select(khis_monthly, county, month, coverage, doses, surviving_infants)),
           file.path(out_dir, "MKDHS_vs_KHIS_Penta3_12month_mean.xlsx"))

# ---- 7. Figure 1: county map of the difference bands ------------------------
counties_sf <- st_read(shape_file, quiet = TRUE) |>
  mutate(key = clean_name(NAME_1)) |>
  left_join(select(counties, key, band), by = "key")

fig1 <- ggplot(counties_sf) +
  geom_sf(aes(fill = band), colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = band_colors, name = "Absolute difference\n(KDHS vs KHIS)", drop = FALSE) +
  theme_void(base_size = 12) +
  labs(title = "Pentavalent 3: Mini KDHS 2025-26 vs KHIS (mean Feb 2024 - Jan 2025)")

ggsave(file.path(out_dir, "Fig1_penta3_12m_map.png"), fig1, width = 8.5, height = 7, dpi = 300, bg = "white")

# ---- 8. Figure 2: difference and 95% CI, national and by county -------------
plot_data <- results |>
  mutate(county = ifelse(key == "total", "KENYA (NATIONAL)", county),
         is_national = key == "total",
         county = reorder(county, ifelse(is_national, -Inf, estimate_diff)))

fig2 <- ggplot(plot_data, aes(x = estimate_diff, y = county, colour = band)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_errorbar(aes(xmin = diff_lower, xmax = diff_upper), width = 0, alpha = 0.6, orientation = "y") +
  geom_point(aes(size = is_national)) +
  scale_size_manual(values = c(`FALSE` = 1.8, `TRUE` = 3.5), guide = "none") +
  scale_colour_manual(values = band_colors, name = "Absolute difference") +
  labs(x = "Difference in coverage, KDHS minus KHIS (percentage points, 95% CI)", y = NULL,
       title = "Pentavalent 3 by county: Mini KDHS 2025-26 vs KHIS 12-month mean") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "Fig2_penta3_12m_county_differences.png"), fig2, width = 8, height = 10, dpi = 300, bg = "white")

# ---- 9. Checks --------------------------------------------------------------
print(table1)
print(table2)
cat("Counties compared:", nrow(counties), "of 47\n")
cat("Counties with KHIS 12-month mean above 100%:", sum(counties$khis_cov > 100), "\n")
