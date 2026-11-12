# =============================================================================
# Mini KDHS 2025-26 vs KHIS: pentavalent 3 and PCV3 coverage
# Children aged 12-23 months, national and county level
# KHIS coverage computed from DOSES ADMINISTERED and LIVE BIRTHS
#
# Run interactively in RStudio.
# =============================================================================

# =============================================================================
# METHODS IMPLEMENTED BY THIS SCRIPT
#
# QUESTION  Do KHIS routine data and the Mini KDHS 2025-26 give the same coverage of
#           the third dose of pentavalent (DPT-HepB-Hib) and pneumococcal vaccine?
#
# STEP 1  Mini KDHS coverage (section 2)
#   Appendix B (sampling-error tables): weighted proportion, SE and unweighted n,
#   children 12-23 months (extracted by extract_mkdhs_tables_B.R).
#
# STEP 2  KHIS coverage from counts (section 3)
#   Numerator    doses administered (MOH 710): DPT/Hep+HiB 3 and Pneumococcal 3, by county and month.
#   Denominator  estimated live births (KHIS expected live births). The annual estimate is
#                spread over the months pro rata to days:
#                  expected births in month = annual estimate x days in month / days in year
#   KHIS coverage = sum of doses over the window / sum of expected births over the window.
#   Window = Feb 2024 - Jan 2025 (12 complete months): children aged 12-23 months when the
#   survey started (Nov 2025) were born ~Nov 2023 - Nov 2024 and receive the 3rd dose
#   14 weeks after birth, i.e. ~Feb 2024 - Jan 2025.
#   National = the national KHIS file (the county totals match it).
#   NOT adjusted for reporting rate: the MOH 710 (immunization) reporting rate is not in the
#   files, only MOH 711. Add it to section 3 if it is exported.
#
# STEP 3  Compare (section 4)
#   Difference = KDHS - KHIS (percentage points). Two-proportion z-test (prop.test) gives the
#   95% CI of the difference and the p-value:
#     KDHS : weighted proportion x children interviewed (unweighted n)
#     KHIS : doses out of expected births
#   plus a design-based 95% CI that uses the KDHS standard error (sensitivity analysis).
#
# STEP 4  Classify (section 5): |difference| < 5 pp minimal, 5 to <10 moderate, 10+ high.
#
# STEP 5  Outputs (sections 6-9): tables (csv + xlsx), maps, difference plots, monthly trends.
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

doses_county_file <- "Downloaded-coverage-pentavalent3-doses-administered.csv"   # county doses, Feb 2024 - Jan 2025
births_county_file <- "KHIS-NOV-2023-OCTOBER-2025 DELIVERIES.xls"               # county estimated live births
national_file     <- "national-coverage-penta3-opv3-rota3.xls"                  # national doses and live births
dhs_file          <- "MKDHS_2025-26_appendixB_all_indicators.csv"
shape_file        <- "../kenya_shape_files/gadm41_KEN_1.shp"
out_dir           <- "outputs/vaccines_livebirths"
dir.create(file.path(out_dir, "figures"), recursive = TRUE, showWarnings = FALSE)

window_months <- seq(as.Date("2024-02-01"), by = "month", length.out = 12)       # Feb 2024 - Jan 2025

colors_cema <- c(blue = "#27aae1", orange = "#fe7501", navy = "#084887",
                 yellow = "#efca08", green = "#497048", teal = "#006d77")
# shades of the CEMA orange: light = <5 pp, medium = 5-10 pp, deep = 10+ pp
band_colors <- c("<5%" = "#fdc08a", "5-10%" = colors_cema[["orange"]], "10+%" = "#d03a00")

clean_name <- function(x) gsub("county|city|[^a-z]", "", tolower(x))
to_date    <- function(x) as.Date(paste("1", x), "%d %B %Y")

# Antigens to compare
antigens <- tribble(
  ~id,      ~label,                           ~dhs_indicator,                                  ~doses_var,
  "penta3", "Pentavalent 3 (DPT-HepB-Hib 3)", "Received DPT-HepB-Hib vaccination (3 doses)",   "penta3_doses",
  "pcv3",   "PCV3",                           "Received pneumococcal vaccination (3 doses)",   "pcv3_doses"
)

# ---- 1. Helper: two-proportion z-test ---------------------------------------
# x = number vaccinated, n = number of children; 1 = KDHS, 2 = KHIS
prop_test_results <- function(x1, n1, x2, n2) {
  t <- suppressWarnings(prop.test(c(x1, x2), c(n1, n2)))
  data.frame(estimate_diff = 100 * unname(t$estimate[1] - t$estimate[2]),
             diff_lower    = 100 * t$conf.int[1],
             diff_upper    = 100 * t$conf.int[2],
             p_value       = t$p.value)
}

# ---- 2. Mini KDHS 2025-26 ---------------------------------------------------
dhs <- read.csv(dhs_file) |>
  filter(Indicator %in% antigens$dhs_indicator, !Geography %in% c("Urban", "Rural")) |>
  transmute(dhs_indicator = Indicator,
            county = ifelse(Geography == "Total", "Kenya (national)", Geography),
            key    = ifelse(Geography == "Total", "total", clean_name(Geography)),
            dhs_cov = 100 * Value, dhs_lower = 100 * Lower, dhs_upper = 100 * Upper,
            dhs_se = 100 * SE, dhs_n = N_unweighted)

# ---- 3. KHIS: doses administered and live births ----------------------------
# national: doses and estimated live births by month
khis_national <- read_excel(national_file) |>
  filter(Data %in% c("MOH710 DPT/Hep+HiB3 doses Administered", "MOH710 Pneumococal 3 doses Administered",
                     "Estimated live births")) |>
  transmute(month = to_date(Period),
            var = recode(Data, "MOH710 DPT/Hep+HiB3 doses Administered" = "penta3_doses",
                         "MOH710 Pneumococal 3 doses Administered" = "pcv3_doses",
                         "Estimated live births" = "est_births"),
            value = Value) |>
  distinct() |>
  pivot_wider(names_from = var, values_from = value) |>
  mutate(key = "total")

# county: doses (csv) joined with estimated live births (deliveries file)
doses_county <- read.csv(doses_county_file, check.names = FALSE) |>
  transmute(month = to_date(periodname), key = clean_name(organisationunitname),
            penta3_doses = `MOH710 DPT/Hep+HiB3 doses Administered`,
            pcv3_doses   = `MOH710 Pneumococal 3 doses Administered`)
births_county <- suppressMessages(read_excel(births_county_file)) |>
  transmute(month = to_date(periodname), key = clean_name(organisationunitname),
            est_births = `Estimated live births`)
khis_county <- inner_join(doses_county, births_county, by = c("month", "key"))

# spread the annual live-birth estimate over the months pro rata to days
days_in_month <- function(d) as.numeric(as.Date(format(d + 32, "%Y-%m-01")) - d)
days_in_year  <- function(d) ifelse(as.numeric(format(d, "%Y")) %% 4 == 0, 366, 365)

khis_monthly <- bind_rows(khis_national, khis_county) |>
  mutate(expected_births = est_births * days_in_month(month) / days_in_year(month))

# check: the 47 counties add up to the national file
chk <- khis_monthly |> filter(month %in% window_months) |>
  group_by(month, is_national = key == "total") |>
  summarise(doses = sum(penta3_doses), births = sum(est_births), .groups = "drop") |>
  pivot_wider(names_from = is_national, values_from = c(doses, births))
stopifnot(all(chk$doses_FALSE == chk$doses_TRUE), all(abs(chk$births_FALSE - chk$births_TRUE) < 2))

# KHIS coverage over the 12-month window: total doses / total expected births
khis_long <- khis_monthly |>
  filter(month %in% window_months) |>
  group_by(key) |>
  summarise(months = n(), penta3_doses = sum(penta3_doses), pcv3_doses = sum(pcv3_doses),
            live_births = sum(expected_births), .groups = "drop") |>
  pivot_longer(c(penta3_doses, pcv3_doses), names_to = "doses_var", values_to = "doses") |>
  mutate(khis_cov = 100 * doses / live_births)

stopifnot(all(khis_long$months == 12), n_distinct(khis_long$key) == 48)   # 47 counties + national

# ---- 4. Compare KDHS and KHIS -----------------------------------------------
results <- antigens |>
  inner_join(dhs, by = "dhs_indicator") |>
  left_join(khis_long, by = c("doses_var", "key")) |>
  mutate(khis_cov_capped = pmin(khis_cov, 100),           # a proportion cannot exceed 100%
         dhs_x  = dhs_cov / 100 * dhs_n,
         khis_x = khis_cov_capped / 100 * live_births) |>
  rowwise() |>
  mutate(test = list(prop_test_results(dhs_x, dhs_n, khis_x, live_births))) |>
  unnest_wider(test) |>
  ungroup() |>
  mutate(# sensitivity: survey standard error (design-based) + binomial SE of KHIS
         khis_se = 100 * sqrt(khis_cov_capped / 100 * (1 - khis_cov_capped / 100) / live_births),
         diff_se_design = sqrt(dhs_se^2 + khis_se^2),
         diff_lower_design = estimate_diff - 1.96 * diff_se_design,
         diff_upper_design = estimate_diff + 1.96 * diff_se_design,
         p_value_design = 2 * pnorm(-abs(estimate_diff / diff_se_design)))

# ---- 5. Classify differences ------------------------------------------------
results <- results |>
  mutate(band = cut(abs(estimate_diff), breaks = c(-Inf, 5, 10, Inf), right = FALSE,
                    labels = c("<5%", "5-10%", "10+%")),
         label = factor(label, levels = antigens$label))

national <- filter(results, key == "total")
counties <- filter(results, key != "total")

# ---- 6. Tables --------------------------------------------------------------
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

# Table 1: national comparison
table1 <- national |>
  arrange(label) |>
  transmute(`Vaccine antigen` = as.character(label),
            `KDHS 2025-26 (%)` = round(dhs_cov, 1), `KDHS 95% CI` = sprintf("%.1f-%.1f", dhs_lower, dhs_upper),
            `KHIS doses administered` = round(doses), `KHIS live births` = round(live_births),
            `KHIS coverage (%)` = round(khis_cov, 1),
            `Difference % (95% CI)` = sprintf("%.1f (%.1f, %.1f)", estimate_diff, diff_lower, diff_upper),
            `P-value` = fmt_p(p_value), Band = as.character(band))

# Table 2: number of counties in each band
table2 <- counties |>
  count(label, band, .drop = FALSE) |>
  group_by(label) |>
  mutate(txt = sprintf("%d (%.1f%%)", n, 100 * n / sum(n))) |>
  ungroup() |>
  select(label, band, txt) |>
  pivot_wider(names_from = label, values_from = txt) |>
  rename(`Absolute difference` = band)

# Supplementary table: every county, difference with 95% CI
supp_table <- counties |>
  arrange(label, county) |>
  transmute(Antigen = as.character(label), County = county,
            `KDHS (%)` = round(dhs_cov, 1), `KDHS 95% CI` = sprintf("%.1f-%.1f", dhs_lower, dhs_upper),
            `KDHS n` = dhs_n, `KHIS doses` = round(doses), `KHIS live births` = round(live_births),
            `KHIS (%)` = round(khis_cov, 1),
            `Difference (KDHS-KHIS), pp` = round(estimate_diff, 1),
            `95% CI of difference` = sprintf("%.1f, %.1f", diff_lower, diff_upper),
            `P-value` = fmt_p(p_value),
            `95% CI (design-based)` = sprintf("%.1f, %.1f", diff_lower_design, diff_upper_design),
            `P-value (design-based)` = fmt_p(p_value_design), Band = as.character(band))

# numeric table, national + counties
all_results <- results |>
  arrange(label, key != "total", county) |>
  transmute(Antigen = as.character(label), Geography = county,
            KDHS_pct = round(dhs_cov, 2), KDHS_lower = round(dhs_lower, 2), KDHS_upper = round(dhs_upper, 2),
            KDHS_n = dhs_n, KHIS_doses = round(doses), KHIS_live_births = round(live_births),
            KHIS_pct = round(khis_cov, 2), Difference_pp = round(estimate_diff, 2),
            Diff_lower = round(diff_lower, 2), Diff_upper = round(diff_upper, 2), P_value = signif(p_value, 3),
            Diff_lower_design = round(diff_lower_design, 2), Diff_upper_design = round(diff_upper_design, 2),
            P_value_design = signif(p_value_design, 3), Band = as.character(band))

write.csv(table1, file.path(out_dir, "Table1_national.csv"), row.names = FALSE)
write.csv(table2, file.path(out_dir, "Table2_county_categories.csv"), row.names = FALSE)
write.csv(supp_table, file.path(out_dir, "SuppTable_county_diff.csv"), row.names = FALSE)
write.csv(all_results, file.path(out_dir, "vaccines_difference_with_95CI_all_geographies.csv"), row.names = FALSE)

write.xlsx(list(Table1_national = table1, Table2_county_categories = table2,
                SuppTable_county_diff = supp_table, Diff_with_95CI_numeric = all_results,
                KHIS_monthly_input = khis_monthly |> filter(month %in% window_months) |>
                  select(key, month, penta3_doses, pcv3_doses, est_births, expected_births)),
           file.path(out_dir, "MKDHS_vs_KHIS_vaccines_livebirths.xlsx"))

# ---- 7. Maps: one per antigen, plus a combined map --------------------------
counties_sf <- st_read(shape_file, quiet = TRUE) |> mutate(key = clean_name(NAME_1))

map_plot <- function(data, title = NULL) {
  ggplot(data) +
    geom_sf(aes(fill = band), colour = "white", linewidth = 0.2) +
    scale_fill_manual(values = band_colors, name = "Absolute difference\n(KDHS vs KHIS)", drop = FALSE) +
    theme_void(base_size = 14) + labs(title = title)
}

for (i in seq_len(nrow(antigens))) {
  d <- left_join(counties_sf, filter(counties, id == antigens$id[i]) |> select(key, band), by = "key")
  p <- map_plot(d, paste0(antigens$label[i], ": Mini KDHS 2025-26 vs KHIS (Feb 2024 - Jan 2025)"))
  ggsave(file.path(out_dir, "figures", paste0("Map_", antigens$id[i], ".png")), p,
         width = 8.5, height = 7, dpi = 300, bg = "white")
}

all_maps <- counties_sf |> select(key) |>
  inner_join(select(counties, key, label, band), by = "key", relationship = "many-to-many")
fig_all <- map_plot(all_maps, "Absolute difference between Mini KDHS 2025-26 and KHIS (Feb 2024 - Jan 2025)") +
  facet_wrap(~label)
ggsave(file.path(out_dir, "figures", "Map_both_antigens.png"), fig_all, width = 14, height = 7, dpi = 300, bg = "white")

# ---- 8. Difference and 95% CI plots: national and county --------------------
for (i in seq_len(nrow(antigens))) {
  d <- results |>
    filter(id == antigens$id[i]) |>
    mutate(county = ifelse(key == "total", "KENYA (NATIONAL)", county), is_national = key == "total",
           county = reorder(county, ifelse(is_national, -Inf, estimate_diff)))
  p <- ggplot(d, aes(x = estimate_diff, y = county, colour = band)) +
    geom_vline(xintercept = 0, colour = "grey50") +
    geom_errorbar(aes(xmin = diff_lower, xmax = diff_upper), width = 0, alpha = 0.6, orientation = "y") +
    geom_point(aes(size = is_national)) +
    scale_size_manual(values = c(`FALSE` = 1.8, `TRUE` = 3.5), guide = "none") +
    scale_colour_manual(values = band_colors, name = "Absolute difference", drop = FALSE) +
    labs(x = "Difference in coverage, KDHS minus KHIS (percentage points, 95% CI)", y = NULL,
         title = paste0(antigens$label[i], " by county: Mini KDHS 2025-26 vs KHIS")) +
    theme_minimal(base_size = 12) + theme(legend.position = "bottom")
  ggsave(file.path(out_dir, "figures", paste0("Difference_", antigens$id[i], ".png")), p,
         width = 8, height = 10, dpi = 300, bg = "white")
}

# ---- 9. Monthly trends ------------------------------------------------------
# Monthly KHIS coverage = doses in the month / expected births in the month.
# All trend plots use a fixed 0-100% axis.
trend_data <- khis_monthly |>
  pivot_longer(c(penta3_doses, pcv3_doses), names_to = "doses_var", values_to = "doses") |>
  mutate(coverage = 100 * doses / expected_births) |>
  inner_join(antigens, by = "doses_var") |>
  inner_join(select(results, id, key, county, dhs_cov, dhs_lower, dhs_upper), by = c("id", "key")) |>
  mutate(label = factor(label, levels = antigens$label))

write.csv(select(trend_data, id, label, geography = county, month, doses, expected_births, coverage,
                 kdhs_pct = dhs_cov, kdhs_lower = dhs_lower, kdhs_upper = dhs_upper),
          file.path(out_dir, "khis_monthly_trend_data.csv"), row.names = FALSE)

# 9a. National: both antigens in ONE plot (24 months). Lines = KHIS monthly; points with bars
#     at the right = Mini KDHS 2025-26 estimate and 95% CI.
ant_colors <- setNames(c(colors_cema[["orange"]], colors_cema[["teal"]]), antigens$label)
nat_lines <- filter(trend_data, key == "total")
nat_survey <- results |>
  filter(key == "total") |>
  mutate(x = max(nat_lines$month) + 45 + 15 * (row_number() - 1))

fig_trend_national <- ggplot() +
  geom_line(data = nat_lines, aes(x = month, y = coverage, colour = label), linewidth = 1) +
  geom_errorbar(data = nat_survey, aes(x = x, ymin = dhs_lower, ymax = dhs_upper, colour = label),
                width = 8, linewidth = 0.6, show.legend = FALSE) +
  geom_point(data = nat_survey, aes(x = x, y = dhs_cov, colour = label), size = 2.6, show.legend = FALSE) +
  geom_vline(xintercept = c(min(window_months), max(window_months)), linetype = "dotted", colour = "grey50") +
  scale_colour_manual(name = NULL, values = ant_colors) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = NULL, y = "Coverage (%)",
       title = "Monthly KHIS coverage vs Mini KDHS 2025-26, Kenya",
       subtitle = "Lines = KHIS doses / expected live births. Points with bars at the right = Mini KDHS\n2025-26 estimate and 95% CI. Dotted lines = comparison window (Feb 2024 - Jan 2025)") +
  theme_minimal(base_size = 18) +   # large text so the plot is easy to read
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.subtitle = element_text(size = 14))
ggsave(file.path(out_dir, "figures", "Trend_national_both_antigens.png"), fig_trend_national,
       width = 14, height = 8.5, dpi = 300, bg = "white")

# 9b. County: one figure per antigen, one panel per county (the 12-month window)
for (i in seq_len(nrow(antigens))) {
  d <- filter(trend_data, key != "total", id == antigens$id[i], month %in% window_months)
  p <- ggplot(d, aes(x = month)) +
    geom_ribbon(aes(ymin = dhs_lower, ymax = dhs_upper, group = 1), fill = colors_cema[["navy"]], alpha = 0.15) +
    geom_hline(aes(yintercept = dhs_cov, colour = "Mini KDHS 2025-26"), linewidth = 0.7) +
    geom_line(aes(y = coverage, colour = "KHIS (doses / expected live births)"), linewidth = 0.8) +
    scale_colour_manual(name = NULL, values = c("Mini KDHS 2025-26" = colors_cema[["navy"]],
                                                "KHIS (doses / expected live births)" = colors_cema[["orange"]])) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b") +
    scale_y_continuous(breaks = c(0, 50, 100)) +
    coord_cartesian(ylim = c(0, 100)) +
    facet_wrap(~county, ncol = 7) +
    labs(x = NULL, y = "Coverage (%)",
         title = paste0(antigens$label[i], ": monthly KHIS coverage vs Mini KDHS 2025-26 by county (Feb 2024 - Jan 2025)"),
         subtitle = "Shaded band = Mini KDHS 95% CI") +
    theme_minimal(base_size = 16) +   # large text so the panels are easy to read
    theme(legend.position = "bottom", plot.subtitle = element_text(size = 14))
  ggsave(file.path(out_dir, "figures", paste0("Trend_county_", antigens$id[i], ".png")), p,
         width = 28, height = 22, dpi = 200, bg = "white")
}

# ---- 10. Checks -------------------------------------------------------------
print(as.data.frame(table1))
print(as.data.frame(table2))
cat("Counties compared per antigen:\n"); print(table(counties$label))
cat("Counties with KHIS coverage above 100%:\n"); print(tapply(counties$khis_cov > 100, counties$label, sum))
