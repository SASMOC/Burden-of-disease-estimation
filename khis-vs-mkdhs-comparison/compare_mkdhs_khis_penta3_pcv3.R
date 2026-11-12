# =============================================================================
# Mini KDHS 2025-26 vs KHIS: DPT-HepB-Hib 3 (pentavalent 3) and PCV3 coverage
# Children 12-23 months. Replicates the method of the BMJ Public Health paper
# (KDHS 2022 vs KHIS): KHIS doses administered in a window aligned to the birth
# months of the surveyed children (+14 weeks for the 3rd dose), divided by the
# surviving-infant population for the same window; differences (survey - KHIS)
# tested with a two-proportion z-test and classed <5 / 5-10 / 10+ percentage pts.
# Run interactively in RStudio. Requires: MKDHS_2025-26_appendixB_all_indicators.csv
# (made by extract_mkdhs_tables_B.R) and the KHIS exports in this folder.
# =============================================================================
suppressMessages({library(readxl); library(dplyr); library(tidyr); library(ggplot2)
                  library(sf); library(openxlsx); library(patchwork)})
invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))

base_dir <- "~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/khis-vs-mkdhs-comparison"
setwd(base_dir)
colors_cema <- c(blue = "#27aae1", orange = "#fe7501", navy = "#084887",
                 yellow = "#efca08", green = "#497048", teal = "#006d77")

# ---- 1. Survey-aligned KHIS window ------------------------------------------
fw_start <- as.Date("2025-11-05"); fw_end <- as.Date("2026-02-10")  # fieldwork dates (report 1.6)
birth_lo <- seq(fw_start, by = "-24 months", length.out = 2)[2] + 1  # youngest-age bound: oldest child (<24 m)
birth_hi <- seq(fw_end,   by = "-12 months", length.out = 2)[2]      # youngest child (12 m)
lag_days <- 14 * 7                                                    # 3rd dose at 14 weeks
win <- c(birth_lo + lag_days, birth_hi + lag_days)
cat("Birth window:", format(birth_lo), "to", format(birth_hi), "\n",
    "KHIS dose window (Penta3/PCV3):", format(win[1]), "to", format(win[2]), "\n")

# ---- 2. KHIS data ------------------------------------------------------------
mdate <- function(x) as.Date(paste("1", x), "%d %B %Y")
norm  <- function(x) gsub("county|city|[^a-z]", "", tolower(x))

# national (monthly doses and surviving infants)
nat <- read_excel("national-coverage-penta3-opv3-rota3.xls") |> as.data.frame()
nat <- nat |> transmute(month = mdate(Period), Data, Value) |>
  filter(Data %in% c("MOH710 DPT/Hep+HiB3 doses Administered", "MOH710 Pneumococal 3 doses Administered",
                     "Population surviving infants (under 1 year)")) |>
  mutate(var = recode(Data, "MOH710 DPT/Hep+HiB3 doses Administered" = "penta3",
                      "MOH710 Pneumococal 3 doses Administered" = "pcv3",
                      "Population surviving infants (under 1 year)" = "pop")) |>
  select(month, var, Value) |> distinct() |> pivot_wider(names_from = var, values_from = Value) |>
  mutate(geo = "Total", key = "total")

# county (add further county export files to this vector, e.g. Feb-May 2025)
county_files <- c("Downloaded-coverage-pentavalent3-doses-administered.csv")
cty <- bind_rows(lapply(county_files, read.csv, check.names = FALSE)) |>
  transmute(month = mdate(periodname), geo = organisationunitname, key = norm(organisationunitname),
            penta3 = `MOH710 DPT/Hep+HiB3 doses Administered`,
            pcv3   = `MOH710 Pneumococal 3 doses Administered`,
            pop    = `Population surviving infants (under 1 year)`) |> distinct(month, key, .keep_all = TRUE)

# ---- 3. Prorate to the window ------------------------------------------------
prorate <- function(df, win) {
  df |> mutate(first = month, last = seq(month[1], by = "month", length.out = 2)[2] - 1) |> rowwise() |>
    mutate(last = seq(first, by = "month", length.out = 2)[2] - 1,
           ov = max(0, as.numeric(min(last, win[2]) - max(first, win[1])) + 1),
           dim = as.numeric(last - first) + 1,
           yr_days = as.numeric(as.Date(paste0(format(first, "%Y"), "-12-31")) - as.Date(paste0(format(first, "%Y"), "-01-01"))) + 1) |>
    ungroup() |> filter(ov > 0) |>
    mutate(penta3_w = penta3 * ov / dim, pcv3_w = pcv3 * ov / dim, den_w = pop * ov / yr_days)
}
n_months_needed <- length(seq(as.Date(format(win[1], "%Y-%m-01")), as.Date(format(win[2], "%Y-%m-01")), by = "month"))
khis <- function(df) prorate(df, win) |> group_by(geo, key) |>
  summarise(months_available = n(), window_complete = n() == n_months_needed,
            penta3_doses = sum(penta3_w), pcv3_doses = sum(pcv3_w), denominator = sum(den_w), .groups = "drop") |>
  pivot_longer(c(penta3_doses, pcv3_doses), names_to = "antigen", values_to = "doses") |>
  mutate(antigen = ifelse(antigen == "penta3_doses", "Pentavalent 3 (DPT-HepB-Hib 3)", "PCV 3"),
         khis_cov = 100 * doses / denominator)
k <- bind_rows(khis(nat), khis(cty))

# ---- 4. Mini KDHS 2025-26 (Appendix B) --------------------------------------
b <- read.csv("MKDHS_2025-26_appendixB_all_indicators.csv") |>
  filter(Indicator %in% c("Received DPT-HepB-Hib vaccination (3 doses)", "Received pneumococcal vaccination (3 doses)")) |>
  transmute(geo_dhs = Geography, key = ifelse(Geography == "Total", "total", norm(Geography)),
            antigen = ifelse(grepl("DPT", Indicator), "Pentavalent 3 (DPT-HepB-Hib 3)", "PCV 3"),
            dhs_cov = 100 * Value, dhs_se = 100 * SE, dhs_lo = 100 * Lower, dhs_hi = 100 * Upper,
            dhs_n = N_unweighted, dhs_nw = N_weighted)
b <- filter(b, !geo_dhs %in% c("Urban", "Rural"))

# ---- 5. Compare (survey - KHIS) ----------------------------------------------
z2 <- function(p1, n1, p2, n2) {  # two-proportion z-test, pooled SE for the test, unpooled for the CI
  pb <- (p1 * n1 + p2 * n2) / (n1 + n2); d <- p1 - p2
  z <- d / sqrt(pb * (1 - pb) * (1 / n1 + 1 / n2)); se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
  list(d = d, lo = d - 1.96 * se, hi = d + 1.96 * se, p = 2 * pnorm(-abs(z)))
}
cmp <- inner_join(b, k, by = c("key", "antigen")) |> rowwise() |> mutate(
  t = list(z2(dhs_cov / 100, dhs_n, min(khis_cov, 100) / 100, denominator)),
  diff = 100 * t$d, diff_lo = 100 * t$lo, diff_hi = 100 * t$hi, p_value = t$p,
  # design-based alternative: survey SE (accounts for clustering) + binomial SE for KHIS
  se_db = sqrt(dhs_se^2 + (100 * sqrt(min(khis_cov, 100) / 100 * (1 - min(khis_cov, 100) / 100) / denominator))^2),
  diff_lo_db = diff - 1.96 * se_db, diff_hi_db = diff + 1.96 * se_db,
  p_value_db = 2 * pnorm(-abs(diff / se_db))) |> ungroup() |> select(-t) |>
  mutate(category = cut(abs(diff), c(-Inf, 5, 10, Inf), right = FALSE, labels = c("<5%", "5-10%", "10+%")),
         fmt_p = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)))
cmp$geo <- ifelse(cmp$key == "total", "Kenya (national)", cmp$geo_dhs)
cmp$antigen <- factor(cmp$antigen, c("Pentavalent 3 (DPT-HepB-Hib 3)", "PCV 3"))

# ---- 6. Tables ---------------------------------------------------------------
tab1 <- cmp |> filter(key == "total") |>
  transmute(`Vaccine antigen` = antigen, `KDHS 2025-26 coverage (%)` = sprintf("%.2f", dhs_cov),
            `KDHS 95% CI` = sprintf("%.2f-%.2f", dhs_lo, dhs_hi),
            `KHIS coverage (%)` = sprintf("%.2f", khis_cov),
            `Difference % (95% CI)` = sprintf("%.2f (%.2f, %.2f)", diff, diff_lo, diff_hi), `P-value` = fmt_p)
cty_cmp <- filter(cmp, key != "total")
tab2 <- cty_cmp |> count(antigen, category) |> group_by(antigen) |>
  mutate(txt = sprintf("%d (%.1f%%)", n, 100 * n / sum(n))) |> ungroup() |>
  select(antigen, category, txt) |> pivot_wider(names_from = antigen, values_from = txt, values_fill = "0 (0.0%)") |>
  rename(`Absolute difference in vaccine coverage` = category)
supp1 <- k |> filter(key == "total" | key %in% cty_cmp$key) |>
  transmute(key, Geography = ifelse(key == "total", "Kenya (national)", sub(" County$", "", geo)), Antigen = antigen,
            `KHIS dose window` = paste(format(win[1]), "to", format(win[2])),
            `Months of KHIS data available` = months_available, `Window complete` = window_complete,
            `KHIS numerator (doses, prorated)` = round(doses), `KHIS denominator (surviving infants, prorated)` = round(denominator),
            `KHIS coverage (%)` = round(khis_cov, 2)) |>
  left_join(cmp |> transmute(key, Antigen = antigen, `KDHS n (unweighted)` = dhs_n,
                             `KDHS n (weighted)` = dhs_nw, `KDHS coverage (%)` = round(dhs_cov, 2)),
            by = c("key", "Antigen")) |> select(-key) |> arrange(Antigen, Geography != "Kenya (national)", Geography)
supp2 <- cty_cmp |> transmute(County = geo_dhs, Antigen = antigen,
  `KDHS (%)` = round(dhs_cov, 1), `KDHS 95% CI` = sprintf("%.1f-%.1f", dhs_lo, dhs_hi), `KHIS (%)` = round(khis_cov, 1),
  `Difference (KDHS-KHIS), pp` = round(diff, 1), `95% CI (two-proportion)` = sprintf("%.1f, %.1f", diff_lo, diff_hi),
  `P-value` = fmt_p, `95% CI (design-based)` = sprintf("%.1f, %.1f", diff_lo_db, diff_hi_db),
  `P-value (design-based)` = ifelse(p_value_db < 0.001, "<0.001", sprintf("%.3f", p_value_db)),
  Category = as.character(category), `KHIS window complete` = window_complete, `KDHS n` = dhs_n) |>
  arrange(Antigen, County)
supp2_wide <- cty_cmp |> select(County = geo_dhs, antigen, diff, category) |>
  pivot_wider(names_from = antigen, values_from = c(diff, category)) |> arrange(County)

wb <- createWorkbook()
add <- function(nm, x) { addWorksheet(wb, nm); writeData(wb, nm, x); setColWidths(wb, nm, 1:ncol(x), "auto")
  addStyle(wb, nm, createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = TRUE), 1, 1:ncol(x), gridExpand = TRUE)
  freezePane(wb, nm, firstRow = TRUE) }
add("Table1_national", tab1); add("Table2_county_categories", tab2)
add("SuppTable1_num_denom", supp1); add("SuppTable2_county_diff", supp2); add("County_diff_wide", supp2_wide)
saveWorkbook(wb, "MKDHS_vs_KHIS_Penta3_PCV3_comparison.xlsx", overwrite = TRUE)

# ---- 7. Figures --------------------------------------------------------------
cat_cols <- c("<5%" = colors_cema[["blue"]], "5-10%" = colors_cema[["yellow"]], "10+%" = colors_cema[["orange"]])
ttl_suffix <- if (all(cty_cmp$window_complete)) "" else "\n(interim: KHIS county data end Jan 2025; Feb-May 2025 not yet loaded)"

shp <- st_read("../kenya_shape_files/gadm41_KEN_1.shp", quiet = TRUE) |> mutate(key = norm(NAME_1))
mapdat <- left_join(shp, cty_cmp |> select(key, antigen, category), by = "key", relationship = "many-to-many") |>
  filter(!is.na(antigen))
fig1 <- ggplot(mapdat) + geom_sf(aes(fill = category), colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = cat_cols, name = "Absolute difference\n(KDHS vs KHIS)", drop = FALSE) +
  facet_wrap(~antigen) + theme_void(base_size = 12) +
  labs(title = paste0("Discrepancy between Mini KDHS 2025-26 and KHIS coverage, children 12-23 months", ttl_suffix))
ggsave("Fig1_county_discrepancy_map.png", fig1, width = 11, height = 6, dpi = 300, bg = "white")

ord <- cty_cmp |> filter(grepl("Pentavalent", antigen)) |> arrange(diff) |> pull(geo_dhs)
fig2 <- cty_cmp |> mutate(geo_dhs = factor(geo_dhs, ord)) |>
  ggplot(aes(diff, geo_dhs, colour = category)) + geom_vline(xintercept = 0, colour = "grey50") +
  geom_errorbarh(aes(xmin = diff_lo, xmax = diff_hi), height = 0, alpha = 0.6) + geom_point(size = 1.8) +
  scale_colour_manual(values = cat_cols, name = "Absolute difference") + facet_wrap(~antigen) +
  labs(x = "Difference in coverage, KDHS minus KHIS (percentage points, 95% CI)", y = NULL,
       title = paste0("County-level differences, Mini KDHS 2025-26 vs KHIS", ttl_suffix)) +
  theme_minimal(base_size = 10) + theme(legend.position = "bottom")
ggsave("Fig2_county_differences.png", fig2, width = 11, height = 10, dpi = 300, bg = "white")

print(tab1); print(tab2)
cat("Counties matched:", length(unique(cty_cmp$key)), "of 47\n")
cat("County windows complete:", all(cty_cmp$window_complete), "\n")
