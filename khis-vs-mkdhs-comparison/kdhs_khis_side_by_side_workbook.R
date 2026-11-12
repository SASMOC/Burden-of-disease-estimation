# KDHS (report, Appendix B) vs KHIS side by side: Penta3, SBA, facility delivery, 4+ ANC, 8+ ANC, PNC mother and infant (48 h)
# Combines outputs of compare_mkdhs_khis_vaccines_livebirths.R and compare_mkdhs_khis_maternal_care.R
# Saves one Excel workbook. Run interactively in RStudio.
library(readr); library(dplyr); library(tidyr); library(openxlsx); library(ggplot2)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/khis-vs-mkdhs-comparison")
colors_cema <- c(blue = "#27aae1", orange = "#fe7501", navy = "#084887", yellow = "#efca08", green = "#497048", teal = "#006d77")

# ---- 1. Read the comparison tables (KDHS = report Appendix B values) ----
mat <- read_csv("outputs/maternal_care/maternal_care_difference_with_95CI_all_geographies.csv", show_col_types = FALSE)
vac <- read_csv("outputs/vaccines_livebirths/vaccines_difference_with_95CI_all_geographies.csv", show_col_types = FALSE)

KEEP <- c("Pentavalent 3 (DPT-HepB-Hib 3)" = "Penta3 (12-23 months)",
          "Skilled birth attendance" = "Skilled birth attendance",
          "Facility delivery" = "Facility delivery rate",
          "4+ ANC visits" = "4+ ANC coverage",
          "8+ ANC visits" = "8+ ANC coverage",
          "PNC for mother, 48 hours" = "PNC for mother within 48 hours",
          "PNC for newborn, 48 hours" = "PNC for infant within 48 hours")
ORDER <- unname(KEEP); ORDER <- c(ORDER[1], "Penta3, estimated live births (sensitivity)", "Penta3, MOH 711 live births / 0.911 (sensitivity)", ORDER[-1])

a <- vac %>% filter(Antigen == "Pentavalent 3 (DPT-HepB-Hib 3)") %>%
  transmute(Indicator = KEEP[Antigen], Geography, KDHS_pct, KDHS_lower, KDHS_upper, KDHS_n,
            KHIS_as_reported_pct = KHIS_pct, KHIS_reporting_rate_pct = NA_real_, KHIS_pct = KHIS_pct,
            Difference_pp, Diff_lower, Diff_upper, P_value, Band, KHIS_numerator = KHIS_doses, KHIS_denominator = KHIS_live_births)
b <- mat %>% filter(Indicator %in% names(KEEP)) %>%
  transmute(Indicator = KEEP[Indicator], Geography, KDHS_pct, KDHS_lower, KDHS_upper, KDHS_n,
            KHIS_as_reported_pct = KHIS_reported_pct, KHIS_reporting_rate_pct, KHIS_pct = KHIS_adjusted_pct,
            Difference_pp, Diff_lower, Diff_upper, P_value, Band, KHIS_numerator = NA_real_, KHIS_denominator = NA_real_)
# Override PNC mother, PNC infant and 4+ ANC with KHIS values recomputed from counts over ACTUAL denominators
# (khis_recompute_pnc_anc.R): PNC mother / skilled deliveries; PNC infant and 4+ ANC / MOH 711 live births; Nov 2023-Oct 2025.
cl <- function(x) gsub("county|city|[^a-z]", "", tolower(x))
rc <- bind_rows(read_csv("outputs/khis_recomputed_pnc_anc4.csv", show_col_types = FALSE), read_csv("outputs/khis_recomputed_penta3_lag3.csv", show_col_types = FALSE)) %>% select(Indicator, k = key, nn = KHIS_numerator, dd = KHIS_denominator, pp = KHIS_pct)
b <- bind_rows(a, a %>% mutate(Indicator = "Penta3, estimated live births (sensitivity)"), a %>% mutate(Indicator = "Penta3, MOH 711 live births / 0.911 (sensitivity)"), b) %>% mutate(k = ifelse(Geography == "Kenya (national)", "kenya", cl(Geography))) %>% left_join(rc, by = c("Indicator", "k")) %>%
  mutate(use = !is.na(pp),
         KHIS_pct = ifelse(use, pp, KHIS_pct), KHIS_as_reported_pct = ifelse(use, pp, KHIS_as_reported_pct),
         KHIS_reporting_rate_pct = ifelse(use, NA, KHIS_reporting_rate_pct),
         KHIS_numerator = ifelse(use, nn, KHIS_numerator), KHIS_denominator = ifelse(use, dd, KHIS_denominator),
         p1 = KDHS_pct / 100, p2 = pmin(KHIS_pct, 100) / 100,
         se = sqrt(p1 * (1 - p1) / KDHS_n + p2 * (1 - p2) / KHIS_denominator),
         Difference_pp = ifelse(use, KDHS_pct - KHIS_pct, Difference_pp),
         Diff_lower = ifelse(use, Difference_pp - 196 * se, Diff_lower), Diff_upper = ifelse(use, Difference_pp + 196 * se, Diff_upper),
         P_value = ifelse(use, 2 * pnorm(-abs(Difference_pp / (100 * se))), P_value)) %>%
  select(-k, -nn, -dd, -pp, -use, -p1, -p2, -se)
DEN <- c("Penta3 (12-23 months)" = "KHIS under-1 population (surviving infants), annual figure pro-rated by days, Feb 2024-Jan 2025; no birth lag because it is a population of infants alive in the period",
         "Penta3, estimated live births (sensitivity)" = "Estimated live births (facility + community), pro-rated by days, same 3-month lag and doses",
         "Penta3, MOH 711 live births / 0.911 (sensitivity)" = "MOH 711 live births / 0.911 (all live births: 91.1% of live births occur in a health facility, Kenya Mini KDHS Table 9.7), lagged 3 months for the 14-week schedule: doses Feb 2024-Jan 2025 over births Nov 2023-Oct 2024 (no reporting-rate adjustment)",
         "Skilled birth attendance" = "Expected births implied by the KHIS skilled-delivery indicator (= estimated live births); monthly values adjusted for reporting rate, averaged over 24 months",
         "Facility delivery rate" = "Same as skilled birth attendance",
         "4+ ANC coverage" = "MOH 711 live births / 0.911 (all live births: 91.1% of live births occur in a health facility, Kenya Mini KDHS Table 9.7), Nov 2023-Oct 2025; numerator = MOH 711 pregnant women completing 4 ANC visits",
         "8+ ANC coverage" = "NOT recomputed: KHIS indicator percentage as exported (no count of women completing 8 contacts in the files)",
         "PNC for mother within 48 hours" = "Total skilled deliveries (MOH 711), Nov 2023-Oct 2025",
         "PNC for infant within 48 hours" = "MOH 711 live births / 0.911 (all live births: 91.1% of live births occur in a health facility, Kenya Mini KDHS Table 9.7), Nov 2023-Oct 2025; numerator rebuilt as KHIS % x live births")
all <- b %>% mutate(Indicator = factor(Indicator, ORDER)) %>% arrange(Indicator, Geography != "Kenya (national)", Geography) %>%
  mutate(KHIS_within_KDHS_CI = ifelse(KHIS_pct >= KDHS_lower & KHIS_pct <= KDHS_upper, "Yes", "No"),
         Agreement = case_when(abs(Difference_pp) < 5 ~ "Minimal difference (<5 pp)",
                               abs(Difference_pp) < 10 ~ "Moderate difference (5-10 pp)", TRUE ~ "Large difference (10+ pp)"),
         Direction = ifelse(Difference_pp > 0, "KHIS lower than KDHS", "KHIS higher than KDHS"))

# ---- 1b. Uncertainty of the difference (design-based) ----
# The Kenya Mini KDHS standard error is taken from the report's own 95% interval (which already reflects the survey design:
# clusters, strata, weights). KHIS is treated as a fixed count of reported services (no sampling error), so the difference
# has the survey's uncertainty only: Difference +/- 1.96 x SE_KDHS; z = Difference / SE_KDHS. Where the report interval is cut at 100%
# the SE is taken from the other side; where the interval is 100-100 (no variation) the CI and p-value are not available (NA).
all <- all %>% mutate(
  se_k = case_when(KDHS_upper >= 100 & KDHS_lower < KDHS_pct ~ (KDHS_pct - KDHS_lower) / 1.96,
                   KDHS_lower <= 0 & KDHS_upper > KDHS_pct ~ (KDHS_upper - KDHS_pct) / 1.96,
                   TRUE ~ (KDHS_upper - KDHS_lower) / 3.92),
  se_k = ifelse(se_k > 0, se_k, NA_real_),
  Diff_lower = Difference_pp - 1.96 * se_k, Diff_upper = Difference_pp + 1.96 * se_k,
  P_value = 2 * pnorm(-abs(Difference_pp / se_k)))

# ---- 2. National side by side ----
national <- all %>% filter(Geography == "Kenya (national)") %>%
  transmute(Indicator, `Kenya Mini KDHS 2025-26 (%)` = KDHS_pct, `Kenya Mini KDHS 95% CI` = sprintf("%.1f-%.1f", KDHS_lower, KDHS_upper), `Kenya Mini KDHS n (unweighted)` = KDHS_n,
            `KHIS denominator used` = DEN[as.character(Indicator)], `KHIS numerator` = KHIS_numerator, `KHIS denominator (count)` = KHIS_denominator,
            `KHIS as reported (%)` = KHIS_as_reported_pct, `KHIS reporting rate (%)` = KHIS_reporting_rate_pct, `KHIS used for comparison (%)` = KHIS_pct,
            `Difference Mini KDHS - KHIS (pp)` = Difference_pp, `95% CI of difference` = sprintf("%.1f to %.1f", Diff_lower, Diff_upper),
            `P-value` = ifelse(P_value < 0.001, "<0.001", sprintf("%.3f", P_value)), `KHIS within Mini KDHS 95% CI` = KHIS_within_KDHS_CI, Agreement, Direction)

# ---- 3. County side by side ----
county <- all %>% filter(Geography != "Kenya (national)") %>% group_by(Indicator) %>% mutate(p_holm = p.adjust(P_value, "holm")) %>% ungroup() %>%
  transmute(Indicator, County = Geography, `Kenya Mini KDHS (%)` = KDHS_pct, `Mini KDHS lower` = KDHS_lower, `Mini KDHS upper` = KDHS_upper, `Mini KDHS n` = KDHS_n,
            `KHIS (%)` = KHIS_pct, `Difference Mini KDHS - KHIS (pp)` = Difference_pp, `Diff lower` = Diff_lower, `Diff upper` = Diff_upper,
            `P-value` = ifelse(is.na(P_value), NA_character_, ifelse(P_value < 0.001, "<0.001", sprintf("%.3f", P_value))),
            `P-value (Holm, 47 counties)` = ifelse(is.na(p_holm), NA_character_, ifelse(p_holm < 0.001, "<0.001", sprintf("%.3f", p_holm))),
            `KHIS within Mini KDHS CI` = KHIS_within_KDHS_CI, Agreement)

# ---- 4. Agreement across the 47 counties ----
ccc <- function(x, y) 2 * cov(x, y) / (var(x) + var(y) + (mean(x) - mean(y))^2)       # Lin's concordance correlation
agree <- all %>% filter(Geography != "Kenya (national)", !is.na(KDHS_pct), !is.na(KHIS_pct)) %>% group_by(Indicator) %>%
  summarise(Counties = n(), `Mean KDHS (%)` = mean(KDHS_pct), `Mean KHIS (%)` = mean(KHIS_pct),
            `Bias: mean KDHS - KHIS (pp)` = mean(KDHS_pct - KHIS_pct), `SD of differences` = sd(KDHS_pct - KHIS_pct),
            `Limits of agreement, lower` = mean(KDHS_pct - KHIS_pct) - 1.96 * sd(KDHS_pct - KHIS_pct),
            `Limits of agreement, upper` = mean(KDHS_pct - KHIS_pct) + 1.96 * sd(KDHS_pct - KHIS_pct),
            `Mean absolute difference (pp)` = mean(abs(KDHS_pct - KHIS_pct)),
            `Pearson r` = cor(KDHS_pct, KHIS_pct), `Lin concordance (CCC)` = ccc(KDHS_pct, KHIS_pct),
            `% counties within 5 pp` = 100 * mean(abs(KDHS_pct - KHIS_pct) < 5), `% counties within 10 pp` = 100 * mean(abs(KDHS_pct - KHIS_pct) < 10),
            `% KHIS inside KDHS 95% CI` = 100 * mean(KHIS_within_KDHS_CI == "Yes"), .groups = "drop") %>%
  mutate(`Agreement (CCC)` = cut(`Lin concordance (CCC)`, c(-Inf, .5, .75, .9, Inf), labels = c("Poor (<0.50)", "Moderate (0.50-0.75)", "Good (0.75-0.90)", "Excellent (>0.90)")),
         across(where(is.numeric), ~ round(., 2)))

nat_lower <- sum(national$`Difference Mini KDHS - KHIS (pp)` > 0)
county_higher <- paste(sapply(ORDER, function(i) sprintf("%s %d", i, sum(county$`Difference Mini KDHS - KHIS (pp)`[county$Indicator == i] < 0))), collapse = "; ")
notes <- tibble(Item = c("KDHS", "KHIS Penta3", "KHIS maternal indicators", "Difference", "Agreement bands", "Agreement across counties", "Reading the results"),
  Description = c(
    "Kenya Mini KDHS 2025-26 published estimates (Appendix B sampling-error tables). Penta3 = children 12-23 months; maternal indicators = women 15-49 with a live birth in the 2 years before the survey. National values were checked against the Appendix B extract.",
    "Headline Penta3 = doses administered (MOH 710 'MOH710 DPT/Hep+HiB3' column), Feb 2024-Jan 2025, divided by the KHIS under-1 population (surviving infants), the denominator of the coverage KHIS itself reports; the annual population is pro-rated by days and no birth lag is applied because it is a population of infants alive in the period. KHIS's own indicator uses the 'Essential - DPT3' doses column (5,525 fewer doses), so the figure here is slightly higher than the coverage KHIS reports. Two sensitivity rows use live births instead, with doses given 3 months after the births (dose 3 is at about 14 weeks; a 3-month shift approximates 14 weeks): (a) KHIS estimated live births pro-rated by days; (b) MOH 711 live births recorded at facilities divided by 0.911, because 91.1% of live births occur in a health facility (8.6% at home, 0.3% elsewhere) in Kenya Mini KDHS Table 9.7. Row (b) uses a survey share to scale facility births to all births, applies the same national share to every county, and is not a KHIS community count; it is above 100% nationally, which means that denominator is too small. Percentages above 100% are not capped in the table. Penta3 is NOT adjusted for reporting rate (the MOH 710 reporting rate is not in the exported files), unlike the maternal indicators. Skilled birth attendance and facility delivery use the same KHIS count (skilled deliveries = deliveries in facilities), so their KHIS values are identical.",
    "All maternal indicators use the 24 months Nov 2023-Oct 2025 (the 2 years before the survey started on 5 Nov 2025). PNC mother = MOH 711 mothers with PNC within 48 h / total skilled deliveries. PNC infant = infants with PNC within 48 h / (MOH 711 live births / 0.911), where the infant count is rebuilt as the KHIS percentage x MOH 711 live births (the exports hold only the percentage; the same rebuild for mothers reproduces KHIS's own percentage exactly, but this was not directly verifiable for infants). 4+ ANC = MOH 711 pregnant women completing 4 ANC visits / (MOH 711 live births / 0.911), as a same-period ratio (4th visits in the months versus births in the same months; no lag applied). These three are ratios of sums of counts from the same MOH 711 reports, so no reporting-rate adjustment was applied. Values above 100% are not capped in the table. 8+ ANC is NOT recomputed: the exports hold only the KHIS percentage, not the number of women completing 8 contacts. Skilled birth attendance and facility delivery keep the KHIS skilled-delivery indicator adjusted for reporting rate, averaged over 24 months.",
    "Kenya Mini KDHS minus KHIS in percentage points; positive = KHIS lower than the survey. The 95% interval (error bars) and p-value of the difference use the survey's design-based standard error, taken from the report's own 95% interval, and treat KHIS as a fixed count of reported services (no sampling error): difference +/- 1.96 x SE, z = difference / SE. Where the report interval is cut at 100% the SE is taken from the lower side; where it is 100-100 there is no interval or p-value (blank). County p-values are shown unadjusted and Holm-adjusted across the 47 counties of each indicator. The p-value tests only whether the difference exceeds survey sampling error; it does not account for KHIS reporting error or denominator error, so a small p-value does not mean the KHIS figure is right or wrong.",
    "Minimal <5 pp, moderate 5 to <10 pp, large 10+ pp.",
    "Bias and limits of agreement (Bland-Altman) of KDHS - KHIS over the 47 counties; Lin's concordance correlation (CCC) combines correlation and bias (1 = perfect). Means are simple averages of the 47 county values (not population-weighted). Counties where Kenya Mini KDHS = 100% have a 100-100 interval, so KHIS cannot fall inside it (this affects the '% KHIS inside CI' column: 4 counties for facility delivery, 5 for skilled birth attendance, 2 for Penta3).",
    paste0("National level: KHIS minus Kenya Mini KDHS is negative for ", nat_lower, " of 7 indicators (see the National sheet). County level: number of counties where KHIS is HIGHER than the survey, by indicator: ", county_higher, ". The reasons for the differences were not tested here.")))

# ---- 5. Figures for the workbook ----
nat_long <- national %>% transmute(Indicator, KDHS = `Kenya Mini KDHS 2025-26 (%)`, KHIS = `KHIS used for comparison (%)`, lo = as.numeric(sub("-.*", "", `Kenya Mini KDHS 95% CI`)), hi = as.numeric(sub(".*-", "", `Kenya Mini KDHS 95% CI`))) %>%
  pivot_longer(c(KDHS, KHIS), names_to = "Source", values_to = "pct") %>% mutate(lo = ifelse(Source == "KDHS", lo, NA), hi = ifelse(Source == "KDHS", hi, NA))
th <- theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"), legend.position = "bottom", strip.text = element_text(face = "bold"))
f1 <- ggplot(nat_long, aes(pct, factor(Indicator, rev(ORDER)), fill = Source)) +
  geom_col(position = position_dodge(.7), width = .6) + geom_errorbar(aes(xmin = lo, xmax = hi), position = position_dodge(.7), width = .2) +
  geom_text(aes(label = sprintf("%.1f", pct)), position = position_dodge(.7), hjust = -.25, size = 3) +
  scale_fill_manual(values = c(KDHS = colors_cema[["navy"]], KHIS = colors_cema[["orange"]]), labels = c(KDHS = "Kenya Mini KDHS", KHIS = "KHIS")) + scale_x_continuous(limits = c(0, 108)) +
  labs(title = "National coverage: Kenya Mini KDHS vs KHIS", x = "Coverage (%)", y = NULL, caption = "Error bars = KDHS 95% CI") + th
ggsave("outputs/fig_national_side_by_side.png", f1, width = 9, height = 5, dpi = 160, bg = "white")
cd <- all %>% filter(Geography != "Kenya (national)")
f2 <- ggplot(cd, aes(KDHS_pct, KHIS_pct)) + geom_abline(colour = "grey50", linetype = 2) +
  geom_point(colour = colors_cema[["blue"]], size = 1.8, alpha = .85) + geom_smooth(method = "lm", se = FALSE, colour = colors_cema[["orange"]], linewidth = .6, formula = y ~ x) +
  facet_wrap(~ Indicator, ncol = 4) + coord_equal(xlim = c(0, 105), ylim = c(0, 105)) +
  labs(title = "County agreement: KHIS vs Kenya Mini KDHS", x = "KDHS (%)", y = "KHIS (%)", caption = "Dashed line = perfect agreement") + th
ggsave("outputs/fig_county_agreement.png", f2, width = 13, height = 7, dpi = 160, bg = "white")

# ---- 5b. County maps per indicator (placed beside the county table in the workbook) ----
library(sf); library(patchwork); sf_use_s2(FALSE)
cl <- function(x) gsub("county|city|[^a-z]", "", tolower(x))
geo <- st_read("../kenya_shape_files/gadm41_KEN_1.shp", quiet = TRUE) %>% st_make_valid() %>% mutate(k = cl(NAME_1))
dir.create("outputs/maps_workbook", showWarnings = FALSE)
map_png <- setNames(character(length(ORDER)), ORDER)
for (ind in ORDER) {
  m <- geo %>% select(k) %>% left_join(cd %>% filter(Indicator == ind) %>% mutate(k = cl(Geography)), by = "k")
  lim <- range(c(m$KDHS_pct, m$KHIS_pct), na.rm = TRUE); dl <- max(abs(m$Difference_pp), na.rm = TRUE)
  base <- function(fill, ttl) ggplot(m) + geom_sf(aes(fill = .data[[fill]]), colour = "white", linewidth = .15) + labs(title = ttl) +
    theme_void(base_size = 10) + theme(plot.title = element_text(face = "bold", hjust = .5, size = 10), legend.position = "bottom", legend.key.width = unit(1.1, "cm"), legend.title = element_text(size = 8))
  seqs <- scale_fill_gradient(low = "#e3f4fb", high = colors_cema[["navy"]], limits = lim, na.value = "#bdbdbd", name = "%", guide = guide_colourbar(title.position = "left", title.vjust = .8))
  p1 <- base("KDHS_pct", "Kenya Mini KDHS 2025-26") + seqs
  p2 <- base("KHIS_pct", "KHIS") + seqs
  p3 <- base("Difference_pp", "Difference: Mini KDHS - KHIS (pp)") +
    scale_fill_gradient2(low = colors_cema[["blue"]], mid = "white", high = colors_cema[["orange"]], midpoint = 0, limits = c(-dl, dl), na.value = "#bdbdbd", name = "pp", guide = guide_colourbar(title.position = "left", title.vjust = .8))
  g <- (p1 / p2 / p3) + plot_annotation(title = ind, theme = theme(plot.title = element_text(face = "bold", size = 12)))
  map_png[[ind]] <- sprintf("outputs/maps_workbook/map_%s.png", gsub("[^A-Za-z0-9]+", "_", ind))
  ggsave(map_png[[ind]], g, width = 4.2, height = 9.6, dpi = 150, bg = "white")
}

# ---- 5c. Difference with 95% error bars and p-values ----
pfmt <- function(p) ifelse(is.na(p), "p n/a", ifelse(p < 0.001, "p<0.001", sprintf("p=%.3f", p)))
nd <- all %>% filter(Geography == "Kenya (national)") %>% mutate(Indicator = factor(Indicator, rev(ORDER)), lab = sprintf("%.1f (%.1f to %.1f), %s", Difference_pp, Diff_lower, Diff_upper, pfmt(P_value)))
f3 <- ggplot(nd, aes(Difference_pp, Indicator)) + geom_vline(xintercept = 0, colour = "grey40") +
  geom_errorbar(aes(xmin = Diff_lower, xmax = Diff_upper), width = .25, colour = colors_cema[["navy"]], orientation = "y") +
  geom_point(colour = colors_cema[["orange"]], size = 3) + geom_text(aes(label = lab), vjust = -1.1, size = 3, colour = "grey25") +
  scale_x_continuous(expand = expansion(mult = c(.3, .25))) +
  labs(title = "National difference: Kenya Mini KDHS minus KHIS (percentage points)", x = "Difference (pp); positive = KHIS lower than the survey", y = NULL,
       caption = "Error bars = 95% interval from the survey's design-based standard error; KHIS treated as a fixed count.") + th
ggsave("outputs/fig_national_difference_ci.png", f3, width = 11, height = 6, dpi = 160, bg = "white")
cdf <- all %>% filter(Geography != "Kenya (national)", !is.na(se_k)) %>% group_by(Indicator) %>% mutate(p_holm = p.adjust(P_value, "holm")) %>% ungroup() %>%
  mutate(Sig = ifelse(p_holm < 0.05, "Holm p < 0.05", "Not significant after Holm"), Indicator = factor(Indicator, ORDER)) %>% arrange(Indicator, Difference_pp) %>%
  mutate(ykey = factor(paste(Indicator, Geography, sep = "___"), levels = unique(paste(Indicator, Geography, sep = "___"))))
f4 <- ggplot(cdf, aes(Difference_pp, ykey, colour = Sig)) + geom_vline(xintercept = 0, colour = "grey40") +
  geom_errorbar(aes(xmin = Diff_lower, xmax = Diff_upper), width = 0, linewidth = .4, orientation = "y") + geom_point(size = 1.4) +
  scale_y_discrete(labels = function(v) sub(".*___", "", v)) + scale_colour_manual(values = c("Holm p < 0.05" = colors_cema[["orange"]], "Not significant after Holm" = colors_cema[["blue"]]), name = NULL) +
  facet_wrap(~ Indicator, ncol = 3, scales = "free_y") + labs(title = "County difference: Kenya Mini KDHS minus KHIS, with 95% error bars", x = "Difference (pp); positive = KHIS lower than the survey", y = NULL,
       caption = "Counties with a 100-100 survey interval are omitted (no interval). Holm adjustment across the counties of each indicator.") + th + theme(axis.text.y = element_text(size = 5))
ggsave("outputs/fig_county_difference_ci.png", f4, width = 16, height = 20, dpi = 120, bg = "white")

# ---- 6. Excel workbook ----
wb <- createWorkbook()
hs <- createStyle(fontColour = "white", fgFill = "#084887", textDecoration = "bold", halign = "center", valign = "center", wrapText = TRUE, border = "bottom")
add <- function(name, df, widths = "auto") {
  addWorksheet(wb, name); writeData(wb, name, df, headerStyle = hs); setRowHeights(wb, name, 1, 36)
  freezePane(wb, name, firstRow = TRUE, firstCol = TRUE); setColWidths(wb, name, seq_len(ncol(df)), widths = widths)
  addStyle(wb, name, createStyle(numFmt = "0.0"), rows = 2:(nrow(df) + 1), cols = which(sapply(df, is.numeric)), gridExpand = TRUE, stack = TRUE)
}
add("National side-by-side", national %>% mutate(Indicator = as.character(Indicator)), c(32, 12, 14, 12, 60, 14, 14, 14, 14, 14, 16, 16, 10, 14, 30, 24))
add("County side-by-side", county %>% mutate(Indicator = as.character(Indicator)), c(32, 18, 11, 11, 11, 10, 11, 16, 11, 11, 10, 14, 14, 30))
for (ind in ORDER) insertImage(wb, "County side-by-side", map_png[[ind]], width = 4.2, height = 9.6, units = "in",
                               startRow = which(county$Indicator == ind)[1] + 1, startCol = 16)
add("County agreement", agree %>% mutate(Indicator = as.character(Indicator), `Agreement (CCC)` = as.character(`Agreement (CCC)`)), c(32, 10, rep(14, 12), 20))
addWorksheet(wb, "Figures"); insertImage(wb, "Figures", "outputs/fig_national_side_by_side.png", width = 9, height = 5, startRow = 1, startCol = 1)
insertImage(wb, "Figures", "outputs/fig_county_agreement.png", width = 13, height = 7, startRow = 29, startCol = 1)
insertImage(wb, "Figures", "outputs/fig_national_difference_ci.png", width = 11, height = 6, startRow = 66, startCol = 1)
insertImage(wb, "Figures", "outputs/fig_county_difference_ci.png", width = 16, height = 20, startRow = 95, startCol = 1)
add("Notes", notes, c(28, 140)); addStyle(wb, "Notes", createStyle(wrapText = TRUE, valign = "top"), rows = 2:(nrow(notes) + 1), cols = 1:2, gridExpand = TRUE)
saveWorkbook(wb, "outputs/KDHS_vs_KHIS_side_by_side.xlsx", overwrite = TRUE)

print(as.data.frame(national %>% select(Indicator, `Kenya Mini KDHS 2025-26 (%)`, `KHIS used for comparison (%)`, `Difference Mini KDHS - KHIS (pp)`, Agreement)), row.names = FALSE)
print(as.data.frame(agree %>% select(Indicator, `Bias: mean KDHS - KHIS (pp)`, `Pearson r`, `Lin concordance (CCC)`, `% counties within 5 pp`, `% KHIS inside KDHS 95% CI`, `Agreement (CCC)`)), row.names = FALSE)
cat("Saved outputs/KDHS_vs_KHIS_side_by_side.xlsx\n")
