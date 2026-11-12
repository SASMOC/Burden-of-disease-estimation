# KHIS indicators recomputed from COUNTS with ACTUAL (recorded) denominators, 24 months Nov 2023 - Oct 2025
# (the 2 years before the survey started on 5 Nov 2025). Ratio of sums over the 24 months; no reporting-rate adjustment is
# needed because numerator and denominator come from the same MOH 711 reports.
#   PNC mother, 48 h : MOH 711 mothers receiving postpartum care within 48 h  /  Total skilled deliveries
#   PNC infant, 48 h : infants with PNC within 48 h (rebuilt = KHIS % x MOH 711 live births)  /  MOH 711 live births
#   4+ ANC           : MOH 711 pregnant women completing 4 ANC visits  /  MOH 711 live births
#   8+ ANC           : NOT computable - the exports hold only a percentage, no count of women completing 8 contacts
library(readxl); library(dplyr); library(readr)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/khis-vs-mkdhs-comparison")
# Community births: KHIS MOH 711 live births are recorded at facilities. Kenya Mini KDHS Table 9.7 (live births, 2 years before the survey):
# 91.1% in a health facility (8.6% at home, 0.3% other). Total live births = facility-recorded live births / 0.911.
COMM <- 1 / 0.911 - 1     # total births = facility births x (1 + COMM) = facility births / 0.911
cl <- function(x) gsub("county|city|[^a-z]", "", tolower(x))
quiet <- function(f) suppressMessages(read_excel(f))
cty <- quiet("ANC-PNC-deliveries-county-level-data.xls")
inf <- quiet("data-added-anc-pnc-khis aggregate.xls")
stopifnot(length(unique(cty$periodname)) == 24, length(unique(inf$periodname)) == 24,
          setequal(unique(cty$periodname), unique(inf$periodname)))

d <- inf %>% filter(grepl("County", organisationunitname)) %>%
  transmute(period = periodname, key = cl(organisationunitname), live_births = `MOH 711 Live birth`,
            anc4_n = `MOH 711 Pregnant women completing 4 ANC visits`, infant_pct = `PNC within 48hours (Infants)`) %>%
  left_join(cty %>% filter(grepl("County", organisationunitname)) %>%
              transmute(period = periodname, key = cl(organisationunitname),
                        mother_n = `MOH 711 Rev 2020_Mothers receiving Postpartum Care within 48 hours`,
                        skilled = `Total Skilled Deliveries`, mother_pct = `PNC within 48hours (Mother)`), by = c("period", "key")) %>%
  mutate(infant_n = round(infant_pct / 100 * live_births))
stopifnot(length(unique(d$key)) == 47, nrow(d) == 47 * 24)

# ---- check the assumption behind the rebuilt infant count: KHIS's own mother % = mothers / skilled deliveries? ----
chk <- d %>% filter(!is.na(mother_pct), skilled > 0) %>% mutate(calc = 100 * mother_n / skilled, gap = calc - mother_pct)
cat(sprintf("Check: KHIS 'PNC within 48hours (Mother)' vs mothers/skilled deliveries: median |gap| = %.3f pp, max |gap| = %.2f pp (n = %d county-months)\n",
            median(abs(chk$gap)), max(abs(chk$gap)), nrow(chk)))
cat("NA counts  mother_n:", sum(is.na(d$mother_n)), " skilled:", sum(is.na(d$skilled)), " live_births:", sum(is.na(d$live_births)),
    " anc4_n:", sum(is.na(d$anc4_n)), " infant_pct:", sum(is.na(d$infant_pct)), "\n")

agg <- function(df) df %>% summarise(
  pnc_mother_num = sum(mother_n, na.rm = TRUE), pnc_mother_den = sum(skilled, na.rm = TRUE),
  pnc_infant_num = sum(infant_n, na.rm = TRUE), pnc_infant_den = sum(live_births, na.rm = TRUE) * (1 + COMM),
  anc4_num = sum(anc4_n, na.rm = TRUE), anc4_den = sum(live_births, na.rm = TRUE) * (1 + COMM))
long <- function(a) a %>% tidyr::pivot_longer(-any_of("key"), names_to = c("ind", ".value"), names_pattern = "(pnc_mother|pnc_infant|anc4)_(num|den)") %>%
  mutate(khis_pct = 100 * num / den)
res <- bind_rows(d %>% group_by(key) %>% agg() %>% long(), d %>% agg() %>% mutate(key = "kenya") %>% long()) %>%
  mutate(Indicator = c(pnc_mother = "PNC for mother within 48 hours", pnc_infant = "PNC for infant within 48 hours", anc4 = "4+ ANC coverage")[ind],
         Denominator = c(pnc_mother = "Total skilled deliveries (MOH 711)", pnc_infant = "MOH 711 live births / 0.911 (all live births)", anc4 = "MOH 711 live births / 0.911 (all live births)")[ind]) %>%
  select(Indicator, key, KHIS_numerator = num, KHIS_denominator = den, KHIS_pct = khis_pct, Denominator)
write_csv(res, "outputs/khis_recomputed_pnc_anc4.csv")
cat("\nNATIONAL (24 months):\n"); print(as.data.frame(res %>% filter(key == "kenya") %>% mutate(KHIS_pct = round(KHIS_pct, 2))), row.names = FALSE)
