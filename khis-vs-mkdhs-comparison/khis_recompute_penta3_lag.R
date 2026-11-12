# KHIS Penta3 coverage with the denominator lagged for the schedule: dose 3 is given at 14 weeks (~3 months) after birth, so
# doses given in month t are matched to births in month t - 3.
#   Doses       : Penta3 doses administered (MOH 710), Feb 2024 - Jan 2025 (12 months)
#   Births      : the same 12-month cohort one lag earlier, Nov 2023 - Oct 2024
#   Denominators: (A) estimated live births (annual estimate pro-rated by days; facility + community) <- primary
#                 (B) MOH 711 live births (recorded at facilities)  <- sensitivity
# Run interactively in RStudio.
library(readxl); library(dplyr); library(readr)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/khis-vs-mkdhs-comparison")
# Facility-recorded MOH 711 live births scaled to all live births using Kenya Mini KDHS Table 9.7 (91.1% of live births in a health facility): total = MOH 711 / 0.911
COMM <- 1 / 0.911 - 1
cl <- function(x) gsub("county|city|[^a-z]", "", tolower(x))
q <- function(f) suppressMessages(read_excel(f))
mdate <- function(p) as.Date(paste("1", p), "%d %B %Y")
yfrac <- function(d) { n <- as.numeric(seq(d, by = "month", length.out = 2)[2] - d); n / ifelse(as.numeric(format(d, "%Y")) %% 4 == 0, 366, 365) }

doses <- q("penta3-administration.xls") %>% filter(grepl("County", organisationunitname)) %>%
  transmute(key = cl(organisationunitname), month = mdate(periodname),
            doses_moh710 = `MOH710 DPT/Hep+HiB3 doses Administered`, doses_essential = `Essential - DPT3 doses administered`)
births <- q("KHIS-NOV-2023-OCTOBER-2025 DELIVERIES.xls") %>% filter(grepl("County", organisationunitname)) %>%
  transmute(key = cl(organisationunitname), bmonth = mdate(periodname), est_annual = `Estimated live births`, moh711 = `MOH 711 Live birth`) %>%
  mutate(est_month = est_annual * sapply(bmonth, yfrac))
u1 <- q("pnt3-coverage.xls") %>% filter(grepl("County", `Organisation unit`)) %>%
  transmute(key = cl(`Organisation unit`), month = mdate(Period), under1_annual = Denominator) %>%
  mutate(u1_month = under1_annual * sapply(month, yfrac))
stopifnot(length(unique(doses$month)) == 12, min(doses$month) == as.Date("2024-02-01"))

calc <- function(lag) {
  doses %>% mutate(bmonth = as.Date(sapply(month, function(m) as.character(seq(m, by = "-1 month", length.out = lag + 1)[lag + 1])))) %>%
    left_join(births, by = c("key", "bmonth")) %>% mutate(lag = lag)
}
res <- bind_rows(lapply(c(0, 3), calc))
stopifnot(sum(is.na(res$est_month)) == 0, sum(is.na(res$moh711)) == 0)
sm <- function(x) x %>% summarise(n = sum(doses_moh710), n_ess = sum(doses_essential), est = sum(est_month), moh711 = sum(moh711) * (1 + COMM), moh711_fac = sum(moh711), .groups = "drop")
cty <- res %>% group_by(lag, key) %>% sm(); nat <- res %>% group_by(lag) %>% sm() %>% mutate(key = "kenya")
out <- bind_rows(cty, nat) %>% mutate(pct_est = 100 * n / est, pct_est_essential = 100 * n_ess / est, pct_moh711 = 100 * n / moh711)

cat("NATIONAL Penta3 (doses / births). lag 0 = same months (previous method); lag 3 = 14-week schedule\n")
print(as.data.frame(out %>% filter(key == "kenya") %>% transmute(lag_months = lag, doses = n, est_births = round(est), pct_est_births = round(pct_est, 2),
      pct_est_births_EssentialDPT3_doses = round(pct_est_essential, 2), moh711_births = moh711, pct_moh711_births = round(pct_moh711, 1))), row.names = FALSE)
cat("\nCounty range lag 3, est. births:", round(range(out$pct_est[out$lag == 3 & out$key != "kenya"]), 1), "| counties >100%:", sum(out$pct_est[out$lag == 3 & out$key != "kenya"] > 100), "\n")
cat("County correlation lag0 vs lag3:", round(cor(out$pct_est[out$lag == 0 & out$key != "kenya"], out$pct_est[out$lag == 3 & out$key != "kenya"]), 3), "\n")

# lag 3 outputs used by the workbook: MOH 711 live births (requested primary) and estimated live births (sensitivity)
l3 <- out %>% filter(lag == 3)
write_csv(bind_rows(
  l3 %>% transmute(Indicator = "Penta3, MOH 711 live births / 0.911 (sensitivity)", key, KHIS_numerator = n, KHIS_denominator = moh711, KHIS_pct = pct_moh711),
  doses %>% left_join(u1, by = c("key", "month")) %>% group_by(key) %>% summarise(n = sum(doses_moh710), den = sum(u1_month), .groups = "drop") %>%
    bind_rows(doses %>% left_join(u1, by = c("key", "month")) %>% summarise(n = sum(doses_moh710), den = sum(u1_month)) %>% mutate(key = "kenya")) %>%
    transmute(Indicator = "Penta3 (12-23 months)", key, KHIS_numerator = n, KHIS_denominator = den, KHIS_pct = 100 * n / den),
  l3 %>% transmute(Indicator = "Penta3, estimated live births (sensitivity)", key, KHIS_numerator = n, KHIS_denominator = est, KHIS_pct = pct_est)),
  "outputs/khis_recomputed_penta3_lag3.csv")
write_csv(out %>% mutate(across(where(is.numeric), ~ round(., 3))), "outputs/khis_penta3_lag_sensitivity.csv")

