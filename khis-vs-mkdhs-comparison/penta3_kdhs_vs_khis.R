# KDHS (Direct / Area-level / Unit-level, survey-weighted) vs KHIS aggregate Penta3 coverage, 12-23 months
# KHIS coverage = Penta3 doses administered / denominator, summed over Feb 2024 - Jan 2025
#   (children 12-23 months at the survey were born ~Nov 2023-Jan 2025 and get Penta3 at ~14 weeks)
# Denominators (annual estimates pro-rated to months by days):
#   A  Estimated live births  (facility + community births, KHIS expected births)   <- primary
#   B  Under-1 population     (surviving infants, KHIS target population)
#   C  MOH 711 live births    (births recorded at facilities only)                  <- sensitivity
# Run interactively in RStudio.
library(readxl); library(dplyr); library(tidyr); library(readr); library(sf); library(ggplot2); library(patchwork)
sf_use_s2(FALSE)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/khis-vs-mkdhs-comparison")
out <- "outputs/penta3_kdhs_vs_khis/"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
colors_cema <- c(blue = "#27aae1", orange = "#fe7501", navy = "#084887", yellow = "#efca08", green = "#497048", teal = "#006d77")
norm <- function(x) gsub("[^a-z]", "", gsub("county|city", "", tolower(x)))
win <- seq(as.Date("2024-02-01"), by = "month", length.out = 12)
pdays <- function(per) { d <- as.Date(paste("1", per), "%d %B %Y"); as.numeric(difftime(seq(d, by = "month", length.out = 2)[2], d, units = "days")) /
                          ifelse(format(d, "%Y") %in% c("2024"), 366, 365) }   # fraction of the (calendar) year

# ---- KHIS numerator: Penta3 doses administered (MOH 710) by county and month ----
doses <- read_excel("penta3-administration.xls") %>% filter(grepl("County", organisationunitname)) %>%
  transmute(period = periodname, key = norm(organisationunitname), doses = `MOH710 DPT/Hep+HiB3 doses Administered`)
stopifnot(length(unique(doses$period)) == 12, length(unique(doses$key)) == 47)

# ---- Denominators ----
births <- read_excel("KHIS-NOV-2023-OCTOBER-2025 DELIVERIES.xls") %>% filter(grepl("County", organisationunitname)) %>%
  transmute(period = periodname, key = norm(organisationunitname), est_births = `Estimated live births`, moh711_births = `MOH 711 Live birth`)
u1 <- read_excel("pnt3-coverage.xls") %>% filter(grepl("County", `Organisation unit`)) %>%
  transmute(period = Period, key = norm(`Organisation unit`), under1 = Denominator)   # annual under-1 population

k <- doses %>% left_join(births, by = c("period", "key")) %>% left_join(u1, by = c("period", "key")) %>%
  mutate(f = sapply(period, pdays),
         denA = est_births * f, denB = under1 * f,
         denC = moh711_births)   # MOH 711 is already a monthly count
cty <- k %>% group_by(key) %>% summarise(doses = sum(doses), denA = sum(denA), denB = sum(denB), denC = sum(denC), .groups = "drop") %>%
  mutate(`Live births (estimated)` = 100*doses/denA, `Under-1 population` = 100*doses/denB, `MOH 711 facility births` = 100*doses/denC)
nat <- cty %>% summarise(doses = sum(doses), denA = sum(denA), denB = sum(denB), denC = sum(denC)) %>%
  transmute(key = "kenya", `Live births (estimated)` = 100*doses/denA, `Under-1 population` = 100*doses/denB, `MOH 711 facility births` = 100*doses/denC)
DEN <- c("Live births (estimated)", "Under-1 population", "MOH 711 facility births")
khis <- bind_rows(cty %>% select(key, all_of(DEN)), nat) %>% pivot_longer(all_of(DEN), names_to = "denominator", values_to = "khis") %>%
  mutate(denominator = factor(denominator, DEN))

# ---- KDHS: weighted estimates (outputs of penta3_stats_maps_plots.R) ----
cs <- read_csv("outputs/penta3_wakefield/penta3_county_statistics.csv", show_col_types = FALSE) %>%
  filter(age_grp == "12-23 months") %>% transmute(key, county, model, kdhs = mean, lo = lower, hi = upper)
ns <- read_csv("outputs/penta3_wakefield/penta3_national_statistics.csv", show_col_types = FALSE) %>%
  filter(age_grp == "12-23 months") %>% transmute(key = "kenya", county = "Kenya", model, kdhs = mean, lo = lower, hi = upper)
kd <- bind_rows(cs, ns) %>% mutate(model = factor(model, c("Direct", "Area-level", "Unit-level")))

cmp <- kd %>% inner_join(khis, by = "key", relationship = "many-to-many") %>%
  mutate(diff = khis - kdhs, khis_in_ci = !is.na(lo) & khis >= lo & khis <= hi,
         band = cut(abs(diff), c(-Inf, 5, 10, Inf), labels = c("<5 pp", "5-10 pp", "10+ pp")))
write_csv(cmp %>% mutate(across(where(is.numeric), ~ round(., 2))), paste0(out, "penta3_kdhs_vs_khis_comparison.csv"))

cat("\nNATIONAL: KHIS vs KDHS (12-23 months, Penta3)\n")
print(as.data.frame(cmp %>% filter(key == "kenya") %>% transmute(model, denominator, KDHS = round(kdhs, 1), KDHS_CI = sprintf("%.1f-%.1f", lo, hi), KHIS = round(khis, 1), diff = round(diff, 1))), row.names = FALSE)
cat("\nCOUNTY SUMMARY (KHIS - KDHS, percentage points)\n")
print(as.data.frame(cmp %>% filter(key != "kenya", !is.na(kdhs)) %>% group_by(model, denominator) %>%
  summarise(n = n(), mean_diff = round(mean(diff), 1), mean_abs = round(mean(abs(diff)), 1), r = round(cor(khis, kdhs), 2),
            pct_within_5pp = round(100*mean(abs(diff) < 5), 0), pct_in_KDHS_CI = round(100*mean(khis_in_ci), 0), .groups = "drop")), row.names = FALSE)

# ---- Plots (CEMA palette) ----
theme_cema <- theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
                                                    strip.text = element_text(face = "bold"), legend.position = "bottom")
c3 <- cmp %>% filter(key != "kenya", !is.na(kdhs))
g1 <- ggplot(c3, aes(kdhs, khis)) + geom_abline(colour = "grey50", linetype = 2) +
  geom_point(colour = colors_cema[["blue"]], size = 1.8, alpha = .85) +
  geom_smooth(method = "lm", se = FALSE, colour = colors_cema[["orange"]], linewidth = .6) +
  facet_grid(denominator ~ model) + coord_equal(xlim = c(50, 130), ylim = c(50, 130)) +
  labs(title = "KHIS vs KDHS Penta3 coverage by county, 12-23 months", x = "KDHS coverage (%)", y = "KHIS coverage (%)",
       caption = "Dashed line = agreement; orange = fitted line. KHIS can exceed 100% (doses / estimated births).") + theme_cema
ggsave(paste0(out, "scatter_khis_vs_kdhs.png"), g1, width = 11, height = 10, dpi = 170, bg = "white")

ord <- c3 %>% filter(model == "Area-level", denominator == DEN[1]) %>% arrange(kdhs) %>% pull(county)
g2 <- ggplot() +
  geom_linerange(data = c3 %>% filter(model == "Area-level", denominator == DEN[1]), aes(y = factor(county, ord), xmin = lo, xmax = hi),
                 colour = colors_cema[["navy"]], linewidth = 1.6, alpha = .35) +
  geom_point(data = c3 %>% filter(model == "Area-level"), aes(kdhs, factor(county, ord)), colour = colors_cema[["navy"]], size = 1.6) +
  geom_point(data = c3 %>% filter(model == "Area-level"), aes(khis, factor(county, ord), shape = denominator), colour = colors_cema[["orange"]], size = 2) +
  geom_vline(xintercept = 90, linetype = 2, colour = "grey40") +
  labs(title = "KHIS (orange) against the KDHS area-level 95% interval (navy), 12-23 months", x = "Penta3 coverage (%)", y = NULL, shape = "KHIS denominator") + theme_cema +
  theme(axis.text.y = element_text(size = 7))
ggsave(paste0(out, "interval_khis_vs_kdhs.png"), g2, width = 9, height = 12, dpi = 170, bg = "white")

geo <- st_read("../kenya_shape_files/gadm41_KEN_1.shp", quiet = TRUE) %>% st_make_valid() %>% mutate(key = norm(NAME_1))
m <- geo %>% select(key) %>% inner_join(c3 %>% filter(model == "Area-level"), by = "key")
g3 <- ggplot(m) + geom_sf(aes(fill = diff), colour = "white", linewidth = .15) + facet_wrap(~ denominator) +
  scale_fill_gradient2(low = colors_cema[["blue"]], mid = "white", high = colors_cema[["orange"]], midpoint = 0, name = "KHIS - KDHS (pp)", limits = c(-40, 40), oob = scales::squish) +
  labs(title = "Where KHIS is above (orange) or below (blue) the KDHS area-level estimate", caption = "12-23 months, Penta3") +
  theme_void() + theme(plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold"), legend.position = "bottom", legend.key.width = unit(1.4, "cm"))
ggsave(paste0(out, "map_khis_minus_kdhs.png"), g3, width = 13, height = 6.5, dpi = 170, bg = "white")

g4 <- ggplot(cmp %>% filter(key == "kenya"), aes(kdhs, model)) +
  geom_linerange(aes(xmin = lo, xmax = hi), colour = colors_cema[["navy"]], linewidth = 1.2) +
  geom_point(colour = colors_cema[["navy"]], size = 3) +
  geom_point(aes(khis, model, shape = denominator), colour = colors_cema[["orange"]], size = 3) +
  labs(title = "National Penta3 coverage, 12-23 months", x = "Coverage (%)", y = NULL, shape = "KHIS denominator",
       caption = "Navy = KDHS (95% interval); orange = KHIS.") + theme_cema
ggsave(paste0(out, "national_khis_vs_kdhs.png"), g4, width = 9, height = 4, dpi = 170, bg = "white")
cat("\nDone. Outputs in", out, "\n")
