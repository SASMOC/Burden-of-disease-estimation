# Penta3 coverage by age group (12-23 and 24-35 months), KDHS 2025 interim (KEBR91)
library(haven); library(dplyr); library(survey); library(readr)

kr_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/khis-vs-mkdhs-comparison/KE_2025_INTERIMDHS_11102026_131_216785/KEBR91SV/KEBR91FL.sav"
kr <- read_sav(kr_path)

d <- kr %>%
  filter(!is.na(H53)) %>%                       # children asked vaccination questions (alive, 0-35m)
  mutate(
    wt        = V005 / 1e6,
    h53       = as.numeric(H53),                # 0 No, 1 card date, 2 mother, 3 card marked, 8 DK
    penta3_all  = as.integer(h53 %in% 1:3),     # card or mother's recall
    penta3_card = as.integer(h53 %in% c(1, 3)), # card only
    age_grp   = case_when(B19 >= 12 & B19 <= 23 ~ "12-23 months",
                          B19 >= 24 & B19 <= 35 ~ "24-35 months"),
    county    = as.character(as_factor(V024))
  ) %>% filter(!is.na(age_grp))

des <- svydesign(ids = ~V021, strata = ~V022, weights = ~wt, data = d, nest = TRUE)
options(survey.lonely.psu = "adjust")

est <- function(by) {
  f <- if (is.null(by)) ~age_grp else as.formula(paste("~age_grp +", by))
  a <- svyby(~penta3_all,  f, des, svyciprop, vartype = "ci", method = "xlogit")
  b <- svyby(~penta3_card, f, des, svyciprop, vartype = "ci", method = "xlogit")
  left_join(a, b[, setdiff(names(b), c("ci_l","ci_u"))], by = all.vars(f))
}
nat <- est(NULL) %>% mutate(n_unweighted = as.integer(table(d$age_grp)[age_grp]))
cty <- est("county") %>%
  left_join(d %>% count(age_grp, county, name = "n_unweighted"), by = c("age_grp", "county"))

pct <- function(x) round(100 * x, 1)
nat_out <- nat %>% transmute(age_grp, n_unweighted,
  penta3_card_or_recall = pct(penta3_all),  lci = pct(ci_l),  uci = pct(ci_u),
  penta3_card_only      = pct(penta3_card))
cty_out <- cty %>% transmute(age_grp, county, n_unweighted,
  penta3_card_or_recall = pct(penta3_all),  lci = pct(ci_l),  uci = pct(ci_u),
  penta3_card_only      = pct(penta3_card)) %>% arrange(age_grp, county)

print(as.data.frame(nat_out)); print(as.data.frame(cty_out))
dir.create("outputs", showWarnings = FALSE)
write_csv(nat_out, "outputs/penta3_coverage_national_kdhs2025.csv")
write_csv(cty_out, "outputs/penta3_coverage_county_kdhs2025.csv")
