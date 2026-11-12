# Penta3 coverage 12-23 and 24-35 months, KMDHS 2025-26: Wakefield-group methods (SUMMER)
# Direct (design-based, logit-scale) estimates -> Fay-Herriot area-level model with BYM2 spatial smoothing
library(haven); library(dplyr); library(sf); library(SUMMER); library(readr)
sf_use_s2(FALSE); options(survey.lonely.psu = "adjust")
root <- "~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/"
kr <- read_sav(paste0(root, "khis-vs-mkdhs-comparison/KE_2025_INTERIMDHS_11102026_131_216785/KEBR91SV/KEBR91FL.sav"))

norm <- function(x) gsub("[^a-z]", "", tolower(x))
d <- kr %>% filter(!is.na(H53), B19 >= 12, B19 <= 35) %>%
  transmute(cluster = as.numeric(V001), strata = as.numeric(V022), weights = V005/1e6,
            age_grp = ifelse(B19 <= 23, "12-23 months", "24-35 months"),
            county = as.character(as_factor(V024)), key = norm(county),
            value = as.integer(H53 %in% 1:3))

geo <- st_read(paste0(root, "kenya_shape_files/gadm41_KEN_1.shp"), quiet = TRUE) %>% st_make_valid()
geo$key <- norm(geo$NAME_1)
stopifnot(all(unique(d$key) %in% geo$key), nrow(geo) == 47)
geo <- geo[match(sort(unique(geo$key)), geo$key), ]
Amat <- getAmat(as(geo, "Spatial"), names = geo$key)

# Three approaches (Wakefield group / SUMMER): Direct, Area-level (Fay-Herriot BYM2), Unit-level (cluster-level binomial BYM2)
fit <- function(a) {
  dd <- as.data.frame(d %>% filter(age_grp == a) %>% mutate(region = key))
  f <- function(...) smoothSurvey(data = dd, Amat = Amat, response.type = "binary", responseVar = "value",
        strataVar = "strata", weightVar = "weights", regionVar = "region", CI = 0.95, ...)
  ar <- f(clusterVar = "~cluster"); un <- f(clusterVar = "cluster", is.unit.level = TRUE)
  dr <- ar$direct; nm <- names(dr)
  p <- function(s, col) round(100 * s[[col]][match(geo$key, s$region)], 1)
  data.frame(age_grp = a, county = geo$NAME_1,
    n_children = as.integer(table(dd$region)[geo$key]),
    direct = p(dr, "direct.est"),
    direct_lci = round(100*plogis(dr$direct.logit.est - 1.96*sqrt(dr$direct.logit.var))[match(geo$key, dr$region)], 1),
    direct_uci = round(100*plogis(dr$direct.logit.est + 1.96*sqrt(dr$direct.logit.var))[match(geo$key, dr$region)], 1),
    area_level = p(ar$smooth, "mean"), area_lci = p(ar$smooth, "lower"), area_uci = p(ar$smooth, "upper"),
    unit_level = p(un$smooth, "mean"), unit_lci = p(un$smooth, "lower"), unit_uci = p(un$smooth, "upper"))
}
res <- bind_rows(lapply(c("12-23 months", "24-35 months"), fit))
print(as.data.frame(res))
dir.create("outputs", showWarnings = FALSE)
write_csv(res, "outputs/penta3_direct_area_unit_county_kmdhs2025.csv")
