# Add area-level covariates to Penta3 direct estimates (Admin 1 and Admin 2)
library(readr); library(dplyr)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/SAE")

c1 <- read_csv("Kenya_Admin_1_cov_template.csv", show_col_types = FALSE) |>
  rename(Region_Name = admin1.name, `4plus_anc` = `4plus_ANC`)
c2 <- read_csv("Kenya_Admin_2_cov_template1.csv", show_col_types = FALSE) |>
  rename(Region_Name_Full = admin2.name.full)

# Admin 1 (county) direct estimates
d1 <- read_csv("penta3-coverage_direct_estimates.csv", show_col_types = FALSE)
d1_cov <- left_join(d1, c1, by = "Region_Name")
write_csv(d1_cov, "penta3-coverage_direct_estimates_with_covariates.csv")

# Admin 2 (sub-county) direct estimates
d2 <- read_csv("penta3_coverage_12-23momths.csv", show_col_types = FALSE)
d2_cov <- left_join(d2, c2, by = "Region_Name_Full")
write_csv(d2_cov, "penta3_coverage_12-23momths_with_covariates.csv")

stopifnot(nrow(d1_cov) == nrow(d1), nrow(d2_cov) == nrow(d2))
cat("Admin1 missing covariates:", sum(!complete.cases(d1_cov)), "\n")
cat("Admin2 rows with missing covariates:", sum(!complete.cases(d2_cov)), "\n")
print(d2_cov[!complete.cases(d2_cov), c("Region_Name_Full", names(c2)[-1])])
