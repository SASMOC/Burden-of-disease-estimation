# Usage (RStudio): source("run_update_bod.R")
# First run seeds the store from the cleaned tracker; later runs: set NEW to the
# new cleaned extract (same schema as Cache/dfs_df_clean.rds) and re-source.
suppressMessages({ library(tidyverse); library(data.table); library(INLA); library(sf); library(spdep) })
source("R/00_setup.R"); source("R/01_helpers.R"); source("R/04_load_reference.R")
source("R/10_auto_update_bod.R")

NEW <- if (file.exists("Cache/bod_store.rds")) NULL else readRDS("Cache/dfs_df_clean.rds")
# NEW <- readRDS("path/to/new_cleaned_extract.rds")   # <- new data goes here
update_bod(NEW)
