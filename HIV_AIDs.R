

library(tidyverse)
library(data.table)
library(mice)
library(brms)
library(bayesplot)
library(posterior)
library(loo)
library(patchwork)
library(sjPlot)
library(gt)
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)

## CEMA color scheme 
my_colors <- c(
  "#FE7501", "#27AAE1", "#76b5c5", "#063970",
  "#141", "#eab676","#FF5733", "#C70039", "#900C3F", "#581845"
)

## HIV raw data from the KHIS aggregate individuals on ART
hiv_data_set<-data.table::fread("~/Library/CloudStorage/OneDrive-Personal/BoD/easyTransfer-S2DSMnkO/PLWHIV-data.csv")

hiv_data_set1<-hiv_data_set|>
  separate(period, into=c("year","month"), sep=4)|>
  mutate(revision=case_when(
           
           grepl("_OnART",dataelementname)~"year_2023", 
           TRUE~"year_2018"),
         
         # Extract gender from pattern _(F)_ or _(M)_
         gender = str_extract(dataelementname, "(?<=\\()M|F(?=\\))"),
         
         age_category=str_extract(dataelementname,"25\\+|20-24|15-19|10-14|5-9|1-4|<1")
             )|>
  mutate(gender=str_to_lower(gender),
         countyname=gsub(" County","",countyname),
         subcountyname=gsub(" Sub County","",subcountyname),
         wardname=gsub(" Ward","",wardname))|>
  rename(facility=facilityname,
         county=countyname,
         subcounty=subcountyname,
         ward=wardname)|>
  mutate(month=factor(month, levels=c("01","02","03","04","05","06","07","08","09","10","11","12")),
         year=factor(year, levels = c(2016,2017,2018, 2019,2020,2021,2022,2023,2024,2025,2026)),
         county=as.factor(county),
         gender=as.factor(gender),
         age_category=as.factor(age_category))|>
  mutate(    month = as.integer(month),
             quarter_yr = case_when( ## Generate the quarters of the year
               month %in% c(1, 2, 3)  ~ "Q1",
               month %in% c(4, 5, 6)  ~ "Q2",
               month %in% c(7, 8, 9)  ~ "Q3",
               month %in% c(10, 11, 12) ~ "Q4"
             ),
             quarter_yr = paste0(year, "_", quarter_yr),
             month=as.factor(month)
         ) 





## Subsetting the data to get the county, subcounty, ward, facility, year, month, revision, gender, age, value

data_hiv <- hiv_data_set1 |>
  select(county, subcounty, ward, mflcode,facility, year, month,quarter_yr, revision, gender, age_category, value)|>
  filter(
    quarter_yr!="mflcode"
  )


## Add outlier checker

# dda<-data_hiv |>
#   filter(year %in% c(2018:2026)) |>
#   pivot_wider(
#     names_from = c(year, quarter_yr),   # remove month - redundant since we have quarters
#     values_from = value,
#     values_fill = 0
#   ) |>
#   rowwise() |>
#   mutate(
#     row_vals = list(c_across(where(is.numeric))),
#     Q1_val   = quantile(unlist(row_vals), 0.25, na.rm = TRUE),
#     Q3_val   = quantile(unlist(row_vals), 0.75, na.rm = TRUE),
#     IQR_val  = Q3_val - Q1_val,
#     outlier  = case_when(
#       any(unlist(row_vals) > Q3_val + 1.5 * IQR_val) ~ "Outlier",
#       any(unlist(row_vals) < Q1_val - 1.5 * IQR_val) ~ "Outlier",
#       TRUE ~ "Not Outlier"
#     )
#   ) |>
#   select(-row_vals, -Q1_val, -Q3_val, -IQR_val) |>  # clean up helper columns
#   pivot_longer(
#     cols = -c(county, subcounty, ward, facility, age_category, gender, revision, outlier),
#     values_to = "cases",
#     names_to = "quarter_year"
#   ) |>
#   ungroup()
# View(dda)

dda <- data_hiv |>
  filter(year %in% c(2018:2026), value > 0) |>        # value > 0 filter here
  pivot_wider(
    names_from  = c(year,month ,quarter_yr),
    values_from = value,
    values_fill = NA                                    # NA not 0, so zeros excluded from IQR
  )

# Identify numeric columns once
num_cols <- which(sapply(dda, is.numeric))
num_mat  <- as.matrix(dda[, num_cols])

# Row-wise quantiles (fast, C-level)
Q1      <- matrixStats::rowQuantiles(num_mat, probs = 0.25, na.rm = TRUE)
Q3      <- matrixStats::rowQuantiles(num_mat, probs = 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1

lower <- Q1 - 1.5 * IQR_val
upper <- Q3 + 1.5 * IQR_val

dda$outlier <- ifelse(
  matrixStats::rowAnys(num_mat > upper, na.rm = TRUE) |
    matrixStats::rowAnys(num_mat < lower, na.rm = TRUE),
  "Outlier", "Not Outlier"
)

# Pivot back long
dda <- dda |>
  pivot_longer(
    cols      = -c(county, subcounty, ward, facility, age_category, gender, revision, outlier),
    values_to = "cases",
    names_to  = "quarter_year"
  ) |>
  filter(!is.na(cases)) |>                             # drop NA-filled cells from pivot
  # Now flag outliers per group (facility × county × age × gender × quarter_year, month)
  group_by(county, facility, age_category, gender,quarter_year) |>
  mutate(
    group_mean = mean(cases, na.rm = TRUE),
    group_sd   = sd(cases,   na.rm = TRUE),
    group_Q1   = quantile(cases, 0.25, na.rm = TRUE),
    group_Q3   = quantile(cases, 0.75, na.rm = TRUE),
    group_IQR  = group_Q3 - group_Q1,
    outlier_group = case_when(
      cases > group_Q3 + 1.5 * group_IQR ~ "Outlier High",
      cases < group_Q1 - 1.5 * group_IQR ~ "Outlier Low",
      TRUE                                ~ "Not Outlier"
    )
  ) |>
  ungroup() |>
  select(-group_mean, -group_sd, -group_Q1, -group_Q3, -group_IQR)

dda_hiv<-dda|>
  mutate(
    quarter = case_when(
      grepl("Q1", quarter_year) ~ "Q1",
      grepl("Q2", quarter_year) ~ "Q2",
      grepl("Q3", quarter_year) ~ "Q3",
      grepl("Q4", quarter_year) ~ "Q4",
      TRUE                      ~ NA_character_
    ),
    year = str_extract(quarter_year, "^\\d{4}"),
    year = as.integer(year),
    quarter = factor(quarter, levels = c("Q1", "Q2", "Q3", "Q4")),
    month = case_when(
      grepl("_1_",  quarter_year) ~ "Jan",
      grepl("_2_",  quarter_year) ~ "Feb",
      grepl("_3_",  quarter_year) ~ "Mar",
      grepl("_4_",  quarter_year) ~ "Apr",
      grepl("_5_",  quarter_year) ~ "May",
      grepl("_6_",  quarter_year) ~ "Jun",
      grepl("_7_",  quarter_year) ~ "Jul",
      grepl("_8_",  quarter_year) ~ "Aug",
      grepl("_9_",  quarter_year) ~ "Sep",
      grepl("_10_", quarter_year) ~ "Oct",
      grepl("_11_", quarter_year) ~ "Nov",
      grepl("_12_", quarter_year) ~ "Dec",
      TRUE                        ~ NA_character_
    ),
    month = factor(month, levels = month.abb)   
  )|>
  filter(quarter_year!="mflcode")

View(dda_hiv)




## Make a line plot 
summary_datacounty<-dda_hiv |>
  filter(year > 2018) |>
  group_by(county, year, quarter) |>
  reframe(nn = sum(cases, na.rm = TRUE)) |>
  pivot_wider(
    names_from  = c(year, quarter),
    values_from = nn,
    values_fill = 0
  ) |>
  mutate(across(where(is.numeric), ~ . / 3)) |>        # divide all numeric cols by 3
  rowwise() |>
  mutate(
    row_median =round( median(c_across(where(is.numeric)), na.rm = TRUE),0),  # median across all quarters
    row_mean   = round(mean(c_across(where(is.numeric)),   na.rm = TRUE),0) , ## Mean across all quarters
    row_min    =round( min(c_across(where(is.numeric)),    na.rm = TRUE),0),
    row_max    =round(max(c_across(where(is.numeric)),    na.rm = TRUE),0)
      ) |>
  ungroup()


## WE can use the mean and median to derive the estimate for people living with HIV in each county by age agroup and gender
 


View(summary_datacounty)


dda_hiv1<-dda_hiv|>
  filter(!if_all(c(gender, age_category),is.na))|>
  mutate(adjus_cases= round(median(cases),0),
   
         .by =c(
           county ,     
           subcounty ,   
           ward ,       
           facility,   
           revision,  
           gender,       
          age_category 
         ))|>
  mutate(
    outlier=case_when((cases/adjus_cases)>1.5~"Outlier",
                      TRUE~"Not Outlier")
  )
 

View(dda_hiv1)
dda_hiv1_1 <- dda_hiv |>
  filter(!if_all(c(gender, age_category), is.na)) |>
  mutate(
    med = median(cases, na.rm = TRUE),
    mad_val = mad(cases, na.rm = TRUE),
    
    # Avoid division by zero
    modified_z = case_when(
      mad_val == 0 ~ NA_real_,
      TRUE ~ (cases - med) / mad_val
    ),
    
    .by = c(
      county, subcounty, ward, facility, revision, gender, age_category
    ),
    
    outlier = case_when(
      is.na(modified_z) ~ "Not Outlier",   # ignore undefined cases
      abs(modified_z) > 3 ~ "Outlier",
      TRUE ~ "Not Outlier"
    )
  ) |>
  ungroup()

## We can now filter facilities with more than 5 outliers
dda_hiv1_filtered <- dda_hiv1_1 |>
  group_by(county, subcounty, ward, facility, year) |>
  mutate(n_outliers = sum(outlier == "Outlier", na.rm = TRUE)) |>
  ungroup() |>
  filter(n_outliers >5& modified_z>2.5)

View(dda_hiv1_filtered)

## Get the trend  Tenwek  mission Hospital hospital
dda_hiv1|>filter(quarter=="Q4"& revision=="year_2023")|>
  group_by(county, year)|>
  reframe(nn=sum(cases),
          median_cases=sum(adjus_cases)
          )|>View()


## Data on individuals starting art each year in each hospital from KHIS. ==========================New enrolled at each Health facility 

read_single_art <- function(file) {
  
  message("Reading: ", basename(file))
  
  df <- tryCatch(
    read_xls(file),
    error = function(e) {
      warning("Failed to read: ", basename(file), " — ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  # ── Detect column name for facility ──────────────────────────
  org_col <- names(df)[str_detect(tolower(names(df)), "organisationunit")]
  
  if (length(org_col) == 0) {
    warning("No 'organisationunitname' column in: ", basename(file), " — skipping.")
    message("Columns found: ", paste(names(df), collapse = ", "))
    return(NULL)
  }
  
  # ── Detect year columns (4-digit numbers) ────────────────────
  year_cols <- names(df)[str_detect(names(df), "^\\d{4}$")]
  
  if (length(year_cols) == 0) {
    warning("No year columns found in: ", basename(file), " — skipping.")
    return(NULL)
  }
  
  message("  Years found: ", paste(year_cols, collapse = ", "))
  
  # ── Transform ─────────────────────────────────────────────────
  df |>
    filter(!if_all(3:5, is.na)) |>                      # drop fully empty rows
   dplyr:: select(all_of(c(org_col, year_cols))) |>             # keep only needed cols
    rename(facility = all_of(org_col)) |>
    mutate(facility = str_trim(facility)) |>
    filter(!is.na(facility), facility != "") |>
    pivot_longer(
      cols      = all_of(year_cols),
      names_to  = "year",
      values_to = "newly_enrolledART"
    ) |>
    mutate(
      newly_enrolledART = suppressWarnings(as.numeric(newly_enrolledART)),
      year              = as.integer(year),
      facility          = str_to_title(str_remove_all(facility, '[",*/():]'))
    ) |>
    filter(!is.na(newly_enrolledART), newly_enrolledART > 0)
}
read_and_combine_xls <- function(folder_path) {
  
  # Find all .xls files (not .xlsx)
  xls_files <- list.files(
    path       = folder_path,
    pattern    = "\\.xls$",
    full.names = TRUE,
    recursive  = TRUE
  )
  
  message(glue::glue("\nFound {length(xls_files)} .xls files in: {folder_path}"))
  
  if (length(xls_files) == 0) {
    stop("No .xls files found. Check your folder path or file extensions.")
  }
  
  # Read and transform each file
  results <- map(xls_files, read_single_art)
  
  # Drop NULLs (failed/skipped files)
  results <- compact(results)
  
  if (length(results) == 0) {
    stop("All files failed to process. Check column names and structure.")
  }
  
  # Combine all into one dataframe
  combined <- bind_rows(results)
  
  # Remove duplicates — same facility + year across files
  before <- nrow(combined)
  combined <- combined |>
    distinct(facility, year, .keep_all = TRUE) |>
    arrange(facility, year)
  after <- nrow(combined)
  
  if (before != after) {
    message(glue::glue("Removed {before - after} duplicate rows."))
  }
  
  message(glue::glue(
    "\nDone! {nrow(combined)} rows | {n_distinct(combined$facility)} facilities | ",
    "Years: {min(combined$year)}–{max(combined$year)}"
  ))
  
  return(combined)
}

# ─────────────────────────────────────────────────────────────────
# Usage
# ─────────────────────────────────────────────────────────────────

new_art <- read_and_combine_xls("~/Library/CloudStorage/OneDrive-Personal/BoD/")

glimpse(new_art)

dfs_hiv<-dda_hiv|>
  group_by(county, facility, year, quarter)|>
  summarise(total=sum(cases))|>
  filter(quarter=="Q4")|>
  
  left_join(new_art, by=c("facility","year"))

  View(dfs_hiv)
  
  ## Generate the new column by lagging the data over each year to see the consistency of new people in PLWHIV on ART
  library(purrr)
  
  dfs_hiv_lagged <- dfs_hiv %>%
    filter(year>2020)%>%
    arrange(county, facility, year) %>%
    group_by(county, facility) %>%
    mutate(
      new_enrolled = replace_na(newly_enrolledART, 0),
      Total_on_ART = accumulate(new_enrolled, 
                                .init = first(total), 
                                ~ .x + .y)
    ) %>%
    mutate(
      Total_on_ART = ifelse(year == 2018, total, Total_on_ART)
    ) %>%
    ungroup()
  
  # Test on one facility
  
  
  dfs_hiv_lagged |>
    filter(facility == "Baringo County Referral Hospital") |>
    select(year, total, newly_enrolledART, Total_on_ART)|>View()
 ## To account for the deaths we can look at the crvs or individual level data and select mortalities
  
  

  
  
  
  
  
  ## Extract the maximum reported number 


summary_datacounty |>
  dplyr::select(county,starts_with("row"), ends_with("_Q4"), -ends_with("_min|_max"))|>View()

 
## Get the trends on individual retained on art in each county over each quarter of year
data_hiv|>
  group_by(county,year,month, quarter_yr)|>
  summarise(total=sum(cases, na.rm = T))|>
  arrange(desc(total))|>
  ungroup()|>

  group_by(county,quarter_yr)|>
  summarise(totals=median(total))|>
  pivot_wider(names_from = c(quarter_yr), values_from = totals, values_fill = 0)|>
view()



### Get the distinct HIV sites in each county om ART

data_hiv |>
  group_by(county, year, quarter_yr, facility) |>
  summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop") |>
  arrange(county, facility, quarter_yr) |>  # arrange chronologically to spot trends
  ungroup()


## Get the trends on individual retained on art in each county over each quarter of year
data_hiv|>filter(county=="Nairobi"& year==2024&quarter_yr==3)|>View()

hiv_data_set1|>
  group_by(revision,year)|>
  summarise(total=sum(value, na.rm = T))|>
  ggplot(aes(x=month,y=total,group=revision,color=revision))+
  geom_line()+
  facet_wrap(~year)

hiv_data_set1 %>%
  arrange(desc(value)) %>%
  head(60) %>%
  View()
hiv_data_set1|>
  filter(
    facilityname =="Coptic Hospital"
  )|>View()

