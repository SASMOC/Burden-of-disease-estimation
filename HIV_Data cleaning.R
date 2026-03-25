setwd("/Documents/HIV_Analysis")
library(tidyverse)
library(lubridate)
library(scales)
library(gtsummary)
library(ggpubr)
library(sf)
library(viridis)
library(readr)
library(janitor)
library(gt)

CT_Patient <- read_csv("CT_Patient.csv")
CT_PatientBaselines <- read_csv("CT_PatientBaselines.csv")
CT_PatientStatus <- read_csv("CT_PatientStatus.csv")
CT_AdverseEvents <- read_csv("CT_AdverseEvents.csv")
CT_AllergiesChronicIllness <- read_csv("CT_AllergiesChronicIllness.csv")
CT_ARTPatients <- read_csv("CT_ARTPatients.csv")
CT_DepressionScreening <- read_csv("CT_DepressionScreening.csv")
CT_DrugAlcoholScreening <- read_csv("CT_DrugAlcoholScreening.csv")
CT_EnhancedAdherenceCounselling <- read_csv("CT_EnhancedAdherenceCounselling.csv")
CT_PatientPharmacy <- read_csv("CT_PatientPharmacy.csv")
CT_PatientVisits <- read_csv("CT_PatientVisits.csv")
CT_ViolenceScreening <- read_csv("CT_ViolenceScreening.csv")


## Cleaning datasets to remain with last exit/visit date of patient; now there are no duplicates--- saw some patients had upto 8 exit/ visit dates

CT_PatientStatus_clean <- CT_PatientStatus |>
  arrange(UniqueID, desc(ExitDate)) |>
  distinct(UniqueID, .keep_all = TRUE) 

CT_DrugAlcoholScreening_clean <- CT_DrugAlcoholScreening |>
  arrange(UniqueID, desc(VisitDate)) |>
  distinct(UniqueID, .keep_all = TRUE)

CT_DepressionScreening_clean <- CT_DepressionScreening |>
  arrange(UniqueID, desc(VisitDate)) |>
  distinct(UniqueID, .keep_all = TRUE)

CT_EnhancedAdherenceCounselling_clean <- CT_EnhancedAdherenceCounselling |>
  arrange(UniqueID, desc(VisitDate)) |>
  distinct(UniqueID, .keep_all = TRUE)

CT_AllergiesChronicIllness_clean <- CT_AllergiesChronicIllness |>
  arrange(UniqueID, desc(VisitDate)) |>
  distinct(UniqueID, .keep_all = TRUE)

CT_PatientPharmacy_clean <- CT_PatientPharmacy |>
  arrange(UniqueID, desc(DispenseDate)) |>
  distinct(UniqueID, .keep_all = TRUE)

CT_PatientVisits <- CT_PatientVisits |>
  arrange(UniqueID, desc(VisitDate)) |>
  distinct(UniqueID, .keep_all = TRUE)

CT_ViolenceScreening_clean <- CT_ViolenceScreening |>
  arrange(UniqueID, desc(VisitDate)) |>
  distinct(UniqueID, .keep_all = TRUE)

CT_ARTPatients_clean <- CT_ARTPatients |>
  arrange(UniqueID, desc(LastVisit)) |>
  distinct(UniqueID, .keep_all = TRUE)|>
  mutate(Sex= case_when(
    Sex == "F" ~ "Female",
    Sex == "M" ~ "Male",
    TRUE ~ NA_character_
  )
  )

CT_AdverseEvents_clean <- CT_AdverseEvents |>
  arrange(UniqueID, desc(VisitDate)) |>
  distinct(UniqueID, .keep_all = TRUE)

# Left joining the tree datasets, to see the outcome
new_data<- CT_Patient |>
  left_join(CT_PatientStatus_clean, by = c("UniqueID", "SiteCode"))|>
  left_join(CT_PatientBaselines, by= c("UniqueID", "SiteCode"))
  
#  Full-join, to see if more observations in the other datasets may be obtained, to enrich the dataset
new_data_2<- CT_Patient |>
  full_join(CT_PatientStatus_clean, by = c("UniqueID", "SiteCode")) |>
  full_join(CT_PatientBaselines, by= c("UniqueID", "SiteCode"))

# ids_not_common <- setdiff(
#   union(new_data$UniqueID, new_data_2$UniqueID),
#   intersect(new_data$UniqueID, new_data_2$UniqueID)
# )

merged_data<-  CT_Patient |>
  left_join(CT_PatientStatus_clean, by = c("UniqueID", "SiteCode")) |>
  left_join(CT_PatientBaselines, by= c("UniqueID", "SiteCode")) |>
  left_join(CT_AllergiesChronicIllness_clean, by= c("UniqueID", "SiteCode")) |>
  left_join(CT_PatientVisits, by= c("UniqueID", "SiteCode")) |>
  left_join(CT_ARTPatients_clean, by= c("UniqueID", "SiteCode"))|>
  select(-starts_with("...1"), -(Sex.y))


# ============================================
# DATA CLEANING AND PREPARATION
# ============================================

# Convert date columns properly
merged_data_clean <- merged_data %>%
  mutate(
    # First convert DateConfirmedHIVPositive to character to ensure consistent type
    DateConfirmedHIVPositive = as.character(DateConfirmedHIVPositive),
    
    # Now apply corrections (all character to character)
    DateConfirmedHIVPositive = case_when(
      DateConfirmedHIVPositive == "7200-07-22" ~ "2007-07-22",
      DateConfirmedHIVPositive == "5200-10-02" ~ "2005-10-02",
      DateConfirmedHIVPositive == "4201-04-2" ~ "2010-04-02",
      DateConfirmedHIVPositive == "2088-04-04" ~ "2010-04-04",
      DateConfirmedHIVPositive == "2201-01-04" ~ "2011-01-04",
      DateConfirmedHIVPositive == "3013-10-31" ~ "2013-10-31",
      DateConfirmedHIVPositive == "2207-01-20" ~ "2007-01-20",
      DateConfirmedHIVPositive == "2913-09-07" ~ "2013-09-07",
      DateConfirmedHIVPositive == "2301-05-21" ~ "2013-05-21",
      DateConfirmedHIVPositive == "2105-12-15" ~ "2015-12-15",
      DateConfirmedHIVPositive == "2060-11-07" ~ "2006-11-07",
      DateConfirmedHIVPositive == "3200-10-01" ~ "2007-10-01",
      DateConfirmedHIVPositive == "2045-02-13" ~ "2014-02-13",
      DateConfirmedHIVPositive == "3011-10-13" ~ "2011-10-13",
      DateConfirmedHIVPositive == "3005-02-15" ~ "2005-02-15",
      DateConfirmedHIVPositive == "2101-05-21" ~ "2011-05-21",
      DateConfirmedHIVPositive == "2201-05-12" ~ "2011-05-12",
      DateConfirmedHIVPositive == "2099-06-03" ~ "2009-06-03",
      DateConfirmedHIVPositive == "3010-03-15" ~ "2010-03-15",
      DateConfirmedHIVPositive == "2041-08-17" ~ "2017-08-17",
      DateConfirmedHIVPositive == "2108-05-30" ~ "2018-05-30",
      DateConfirmedHIVPositive == "2200-01-15" ~ "2007-01-15",
      DateConfirmedHIVPositive == "2048-04-23" ~ "2014-04-23",
      DateConfirmedHIVPositive == "3016-03-17" ~ "2016-03-17",
      DateConfirmedHIVPositive == "3013-11-18" ~ "2013-11-18",
      DateConfirmedHIVPositive == "2201-02-21" ~ "2011-02-21",
      DateConfirmedHIVPositive == "3201-10-03" ~ "2013-10-03",
      DateConfirmedHIVPositive == "2035-11-23" ~ "2015-11-23",
      DateConfirmedHIVPositive == "2055-03-15" ~ "2005-03-15",
      DateConfirmedHIVPositive == "2083-12-12" ~ "2013-12-12",
      DateConfirmedHIVPositive == "2201-02-27" ~ "2011-02-27",
      DateConfirmedHIVPositive == "2200-12-01" ~ "2005-12-01",
      DateConfirmedHIVPositive == "3008-03-12" ~ "2008-03-12",
      DateConfirmedHIVPositive == "2019-05-15" ~ "2019-05-15",
      DateConfirmedHIVPositive == "2035-08-30" ~ "2005-08-30",
      DateConfirmedHIVPositive == "2501-07-21" ~ "2015-07-21",
      DateConfirmedHIVPositive == "2099-09-10" ~ "2009-09-10",
      DateConfirmedHIVPositive == "4201-04-24" ~ "2010-04-24",
      DateConfirmedHIVPositive == "2919-05-15" ~ "2009-05-15",
      TRUE ~ DateConfirmedHIVPositive  # Now this is also character
    ),
    
    # Convert all date columns to Date type
    DOB = as.Date(DOB),
    DateConfirmedHIVPositive = as.Date(DateConfirmedHIVPositive),
    LastVisit = as.Date(LastVisit),
    ExitDate = as.Date(ExitDate),
    
    # Extract years
    YearDiagnosis = year(DateConfirmedHIVPositive),
    YearExit = year(ExitDate),
    YearLastVisit = year(LastVisit),
    BirthYear = year(DOB),
    
    # Clean Sex variable
    Sex = case_when(
      Sex.x %in% c("Male") ~ "Male",
      Sex.x %in% c("Female") ~ "Female",
      TRUE ~ NA_character_
    ),
    
    # Clean County (remove empty strings)
    County = ifelse(County == "" | is.na(County), "Unknown", County),
    
    # Identify deaths
    IsDead = grepl("Death", ExitReason, ignore.case = TRUE),
    
    # Exit status categories
    ExitCategory = case_when(
      ExitReason == "" | is.na(ExitReason) ~ "Active",
      grepl("Death", ExitReason, ignore.case = TRUE) ~ "Dead",
      grepl("LTFU|Lost", ExitReason, ignore.case = TRUE) ~ "LTFU",
      grepl("Transfer out", ExitReason, ignore.case = TRUE) ~ "Transferred Out",
      grepl("endpmttct", ExitReason, ignore.case = TRUE) ~ "End PMTCT",
      grepl("Completed treatment", ExitReason, ignore.case = TRUE) ~ "Completed Treatment",
      grepl("Stopped treatment", ExitReason, ignore.case = TRUE) ~ "Stopped Treatment",
      TRUE ~ "Other Exit"
    ),
    
    # Age at diagnosis
    age_at_diagnosis = case_when(
      is.na(DOB) | is.na(DateConfirmedHIVPositive) ~ NA_real_,
      TRUE ~ floor(interval(DOB, DateConfirmedHIVPositive) / years(1))
    ),
    
    # Age groups - note: use consistent dash characters!
    age_diag_group = case_when(
      age_at_diagnosis < 1 ~ "<1",
      age_at_diagnosis < 5 ~ "1-4",
      age_at_diagnosis < 15 ~ "5-14",
      age_at_diagnosis < 25 ~ "15-24",
      age_at_diagnosis < 35 ~ "25-34",
      age_at_diagnosis < 45 ~ "35-44",
      age_at_diagnosis < 55 ~ "45-54",
      age_at_diagnosis < 65 ~ "55-64",
      age_at_diagnosis >= 65 ~ "65+",
      TRUE ~ NA_character_
    ),
    
    # Factor with matching levels (use regular dash -)
    age_diag_group = factor(age_diag_group, levels = c("<1", "1-4", "5-14", "15-24",
                                                       "25-34", "35-44", "45-54",
                                                       "55-64", "65+"))
  )

# Clean the data
merged_data_clean <- merged_data_clean %>%
  mutate(
    
    # Create flag for imputed values
    DateConfirmed_Imputed = is.na(DateConfirmedHIVPositive) & 
      !is.na(DOB) & 
      !is.na(AgeEnrollment),
    
    # Impute: DOB + AgeEnrollment (in years)
    DateConfirmedHIVPositive_Clean = case_when(
      # Keep original if exists
      !is.na(DateConfirmedHIVPositive) ~ DateConfirmedHIVPositive,
      # Impute from DOB + AgeEnrollment
      !is.na(DOB) & !is.na(AgeEnrollment) ~ DOB + years(floor(AgeEnrollment)),
      # Otherwise NA
      TRUE ~ as.Date(NA)
    ),
    
    # 2. RECALCULATE YEAR OF DIAGNOSIS
    YearDiagnosis_Clean = year(DateConfirmedHIVPositive_Clean),
    
    # 3. RECALCULATE AGE AT DIAGNOSIS
    age_at_diagnosis_clean = case_when(
      !is.na(DateConfirmedHIVPositive_Clean) & !is.na(DOB) ~ 
        as.numeric(difftime(DateConfirmedHIVPositive_Clean, DOB, units = "days")) / 365.25,
      !is.na(AgeEnrollment) ~ AgeEnrollment,
      TRUE ~ NA_real_
    ),
    
    # 4. CREATE AGE AT DIAGNOSIS GROUPS
    `Age Group` = cut(
      age_at_diagnosis_clean,
      breaks = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf),
      labels = c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", 
                 "25-29", "30-34", "35-39", "40-44", "45-49", 
                 "50-54", "55-59", "60-64", "65+"),
      right = FALSE
    ),
    
    # Broader age groups
    age_diag_group_broad = cut(
      age_at_diagnosis_clean,
      breaks = c(0, 15, 25, 35, 50, 65, Inf),
      labels = c("0-14 (Children)", "15-24 (Youth)", "25-34 (Young Adults)", 
                 "35-49 (Adults)", "50-64 (Older Adults)", "65+ (Elderly)"),
      right = FALSE
    ),
    
    # =====================================================
    # 5. CURRENT AGE CALCULATIONS
    # =====================================================
    current_age = as.numeric(difftime(Sys.Date(), DOB, units = "days")) / 365.25,
    
    current_age_group = cut(
      current_age,
      breaks = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf),
      labels = c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", 
                 "25-29", "30-34", "35-39", "40-44", "45-49", 
                 "50-54", "55-59", "60-64", "65+"),
      right = FALSE
    ),
    
    # =====================================================
    # 6. STANDARDIZE OTHER VARIABLES
    # =====================================================
    # Standardize County names
    County = str_to_title(str_trim(County)),
    SubCounty = str_to_title(str_trim(SubCounty)),
    
    # Clean Sex variable
    Sex = case_when(
      Sex %in% c("Male", "M", "male", "MALE") ~ "Male",
      Sex %in% c("Female", "F", "female", "FEMALE") ~ "Female",
      TRUE ~ Sex
    ),
    
    # =====================================================
    # 7. EXIT STATUS CATEGORIES
    # =====================================================
    status = case_when(
      IsDead == TRUE ~ "Dead",
      ExitCategory == "LTFU" | ExitReason == "LTFU" ~ "LTFU",
      ExitCategory == "Transferred Out" | str_detect(ExitReason, "(?i)transfer") ~ "Transferred Out",
      ExitCategory == "Active" | is.na(ExitCategory) ~ "Active",
      TRUE ~ "Other"
    ),
    
    # =====================================================
    # 8. DURATION ON TREATMENT (Years)
    # =====================================================
    # Calculate years on treatment
    years_on_treatment = case_when(
      !is.na(LastVisit) & !is.na(DateConfirmedHIVPositive_Clean) ~
        as.numeric(difftime(LastVisit, DateConfirmedHIVPositive_Clean, units = "days")) / 365.25,
      !is.na(ExitDate) & !is.na(DateConfirmedHIVPositive_Clean) ~
        as.numeric(difftime(ExitDate, DateConfirmedHIVPositive_Clean, units = "days")) / 365.25,
      TRUE ~ NA_real_
    ),
    
    has_chronic_illness = case_when(
      is.na(ChronicIllness)| ChronicIllness %in% c("None", "NULL", "NA") ~ "No",
      TRUE ~ "Yes"
    )
  )

# ===========================================
# PRINT CLEANING SUMMARY
# ===========================================

cat("\n=== AFTER CLEANING ===\n")
cat("Total rows:", nrow(merged_data_clean), "\n\n")

cat("--- DateConfirmedHIVPositive ---\n")
cat("Original missing:", sum(is.na(merged_data_clean$DateConfirmedHIVPositive)), "\n")
cat("After imputation missing:", sum(is.na(merged_data_clean$DateConfirmedHIVPositive_Clean)), "\n")
cat("Values imputed:", sum(merged_data_clean$DateConfirmed_Imputed, na.rm = TRUE), "\n")
cat("Imputation rate:", 
    round(sum(merged_data_clean$DateConfirmed_Imputed, na.rm = TRUE) / 
            sum(is.na(merged_data_clean$DateConfirmedHIVPositive)) * 100, 1), "%\n\n")

cat("--- Age at Diagnosis ---\n")
cat("Original missing:", sum(is.na(merged_data_clean$age_at_diagnosis)), "\n")
cat("After cleaning missing:", sum(is.na(merged_data_clean$age_at_diagnosis_clean)), "\n\n")

cat("--- Year of Diagnosis ---\n")
cat("Original missing:", sum(is.na(merged_data_clean$YearDiagnosis)), "\n")
cat("After cleaning missing:", sum(is.na(merged_data_clean$YearDiagnosis_Clean)), "\n")



# Check cleaning results
cat("Data Quality Summary:\n")
cat("Total Records:", nrow(merged_data_clean), "\n")
cat("Counties:", n_distinct(merged_data_clean$County), "\n")

library(naniar)
library(DataExplorer)
library(flextable)
gg_miss_var(merged_data_clean)
plot_missing(merged_data_clean)

# Analysis ----------------------------------------------------------------

# ============================================
# SECTION 2: OVERALL SUMMARY STATISTICS
# ============================================
library(arsenal)
library(gtsummary)
t_summary2<- tableby(ExitCategory ~ Sex + `Age Group` + County + YearDiagnosis_Clean
                     + MaritalStatus + AdherenceCategory + has_chronic_illness + 
                       PatientType + years_on_treatment + AppointmentReminderWillingness,
                     data = merged_data_clean, cat.stats= "countrowpct" )
summary(t_summary2, text= TRUE)
write2word(t_summary2, "Summary table.docx",
           title = "Table 1: Summary table by Outcome")

t_sum<- tbl_summary(
  merged_data_clean,
  by= Sex,
  include = c(`Age Group`, YearDiagnosis_Clean, MaritalStatus, ExitCategory)
)

tsum<- merged_data_clean |>
  select( Sex,`Age Group`, ExitCategory)|>
  tbl_summary()
tsum
# 2.1 Total Cases, Deaths, and Active Cases
overall_summary <- merged_data_clean %>%
  summarise(
    Total_Cases = n_distinct(UniqueID),
    Total_Records = n(),
    Total_Deaths = sum(ExitReason=="DEATH", na.rm = TRUE),
    Total_Active = sum(ExitCategory == "Active", na.rm = TRUE),
    Total_LTFU = sum(ExitCategory == "LTFU", na.rm = TRUE),
    Total_Transferred = sum(ExitCategory == "Transferred Out", na.rm = TRUE),
    Total_Completed_PMTCT = sum(ExitCategory == "End PMTCT", na.rm = TRUE),
    
    # Case Fatality Rate
    Case_Fatality_Rate = round(Total_Deaths / Total_Cases * 100, 2),
    
    # Retention Rate (Active / Total)
    Retention_Rate = round(Total_Active / Total_Cases * 100, 2)
  )

print("=== OVERALL HIV BURDEN SUMMARY ===")
flextable(overall_summary)

# 2.2 Exit Reasons Distribution
exit_distribution <- merged_data_clean %>%
  count(ExitCategory) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2),
  ) %>%
  arrange(desc(n))


print("\n=== EXIT STATUS DISTRIBUTION ===")
print(exit_distribution)

# Visualization
ggplot(exit_distribution,
       aes(x = reorder(ExitCategory, n),
           y = n,
           fill = ExitCategory)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = paste0(n, " (", round(Percentage, 2), "%)")),
    hjust = -0.1,
    size = 3.5
  ) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(
    title = "Distribution of HIV Patients by Outcome Status in Kenya",
    subtitle = paste("Total Cases:", scales::comma(sum(exit_distribution$n))),
    x = "Exit Category",
    y = "Number of Cases"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size=18, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 16),
    legend.position = "none")


ggsave("exit_status_distribution.png", width = 25, height = 8, dpi = 300)

missing<- merged_data_clean|>
  filter(is.na(DateConfirmedHIVPositive_Clean))

pop_data<- read_csv("~/Downloads/Folder_C/Population per county,sex,age group.csv")

pop_2025 <- pop_data |>
  filter(year== 2025) |>
  group_by(county, gender) |>
  summarise(
    total_pop = sum(population)
  )

kenya_shapefile<- read_sf("~/Downloads/Folder_C/Kenya/gadm41_KEN_1.shp")


# ============================================
# SECTION: PLHIV in 2025
# ============================================
# Define the reference date
reference_date <- as.Date("2025-12-31")

# Calculate PLHIV as of end of 2025
plhiv_2025 <- merged_data_clean %>%
  filter(
    # Diagnosed on or before reference date
    DateConfirmedHIVPositive_Clean <= reference_date,
    
    # Exclude those who died before or on reference date
    !(ExitCategory == "Dead" & ExitDate <= reference_date)
  ) %>%
  nrow()

print(paste("PLHIV as of December 31, 2025:", plhiv_2025))


# Create a function to calculate PLHIV at any date
calculate_plhiv <- function(data, ref_date) {
  data %>%
    filter(
      DateConfirmedHIVPositive_Clean <= ref_date,
      # Not dead by reference date (NA in ExitDate means still active)
      !(ExitCategory == "Dead" & !is.na(ExitDate) & ExitDate <= ref_date)
    )
}

# Get PLHIV for 2025
plhiv_2025_data <- calculate_plhiv(merged_data_clean, as.Date("2025-12-31"))

# Summary statistics
plhiv_2025_summary <- plhiv_2025_data %>%
  summarise(
    Total_PLHIV = n(),
    Active_in_Care = sum(ExitCategory == "Active", na.rm = TRUE),
    LTFU = sum(ExitCategory == "LTFU", na.rm = TRUE),
    Transferred_Out = sum(ExitCategory == "Transferred Out", na.rm = TRUE),
    Other_Exits = sum(ExitCategory %in% c("Other Exit", "End PMTCT", 
                                          "Completed Treatment", "Stopped Treatment"), 
                      na.rm = TRUE)
  )

print(plhiv_2025_summary)

plhiv_2025_summary|>
  summarise(prev= Active_in_Care/ sum(pop_2025$total_pop)* 100)

gender_prev<- pop_data |>
  filter(year== 2025) |>
  group_by( gender) |>
  summarise(
    total_popg = sum(population)
  )


# PLHIV by Sex
plhiv_by_sex <- plhiv_2025_data %>%
  filter(ExitCategory %in% c("Active"))|>
  group_by(Sex) %>%
  summarise(PLHIV = n()) %>%
  mutate(Percentage = round(PLHIV / sum(PLHIV) * 100, 1))
plhiv_by_sex_prev<- plhiv_by_sex|>
  left_join(gender_prev, by= c("Sex"= "gender")) |>
  mutate(prevalence= PLHIV/ gender_prev$total_popg*100)

print(plhiv_by_sex)

# PLHIV by County
plhiv_by_county <- plhiv_2025_data %>%
  filter(ExitCategory == "Active" & YearDiagnosis %in% c(2022:2025))|>
  group_by(County, YearDiagnosis) %>%
  summarise(PLHIV = n()) %>%
  arrange(desc(PLHIV))
prev_county<- plhiv_by_county |>
  left_join(pop_2025_county, by= c("County"= "county")) |>
  mutate(prev= round(PLHIV/total_pop *100,2),
         County= case_when(
           County == "Elgeyo Marakwet" ~ "Elgeyo-Marakwet",
           TRUE ~ County
         ))
pop_2025_county <- pop_data |>
  filter(year== 2025) |>
  mutate(county= case_when(
    county == "Elgeyo-\nMarakwet" ~ "Elgeyo Marakwet",
    county == "Taita-Taveta" ~ "Taita Taveta",
    county == "Trans-Nzoia" ~ "Trans Nzoia",
    county == "Nairobi City" ~ "Nairobi",
    TRUE ~ county
  )) |>
  group_by(county) |>
  summarise(
    total_pop = sum(population)
  )
prev_data<- kenya_shapefile|>
  left_join(prev_county, by= c("NAME_1"= "County")) 

kenya_outline <- kenya_shapefile %>%
  mutate(geometry = st_union(geometry))

prev_data<- prev_data|>
  filter(!is.na(YearDiagnosis))

ggplot(prev_data) +
  geom_sf(data = kenya_outline, fill = "white", color= "black") +
  geom_sf(aes(fill = prev)) +
  scale_fill_viridis_c(
    na.value = "white",
    direction = -1,
    option = "rocket"
  ) +
  labs(title = "Point Prevalence of HIV in Kenya (2022–2025)") +
  theme_void() +
  facet_wrap(~ YearDiagnosis, ncol= 4)+
  theme(
    plot.title = element_text(size= 18, face= "bold", hjust= 0.5)
  )

ggplot(prev_data) +
  geom_sf(
    data = kenya_outline,
    fill = NA,
    color = "black",
    linewidth = 0.3
  ) +
  geom_sf(aes(fill = prev)) +
  scale_fill_viridis_c(
    na.value = "grey",
    direction = -1,
    option = "rocket"
  ) +
  labs(title = "Point Prevalence of HIV in Kenya (2022–2025)") +
  theme_classic() +
  coord_sf(datum = NA) +
  facet_wrap(~ YearDiagnosis)+
  theme(
    plot.title = element_text(size= 18, face= "bold", hjust= 0.5)
  )


print(plhiv_by_county)

# PLHIV by Age Group (current age in 2025)
plhiv_2025_data <- plhiv_2025_data %>%
  filter(ExitCategory == "Active")|>
  mutate(
    current_age_2025 = floor(interval(DOB, as.Date("2025-12-31")) / years(1)),
    current_age_group = case_when(
      current_age_2025 < 1 ~ "<1",
      current_age_2025 < 5 ~ "1-4",
      current_age_2025 < 10 ~ "5-9",
      current_age_2025 < 15 ~ "10-14",
      current_age_2025 < 20 ~ "15-19",
      current_age_2025 < 25 ~ "20-24",
      current_age_2025 < 30 ~ "25-29",
      current_age_2025 < 35 ~ "30-34",
      current_age_2025 < 40 ~ "35-39",
      current_age_2025 < 45 ~ "40-44",
      current_age_2025 < 50 ~ "45-49",
      current_age_2025 < 55 ~ "50-54",
      current_age_2025 < 60 ~ "55-59",
      current_age_2025 < 65 ~ "60-64",
      current_age_2025 >= 65 ~ "65+",
      TRUE ~ NA_character_
    )
  )

plhiv_by_age <- plhiv_2025_data %>%
  filter(ExitCategory == "Active")|>
  group_by(current_age_group) %>%
  summarise(PLHIV = n()) %>%
  filter(!is.na(current_age_group))

print(plhiv_by_age)

# Calculate PLHIV for each year
years <- 1990:2025

plhiv_trend <- map_df(years, function(yr) {
  ref_date <- as.Date(paste0(yr, "-12-31"))
  
  plhiv_count <- merged_data_clean %>%
    filter(
      DateConfirmedHIVPositive_Clean <= ref_date,
      !(ExitCategory == "Dead" & !is.na(ExitDate) & ExitDate <= ref_date)
    ) %>%
    nrow()
  
  tibble(Year = yr, PLHIV = plhiv_count)
})

print(plhiv_trend)

# Plot the trend
library(ggplot2)

ggplot(plhiv_trend, aes(x = Year, y = PLHIV)) +
  geom_col(fill= "steelblue", size = 1.2) +
  labs(
    title = "PLHIV Trend Over Time",
    subtitle = "People Living with HIV (Cumulative)",
    x = "Year",
    y = "Number of PLHIV"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# Create a comprehensive summary table
plhiv_cascade_2025 <- merged_data_clean %>%
  filter(DateConfirmedHIVPositive_Clean <= as.Date("2025-12-31")) %>%
  mutate(
    Status_2025 = case_when(
      ExitCategory == "Dead" & ExitDate <= as.Date("2025-12-31") ~ "Deceased",
      ExitCategory == "Active" ~ "Active in Care",
      ExitCategory == "LTFU" & ExitDate <= as.Date("2025-12-31") ~ "LTFU",
      ExitCategory == "Transferred Out" & ExitDate <= as.Date("2025-12-31") ~ "Transferred Out",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(Status_2025) %>%
  summarise(Count = n()) %>%
  mutate(
    Percentage = round(Count / sum(Count) * 100, 1),
    Is_PLHIV = ifelse(Status_2025 != "Deceased", "Yes", "No")
  ) %>%
  arrange(desc(Count))

print(plhiv_cascade_2025)
library(flextable)
flextable(plhiv_cascade_2025)

# Total PLHIV (excluding deceased)
total_plhiv <- plhiv_cascade_2025 %>%
  filter(Is_PLHIV == "Yes") %>%
  summarise(Total_PLHIV = sum(Count))

print(paste("Total PLHIV in 2025:", total_plhiv$Total_PLHIV))
print(paste("Total PLHIV in 2025:", plhiv_cascade_2025$Count[1]))

# ============================================
# SECTION: NEW INFECTIONS 2025
# ============================================

# Filter for new infections in 2025
new_infections_2025 <- merged_data_clean %>%
  filter(
    YearDiagnosis == 2025
  )

# Total new infections
total_new_2025 <- nrow(new_infections_2025)
print(paste("Total New HIV Infections in 2025:", total_new_2025))

# Summary by Gender
new_infections_by_gender <- new_infections_2025 %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex) %>%
  summarise(
    New_Infections = n()
  ) %>%
  mutate(
    Percentage = round(New_Infections / sum(New_Infections) * 100, 1)
  ) %>%
  arrange(desc(New_Infections))

print(new_infections_by_gender)

# Summary by County
new_infections_by_county <- new_infections_2025 %>%
  group_by(County) %>%
  summarise(
    New_Infections = n()
  ) %>%
  mutate(
    Percentage = round(New_Infections / sum(New_Infections) * 100, 1)
  ) %>%
  arrange(desc(New_Infections))

print(new_infections_by_county)

# Cross-tabulation: Gender x County
new_infections_gender_county <- new_infections_2025 %>%
  filter(!is.na(Sex)) %>%
  group_by(County, Sex) %>%
  summarise(
    New_Infections = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Sex,
    values_from = New_Infections,
    values_fill = 0
  ) %>%
  mutate(
    Total = Female + Male,
    Pct_Female = round(Female / Total * 100, 1),
    Pct_Male = round(Male / Total * 100, 1)
  ) %>%
  arrange(desc(Total))

print(new_infections_gender_county)

library(ggplot2)
library(scales)
library(patchwork)

# Color palette
gender_colors <- c("Female" = "#fe7501", "Male" = "#27aae1")#27aae1", "#fe7501"

# Bar chart by gender
p1 <- ggplot(new_infections_by_gender, aes(x = Sex, y = New_Infections, fill = Sex)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(New_Infections, "\n(", Percentage, "%)")), 
            vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_manual(values = gender_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "New HIV Infections in Kenya (2025) by Gender",
    subtitle = paste("Total New Infections:", total_new_2025),
    x = "Gender",
    y = "Number of New Infections"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text( size = 11),
    axis.text = element_text(size = 16),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

print(p1)
# Save plot
ggsave("new_infections_2025_by_gender.png", p1, width = 8, height = 6, dpi = 300)

# Prepare data for stacked bar
county_gender_long <- new_infections_2025 %>%
  filter(!is.na(Sex)) %>%
  group_by(County, Sex) %>%
  summarise(New_Infections = n(), .groups = "drop")

# Get top 15 counties by total infections
cases_counties <- county_gender_long %>%
  group_by(County) %>%
  summarise(Total = sum(New_Infections)) %>%
  top_n(47, Total) %>%
  pull(County)

county_gender_cases <- county_gender_long %>%
  filter(County %in% cases_counties)

# Calculate totals for ordering
county_order <- county_gender_cases %>%
  group_by(County) %>%
  summarise(Total = sum(New_Infections)) %>%
  arrange(Total) %>%
  pull(County)

county_gender_cases$County <- factor(county_gender_cases$County, levels = county_order)

p2 <- ggplot(county_gender_cases, aes(x = reorder(County, New_Infections), y = New_Infections, fill = Sex)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = New_Infections), 
            position = position_stack(vjust = 0.5), 
            size = 3, color = "white", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = gender_colors) +
  labs(
    title = "New HIV Infections in 2025 by County and Gender",
    subtitle = paste("Total New Infections:", sum(county_gender_cases$New_Infections)),
    x = "County",
    y = "Number of New Infections",
    fill = " "
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 13),
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

print(p2)
p2|p1

ggsave("new_infections_2025_county_gender_stacked.png", p2, width = 10, height = 8, dpi = 300)


# Create a formatted summary table using gt package

# Gender summary table
gender_table <- new_infections_by_gender %>%
  gt() %>%
  tab_header(
    title = md("**New HIV Infections in 2025 by Gender**"),
    subtitle = paste("Total:", total_new_2025)
  ) %>%
  cols_label(
    Sex = "Gender",
    New_Infections = "New Infections",
    Percentage = "Percentage (%)"
  ) %>%
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body(rows = Sex == "Female")
  ) %>%
  tab_style(
    style = cell_fill(color = "white"),
    locations = cells_body(rows = Sex == "Male")
  )

print(gender_table)

# Save as HTML
gtsave(gender_table, "new_infections_2025_gender_table.html")



# ============================================
# SECTION: DEATHS 2025
# ============================================

# Filter for deaths in 2025
deaths_2025 <- merged_data_clean %>%
  filter(
    ExitCategory %in% c("Dead","Transferred Out", "LTFU"),
    YearExit == 2025
  )

# Total deaths
total_deaths_2025 <- nrow(deaths_2025)
print(paste("Total HIV-Related Deaths in 2025:", total_deaths_2025))

# Summary by Gender
deaths_by_gender <- deaths_2025 %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex) %>%
  summarise(
    Deaths = n()
  ) %>%
  mutate(
    Percentage = round(Deaths / sum(Deaths) * 100, 1)
  ) %>%
  arrange(desc(Deaths))

print(deaths_by_gender)

# Summary by County
deaths_by_county <- deaths_2025 %>%
  group_by(County) %>%
  summarise(
    Deaths = n()
  ) %>%
  mutate(
    Percentage = round(Deaths / sum(Deaths) * 100, 1)
  ) %>%
  arrange(desc(Deaths))

print(deaths_by_county)

# Cross-tabulation: Gender x County
deaths_gender_county <- deaths_2025 %>%
  filter(!is.na(Sex)) %>%
  group_by(County, Sex) %>%
  summarise(
    Deaths = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Sex,
    values_from = Deaths,
    values_fill = 0
  ) %>%
  mutate(
    Total = Female + Male,
    Pct_Female = round(Female / Total * 100, 1),
    Pct_Male = round(Male / Total * 100, 1)
  ) %>%
  arrange(desc(Total))

print(deaths_gender_county)

# Calculate age at death
deaths_2025 <- deaths_2025 %>%
  mutate(
    age_at_death = floor(interval(DOB, ExitDate) / years(1)),
    age_death_group = case_when(
      age_at_death < 1 ~ "<1",
      age_at_death < 5 ~ "1-4",
      age_at_death < 10 ~ "5-9",
      age_at_death < 15 ~ "10-14",
      age_at_death < 20 ~ "15-19",
      age_at_death < 25 ~ "20-24",
      age_at_death < 30 ~ "25-29",
      age_at_death < 35 ~ "30-34",
      age_at_death < 40 ~ "35-39",
      age_at_death < 45 ~ "40-44",
      age_at_death < 50 ~ "45-49",
      age_at_death < 55 ~ "50-54",
      age_at_death < 60 ~ "55-59",
      age_at_death < 65 ~ "60-64",
      age_at_death >= 65 ~ "65+",
      TRUE ~ NA_character_
    ),
    age_death_group = factor(age_death_group, 
                             levels = c("<1", "1-4", "5-9", "10-14", "15-19",
                                        "20-24", "25-29", "30-34", "35-39", 
                                        "40-44", "45-49", "50-54", "55-59", 
                                        "60-64", "65+"))
  )

# Summary by age group
deaths_by_age <- deaths_2025 %>%
  filter(!is.na(age_death_group)) %>%
  group_by(age_death_group) %>%
  summarise(Deaths = n()) %>%
  mutate(Percentage = round(Deaths / sum(Deaths) * 100, 1))

print(deaths_by_age)

deaths_by_age_gender <- deaths_2025 %>%
  filter(!is.na(age_death_group), !is.na(Sex)) %>%
  group_by(age_death_group, Sex) %>%
  summarise(Deaths = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = Sex,
    values_from = Deaths,
    values_fill = 0
  ) %>%
  mutate(Total = Female + Male)

print(deaths_by_age_gender)


# Calculate time from diagnosis to death
deaths_2025 <- deaths_2025 %>%
  mutate(
    years_since_diagnosis = as.numeric(difftime(ExitDate, DateConfirmedHIVPositive_Clean, units = "days")) / 365.25,
    duration_category = case_when(
      years_since_diagnosis < 1 ~ "<1 year",
      years_since_diagnosis < 2 ~ "1-2 years",
      years_since_diagnosis < 5 ~ "2-5 years",
      years_since_diagnosis < 10 ~ "5-10 years",
      years_since_diagnosis >= 10 ~ "10+ years",
      TRUE ~ NA_character_
    ),
    duration_category = factor(duration_category, 
                               levels = c("<1 year", "1-2 years", "2-5 years", 
                                          "5-10 years", "10+ years"))
  )

# Summary of time since diagnosis
duration_summary <- deaths_2025 %>%
  filter(!is.na(duration_category)) %>%
  group_by(duration_category) %>%
  summarise(Deaths = n()) %>%
  mutate(Percentage = round(Deaths / sum(Deaths) * 100, 1))

print(duration_summary)

# Average years since diagnosis
avg_duration <- mean(deaths_2025$years_since_diagnosis, na.rm = TRUE)
print(paste("Average Years from Diagnosis to Death:", round(avg_duration, 1)))

# Color palette
gender_colors <- c("Female" = "#E91E63", "Male" = "#2196F3")

p1 <- ggplot(deaths_by_gender, aes(x = Sex, y = Deaths, fill = Sex)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(Deaths, "\n(", Percentage, "%)")), 
            vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_manual(values = gender_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "HIV-Related Deaths in 2025 by Gender",
    subtitle = paste("Total Deaths:", total_deaths_2025),
    x = "Gender",
    y = "Number of Deaths"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.position = "none",
    panel.grid.major.x = element_blank()
  )

print(p1)

ggsave("deaths_2025_by_gender.png", p1, width = 8, height = 6, dpi = 300)

counties_deaths <- deaths_by_county %>%
  filter(County != "Unknown")

p3 <- ggplot(counties_deaths, aes(x = reorder(County, Deaths), y = Deaths)) +
  geom_bar(stat = "identity", fill = "#D32F2F", width = 0.7) +
  geom_text(aes(label = Deaths), hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "HIV-Related Deaths in Kenya by County (2025)",
    subtitle = "Total deaths; 5936 reported from 40 counties",
    x = "County",
    y = "Number of Deaths"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 11),
    axis.text = element_text(size = 16),
    panel.grid.major.y = element_blank()
  )

print(p3)

ggsave("deaths_2025_by_county.png", p3, width = 10, height = 8, dpi = 300)

# ============================================
# SECTION 3: TEMPORAL ANALYSIS - BY YEAR
# ============================================

# 3.1 New Cases by Year
cases_by_year <- merged_data_clean %>%
  filter(!is.na(YearDiagnosis)) %>%
  group_by(YearDiagnosis) %>%
  summarise(
    New_Cases = n_distinct(UniqueID),
    .groups = 'drop'
  ) %>%
  mutate(
    Cumulative_Cases = cumsum(New_Cases),
    YoY_Change = (New_Cases - lag(New_Cases)) / lag(New_Cases) * 100
  )

print("=== NEW HIV CASES BY YEAR ===")
print(cases_by_year)

# 3.2 Deaths by Year
deaths_by_year <- merged_data_clean %>%
  filter(IsDead == TRUE, !is.na(YearExit)) %>%
  group_by(YearExit) %>%
  summarise(
    Deaths = n(),
    .groups = 'drop'
  )

deaths_by_year2 <- merged_data_clean %>%
  filter(ExitCategory %in% c("Dead", "LTFU", "Transferred Out") & !is.na(YearExit)) %>%
  group_by(YearExit) %>%
  summarise(
    Deaths = n(),
    .groups = 'drop'
  )
# 3.3 Combined Year Analysis
yearly_summary <- cases_by_year %>%
  left_join(deaths_by_year, by = c("YearDiagnosis" = "YearExit")) %>%
  mutate(
    Deaths = replace_na(Deaths, 0),
    Mortality_Rate = round(Deaths / Cumulative_Cases * 1000, 2)  # Per 1000 cases
  )

print("\n=== YEARLY SUMMARY: CASES AND DEATHS ===")
print(yearly_summary)

# Visualization - Dual axis plot
p_yearly <- ggplot(yearly_summary, aes(x = YearDiagnosis)) +
  geom_bar(aes(y = New_Cases, fill = "New Cases"), stat = "identity", alpha = 0.7) +
  geom_line(aes(y = Deaths * 10, color = "Deaths"), size = 0.3) +
  geom_point(aes(y = Deaths * 10, color = "Deaths"), size = 1 ) +
  scale_y_continuous(
    name = "New Cases",
    sec.axis = sec_axis(~./10, name = "Deaths")
  ) +
  scale_fill_manual(values = c("New Cases" = "steelblue")) +
  scale_color_manual(values = c("Deaths" = "darkred")) +
  labs(title = "HIV New Cases and Deaths in Kenya by Year (1980- 2025)",
       subtitle = "Total cases: 2,655,895 | Total Deaths: 193,019",
       x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y.left = element_text(size = 16, colour = "steelblue"),
        axis.title.y.right = element_text(size = 16, colour = "darkred"),
        axis.text.y.left = element_text(size = 16, colour = "steelblue"),
        axis.text.y.right = element_text(size = 16, colour = "darkred"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

print(p_yearly)
ggsave("yearly_cases_deaths.png", width = 12, height = 7)

# 3.4 Incidence Rate Trend
incidence_trend <- ggplot(cases_by_year, aes(x = YearDiagnosis, y = New_Cases)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.2, color = "red") +
  scale_y_continuous(labels = comma) +
  labs(title = "HIV Case Notification Trend",
       subtitle = "With LOESS smoothing trend line",
       x = "Year of Diagnosis",
       y = "Number of New Cases") +
  theme_minimal()

print(incidence_trend)
ggsave("incidence_trend.png", width = 10, height = 6)

# ============================================
# SECTION 4: GEOGRAPHIC ANALYSIS - BY COUNTY
# ============================================

# 4.1 Cases by County
cases_by_county <- merged_data_clean %>%
  filter(County != "Unknown" , YearDiagnosis == 2025) %>%
  group_by(County) %>%
  summarise(
    Total_Cases = n_distinct(UniqueID),
    Total_Deaths = sum(ExitCategory== "Dead", na.rm = TRUE),
    Active_Cases = sum(ExitCategory == "Active", na.rm = TRUE),
    LTFU_Cases = sum(ExitCategory == "LTFU", na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Case_Fatality_Rate = round(Total_Deaths / Total_Cases * 100, 2),
    Retention_Rate = round(Active_Cases / Total_Cases * 100, 2),
    LTFU_Rate = round(LTFU_Cases / Total_Cases * 100, 2)
  ) %>%
  arrange(desc(Total_Cases))

print("=== TOP 20 COUNTIES BY HIV BURDEN ===")
print(head(cases_by_county, 20))
sum(cases_by_county$Total_Cases)
sum(cases_by_county$Total_Deaths)

# Save full county data
write.csv(cases_by_county, "county_hiv_burden.csv", row.names = FALSE)

# 4.2 Top 20 Counties Visualization
counties <- head(cases_by_county)

p_counties <- ggplot(counties, aes(x = reorder(County, Total_Cases), y = Total_Cases)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = comma(Total_Cases)), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Top 20 Counties by HIV Burden",
       subtitle = "Total Confirmed Cases",
       x = "County",
       y = "Number of Cases") +
  theme_minimal()

print(p_counties)
ggsave("counties_burden.png", width = 10, height = 8)

# 4.3 County-Level Mortality Analysis
p_cfr <- ggplot(top_20_counties, aes(x = reorder(County, Case_Fatality_Rate),
                                     y = Case_Fatality_Rate)) +
  geom_bar(stat = "identity", fill = "darkred", alpha = 0.7) +
  geom_text(aes(label = paste0(Case_Fatality_Rate, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Case Fatality Rate by County (Top 20 by Burden)",
       x = "County",
       y = "Case Fatality Rate (%)") +
  theme_minimal()

print(p_cfr)
ggsave("county_cfr.png", width = 10, height = 8)

# ============================================
# SECTION 5: GENDER ANALYSIS
# ============================================

# 5.1 Overall Gender Distribution
gender_summary <- merged_data_clean %>%
  filter(!is.na(Sex)) %>%
  group_by(Sex) %>%
  summarise(
    Total_Cases = n_distinct(UniqueID),
    Total_Deaths = sum(ExitCategory %in% c("Dead", "LTFU", "Transferred Out"), na.rm = TRUE),
    Active_Cases = sum(ExitCategory == "Active", na.rm = TRUE),
    Mean_Age_Enrollment = round(mean(AgeEnrollment, na.rm = TRUE), 1),
    Median_Age_Enrollment = median(AgeEnrollment, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    Percentage = round(Total_Cases / sum(Total_Cases) * 100, 2),
    Case_Fatality_Rate = round(Total_Deaths / Total_Cases * 100, 2),
    Retention_Rate = round(Active_Cases / Total_Cases * 100, 2)
  )

print("=== HIV BURDEN BY GENDER ===")
print(gender_summary)

# 5.2 Gender Distribution
p_gender_pie <- ggplot(gender_summary, aes(x = "", y = Total_Cases, fill = Sex)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Sex, "\n", comma(Total_Cases), "\n(", Percentage, "%)")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("Female" = "#27aae1", "Male" = "#fe7501")) +
  labs(title = "HIV Cases by Gender Distribution") +
  theme_void() +
  theme(legend.position = "none")

print(p_gender_pie)
ggsave("gender_distribution.png", width = 8, height = 8)

# 5.3 Gender Trend Over Time
gender_by_year <- merged_data_clean %>%
  filter(!is.na(Sex), !is.na(YearDiagnosis)) %>%
  group_by(YearDiagnosis, Sex) %>%
  summarise(Cases = n_distinct(UniqueID), .groups = 'drop')

p_gender_trend <- ggplot(gender_by_year, aes(x = YearDiagnosis, y = Cases,
                                             color = Sex, group = Sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Female" = "#27aae1", Male = "#fe7501")) +
  scale_y_continuous(labels = comma) +
  labs(title = "HIV Case Trends by Gender",
       subtitle = "New cases diagnosed per year",
       x = "Year",
       y = "Number of Cases") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_gender_trend)
ggsave("gender_trend.png", width = 10, height = 6)

# 5.4 Gender-Age Distribution
gender_age <- merged_data_clean %>%
  filter(!is.na(Sex), !is.na(age_diag_group)) %>%
  group_by(age_diag_group, Sex) %>%
  summarise(Cases = n_distinct(UniqueID), .groups = 'drop')

p_gender_age <- ggplot(gender_age, aes(x = age_diag_group, y = Cases, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Female" = "#27aae1", "Male" = "#fe7501") ) +
  scale_y_continuous(labels = comma) +
  labs(title = "HIV Cases by Age Group and Gender",
       x = "Age Group",
       y = "Number of Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text( hjust = 1))

print(p_gender_age)
ggsave("gender_age_pyramid.png", width = 10, height = 6)


# ============================================
# SECTION 6: PREVALENCE ANALYSIS
# ============================================

# deaths over the years

deaths_nsdcc <- tibble(
  year = c(2013, 2015, 2017, 2019, 2020, 2021, 2022, 2023),
  Deaths_NSDCC = c(58446, 35821, 28214, 20997, 19486, 22373, 18473, 20480)
)

deaths_ihme <- tibble(
  year = c(2013,2014, 2015, 2016, 2017,2018, 2019, 2020),
  Deaths_IHME= c(61909.68, 59583.83, 54780.30, 49296.01, 44979.92,42671.27, 40783.69, 36496.61)
)


deaths_comparison<- deaths_by_year |>
  left_join(deaths_nsdcc, by= c("YearExit"= "year"))|>
  left_join(deaths_ihme, by= c("YearExit"="year")) |>
  filter(YearExit %in% c(2013, 2015, 2017, 2019, 2020, 2021, 2022, 2023))

flextable(deaths_comparison)
# Reshape data to long format for ggplot
deaths_long <- deaths_comparison %>%
  pivot_longer(cols = c(Deaths_ProgramData, Deaths_NSDCC, Deaths_IHME),
               names_to = "Source",
               values_to = "Deaths")

# Create line plot
ggplot(deaths_long, aes(x = YearExit, y = Deaths, color = Source)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Comparison of Deaths by Data Source",
       x = "Year",
       y = "Number of Deaths") +
  theme_minimal() +
  scale_color_manual(values = c("Deaths_ProgramData" = "blue", 
                                "Deaths_NSDCC" = "red", 
                                "Deaths_IHME" = "green"),
                     labels = c( "NSDCC", "IHME", "Program Data"))
# Base R plot
plot(deaths_comparison$YearExit, deaths_comparison$Deaths_ProgramData, 
     type = "b", col = "blue", pch = 16, ylim = range(deaths_comparison[,-1], na.rm = TRUE),
     xlab = "Year", ylab = "Deaths", main = "Deaths Comparison")
lines(deaths_comparison$YearExit, deaths_comparison$Deaths_NSDCC, type = "b", col = "red", pch = 17)
lines(deaths_comparison$YearExit, deaths_comparison$Deaths_IHME, type = "b", col = "green", pch = 18)
legend("topright", legend = c("PDATA", "NSDCC", "IHME"), 
       col = c("blue", "red", "green"), pch = c(16, 17, 18), lty = 1)
ggplot(deaths_long, aes(x = factor(YearExit), y = Deaths, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Deaths Comparison by Data Source",
       x = "Year", y = "Deaths") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(size= 16, hjust = 0.5, face = "bold"))

deaths_by_year2<- deaths_by_year2|>
  rename(d_pdata= Deaths)

deaths_comparison2<- deaths_by_year2 |>
  left_join(deaths_nsdcc, by= c("YearExit"= "year"))|>
  left_join(deaths_ihme, by= c("YearExit"="year")) |>
  filter(YearExit %in% c(2013, 2015, 2017, 2019, 2020, 2021, 2022, 2023))

# Reshape data to long format for ggplot
deaths_long2 <- deaths_comparison2 %>%
  pivot_longer(cols = c(Deaths_ProgramData, Deaths_NSDCC, Deaths_IHME),
               names_to = "Source",
               values_to = "Deaths")

# Create line plot
ggplot(deaths_long2, aes(x = YearExit, y = Deaths, color = Source)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Comparison of Deaths by Data Source",
       x = "Year",
       y = "Number of Deaths") +
  theme_minimal() +
  scale_color_manual(values = c("Deaths_ProgramData" = "blue", 
                                "Deaths_NSDCC" = "red", 
                                "Deaths_IHME" = "green"),
                     labels = c( "NSDCC", "IHME", "Program Data"))
# Base R plot
plot(deaths_comparison2$YearExit, deaths_comparison2$Deaths_ProgramData, 
     type = "b", col = "blue", pch = 16, ylim = range(deaths_comparison2[,-1], na.rm = TRUE),
     xlab = "Year", ylab = "Deaths", main = "Deaths Comparison")
lines(deaths_comparison2$YearExit, deaths_comparison2$Deaths_NSDCC, type = "b", col = "red", pch = 17)
lines(deaths_comparison2$YearExit, deaths_comparison2$Deaths_IHME, type = "b", col = "green", pch = 18)
legend("topright", legend = c("PDATA", "NSDCC", "IHME"), 
       col = c("blue", "red", "green"), pch = c(16, 17, 18), lty = 1)
ggplot(deaths_long2, aes(x = factor(YearExit), y = Deaths, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Deaths Comparison by Data Source",
       x = "Year", y = "Deaths") +
  theme_classic() +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(size= 16, hjust = 0.5, face = "bold"))

