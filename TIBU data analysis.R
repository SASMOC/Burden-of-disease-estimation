## Complete analysis on the TIBU DATA 


# Import necessary libraries
rm(list=ls())
Sys.setenv('R_MAX_VSIZE' = 16000000000)  
system("vm_stat")  # macOS
library(pacman)
p_load(
  here, readr, janitor, dplyr,  fuzzyjoin, readr, readxl, tidyr, ggplot2,sf,
  data.table,reshape2, ggplot2, furrr, arsenal, summarytools,nanair,VIM,lubridate,stringr,forcats,treemapify,zip,arrow,purrr,utils,
  epitools,boot,vroom,codigo,duckplyr,hablar,fastDummies,DT,plotly,ggrepel,scales,lme4,lmerTest,mice,Amelia,micemd,MASS,mgcv,glmmTMB,broom.mixed,performance, scale,
  magnify, tidytext,  
stargazer
)
# =========================== =========================== =========================== =========================== =========================== ===========================

## My color palette
my_colors <- c(
  "#27AAE1" ,"#FE7501", "#76b5c5", "#063970",
  "#eab676","#581845","#FF5733", "#C70039", "#900C3F"
)
# =========================== =========================== =========================== =========================== =========================== ===========================
# READING DATA FOR TIBU Kirinyaga county
# =========================== =========================== =========================== =========================== =========================== ===========================
kirinyaga_data<-read_excel("~/Library/CloudStorage/OneDrive-Personal/BoD/DSTB_Register_20260219_No_Patient_Name.xlsx")|>
  mutate(number=1)|>
  mutate(
    # Extract years: look for digits followed by Y
    Years = str_extract(`Age on Registration`, "\\d+(?=Y)"),
    Years = as.numeric(Years),
    
    # Extract months: look for digits followed by M
    Months = str_extract(`Age on Registration`, "\\d+(?=M)"),
    Months = as.numeric(Months),
    
    # Replace missing values with 0
    Years = ifelse(is.na(Years), 0, Years),
    Months = ifelse(is.na(Months), 0, Months),
    age_months=Years*12+Months,
    age_years=age_months/12,
    age_years_categorical = cut(
      age_years,
      # breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf),
      # labels = c("0-4",
      #            "5-9",
      #            "10-14",
      #            "15-19",
      #            "20-24" ,
      #            "25-29", 
      #            "30-34", 
      #            "35-39",
      #            "40-44",
      #            "45-49" , 
      #            "50-54", 
      #            "55-59" ,
      #            "60-64", 
      #            "65-69" ,
      #            "70-74" ,
      #            "75-79" , 
      #            "80+"),
      breaks = c(0,1,3, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf),
      labels = c("<1",
                 "1-2",
                 "3-4",
                 "5-9",
                 "10-14",
                 "15-19",
                 "20-24" ,
                 "25-29", 
                 "30-34", 
                 "35-39",
                 "40-44",
                 "45-49" , 
                 "50-54", 
                 "55-59" ,
                 "60-64", 
                 "65-69" ,
                 "70-74" ,
                 "75-79" , 
                 "80+"),
      right = FALSE,  # intervals are closed on the left (e.g., [0,1))
      include.lowest = TRUE)
    
    
  )

# =========================== =========================== =========================== =========================== =========================== ===========================
# 1. DATA PREPARATION
# =========================== =========================== =========================== =========================== =========================== ==========================

# Convert date columns
kirinyaga_data <- kirinyaga_data |>
  mutate(
    `Date of Registration` = as.Date(`Date of Registration`),
    `Date of treatment started` = as.Date(`Date of treatment started`),
    `Treatment Outcome Date` = as.Date(`Treatment Outcome Date`),
    Year = as.integer(Year),
    Quarter = as.factor(Quarter),
    `Age on Registration` = as.numeric(`Age on Registration`),
    `Weight(Kgs)` = as.numeric(`Weight(Kgs)`),
    BMI = as.numeric(BMI)
  )


## Get the summary of cases by health facility in the county and by age gender and age category
tibu_data<-kirinyaga_data|>
  group_by(County, age_years_categorical, Gender=`Sex M/F`,`Treatment Outcome`,`Type of TB P/EP`,Year, `Health Facility`)|>
  reframe(Cases=sum(number))|>
  ungroup()|>
  mutate(
    AgeGroup=case_when(
      age_years_categorical=="<1"~"0 - 1",
      age_years_categorical%in%c("1-2","3-4")~"1-4",
      age_years_categorical%in%c("80-84","85-89",  "90-94",    "95+")~"80+",
      TRUE~age_years_categorical
    ),
    Gender=case_when(Gender=="F"~"female",
                     Gender=="M"~"male",
                     TRUE~Gender),
    outcome=case_when(
      `Treatment Outcome` %in% c("Cured (C)",
                                 "Treatment Completed (TC)",
                                 "Lost to follow-up (LTFU)",
                                 "Moved to Category 4 (MT4)",
                                 "Still on Treatment (NC)",
                                 "Transferred Out (TO)",
                                 "Treatment Failed (F)"
                                 ) ~ "Morbidity",
      `Treatment Outcome`=="Died (D)"~"Mortality",
      TRUE ~ "Other/Unknown"
    ),
    

      
      `Health Facility`=gsub("Sub-District","Sub-County",`Health Facility`),
      `Health Facility`=case_when(
        `Health Facility`=="Kerugoya District Hospital"~"Kerugoya County Refferal Hospital",
        `Health Facility`=="Mt. Kenya (ACK) Hospital"~"Mt Kenya (Ack) Hospital",
        `Health Facility`== "Sagana Sub-County Hospital" ~"Sagana Subcounty Hospital" ,
        `Health Facility`=="Nucleaur Medical Diagnostic Center" ~ "Nucleaur Medical Diagnostic Centre" ,
        `Health Facility`=="Kagio Catholic Dispensary (Mary Immucate Catholic Dispensary)"~"Kagio Hospital",
        `Health Facility`=="Mary Immaculate Catholic Dispensary Makutano" ~ "Mary Immaculate Kerugoya Hospital" ,
        TRUE~`Health Facility`
   
    )
  )|>
  group_by(
    County,Year, `Health Facility`, Gender, AgeGroup, outcome, Disease=`Type of TB P/EP` 
  )|>reframe(
    cases=sum(Cases, na.rm = T)
  )|>
  pivot_wider(names_from = outcome, values_from = cases, values_fill = 0)|>
 rename(Cases=Morbidity,
        Deaths=Mortality
  )|>
  mutate(CFR=round(Deaths/(Deaths+Cases),3))
 ## Population level data for computing the prevalence when needed 
population <- readRDS("~/Library/CloudStorage/OneDrive-Personal/BoD/population data.rds")

# ── Clean population data ─────────────────────────────────────
pop_data <- population |>
  rename(
    County     = county,
    Year       = year,
    Gender     = gender,
    AgeGroup   = Age,
    Population = population
  ) |>
  mutate(Year = as.numeric(Year))

## Combine TIBU data with the population data 

tibu_data1<-tibu_data|>
  left_join(pop_data,
            by=c("County", "Year", "Gender", "AgeGroup"))


## Import the facility data so that we can combine with the TIBU to get the KEPH LEVELs
library(janitor
        )
khfa_data<-read_csv("/Volumes/D/Immunization_estimations/Inputs/khfa_data.csv")|>
  
  dplyr::select(kmfl,county,sub_county,keph_level,facility) |>
  
  mutate( facility=str_to_title(facility),
          kmfl=gsub("n_|n","",kmfl))|>
  clean_names()|>
  
  mutate(across(where(is.character), str_to_title))|>
  mutate(county = case_when(
    county == "Elgeyo-Marakwet" ~ "Elgeyo Marakwet",
    county == "Muranga" ~ "Murang'a",
    county == "Thara-Nithi" ~ "Tharaka Nithi",
    TRUE ~ county
  )) 

## Select County, facility amd keph level
haf_fac<-khfa_data|>
 dplyr:: select(
    county,keph_level,facility, kmfl
  )

tibu_data2<-tibu_data1|>
  left_join(haf_fac,
            by=c("County"="county", "Health Facility"="facility"))|>
  mutate(
    keph_level=case_when(
      `Health Facility`%in%c(
        "Sagana Subcounty Hospital",
        "Kianyaga Sub-County Hospital",
         "Silvercrest Medcare Hospital",
          "Kianyaga Sub-County Hospital",
        "Kianyaga Sub-County Hospital"
                  )~"Level 4",
     `Health Facility`%in% c( "Rukanga Dispensary","Kiangai Dispensary","Thiba Dispensary",
                       "Njegas Dispensary","Kutus Dispensary",'Kibirigwi Dispensary',
                       "Kerugoya Catholic Dispensary.","Karumandi Dispensary","Kagumo Dispensary",
                       "Gathambi Dispensary","Ciagini Dispensary","Whiterose Medical Centre","Nguka Dispensary",
                       "Kibirigwi Dispensary","Ciagini Dispensary","Kagumo Dispensary","Karumandi Dispensary","Medlife Health Care"
                       )~"Level 3",
      
      `Health Facility`%in%c("Queens and Kings Accurate Health Centre","Nucleaur Medical Diagnostic Centre",
                      "Muchagara Dispensary","Kiangombe Dispensary","Kagio Nursing Home","Joshua Memorial Mbai Dispensary",
                      "Gatwe Dispensary","GK Prison Dispensary (Kerugoya)","agumo Live Giving Dispensary/Laboratory","Gatumbi (SDA) Dispensary",
                      "GK Prison Dispensary (Gathigiriri )","Mbiri (ACK) Dispensary","Thigirichi Mukui","Jesmond (ACK) Dispensary (Mburi)","Kiandangae Dispensary",
                      "Rurii Kiandegwa Dispensary","Thigirichi Mukui","Kiandai Dispensary","Mukangu Dispensary","Vital Fort Medical Center"
                      )~"Level 2",
      
      
      TRUE~keph_level
    )
    
  )|>
  separate(
    keph_level, into=c("Level","FacilityLevel", sep =" "))|>
  dplyr::select(-c(Level, kmfl, 
                   " "))|>
  mutate(Source="TIBU")
  saveRDS(tibu_data2,"~/Library/CloudStorage/OneDrive-Personal/BoD/tibu-data.rds")

# =========================== =========================== =========================== =========================== =========================== ===========================
# 2. DESCRIPTIVE STATISTICS
# =========================== =========================== =========================== =========================== =========================== ===========================


# Get the gender distribution by year and age ======== Plot
kirinyaga_age<-kirinyaga_data|>
  group_by(Year,`Sex M/F`, age_years_categorical  )|>
  reframe(nn=n())|>
  ungroup()
## Convert the data to a negative values for male and positive for females
kirinyaga_pyramid <- kirinyaga_age |>
  mutate(
    sex = case_when(
      `Sex M/F` %in% c("Male", "M", "MALE") ~ "Male",
      `Sex M/F` %in% c("Female", "F", "FEMALE") ~ "Female"
    ),
    nn1 = ifelse(sex == "Male",nn,-nn),
    sex = factor(sex, levels = c("Male", "Female"))
  )

# Check distribution
table(kirinyaga_pyramid$sex)


## Plot


ggplot(kirinyaga_pyramid, 
       aes(x = age_years_categorical, y = nn1, fill = sex)) +
  geom_col(position = "identity", width = 0.9) +   # <-- important fix
  coord_flip() +                                   # horizontal pyramid
  scale_y_continuous(
    breaks = seq(-max(abs(kirinyaga_pyramid$nn1)), max(abs(kirinyaga_pyramid$nn1)), by = 50),
    labels = abs
  ) +
  scale_fill_manual(values = c("Male" = "#27aae1", "Female" = "#fe7501")) +
  facet_wrap(~ Year) +
  labs(
    title = "Age–Sex Distribution of TB Cases in Kirinyaga County",
    # subtitle = "Faceted by Year",
    x = "Age Group ",
    y = "Number of TB Cases",
    fill = "Gender",
    
    caption = "Data source: TIBU Program"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    strip.text = element_text(face = "bold", size = 16),
    legend.position = "bottom",
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size=16, face = "bold"),
    axis.text.x = element_text(size=16),
    axis.text.y = element_text(size = 16),
    
  )
ggsave("~/OneDrive/TIBU output/kirinyaga_tb_age_distribution.png", width = 10, height = 13)


kirinyaga_summary <- kirinyaga_age |>
  group_by(Year) |>
  summarise(Total_Cases = sum(nn), .groups = "drop")

stargazer(kirinyaga_summary,
          type = "html",                     # or "latex" for APA-style output
          summary = FALSE,
          title = "Table 1\nSummary of Tuberculosis Cases in Kirinyaga County by Year",
          digits = 0,
          align = TRUE,
          out = "kirinyaga_summary_APA.html",
          notes = "Note. Data aggregated from TIBU and individual-level datasets, 2022–2025."
)

  

# Overall summary

cat("=== KIRINYAGA TB DATA SUMMARY ===\n")
cat("Total TB cases:", nrow(kirinyaga_data), "\n")
cat("Date range:", min(kirinyaga_data$`Date of Registration`, na.rm = TRUE), 
    "to", max(kirinyaga_data$`Date of Registration`, na.rm = TRUE), "\n\n")

# Cases by year
yearly_summary <- kirinyaga_data |>
  group_by(Year) |>
  summarise(
    Cases = n(),
    .groups = "drop"
  )
print(yearly_summary)
# Mortality summary
mortality_summary <- kirinyaga_data |>
  filter(`Treatment Outcome` == "Died (D)") |>
  group_by(Year) |>
  summarise(
    `Died (D)` = n(),
    .groups = "drop"
  )
## GET THE MORTALITIES

plot_data <- yearly_summary |>
  left_join(mortality_summary, by = "Year") |>
  pivot_longer(cols = c("Cases", "Died (D)"), 
               names_to = "Category", 
               values_to = "Count") |>
  mutate(Category = ifelse(Category == "Cases", "Total Cases", "Deaths"))
## Visualize mortalities vs all cases
ggplot(plot_data, aes(x = factor(Year), y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "identity", alpha = 0.9) +
  scale_fill_manual(values = c("Total Cases" = "#27aae1", "Deaths" = "#fe7501")) +
  labs(title = "TB Cases and Mortalities in Kirinyaga by Year",
       x = "Year",
       y = "Number of Cases",
       fill = "",) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.title = element_text(size = 16,"Outcome"),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 16, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    legend.position = "right",
  
    legend.background = element_rect(fill = "lightgrey", color = "black")
  )

ggsave("~/OneDrive/TIBU output/kirinyaga_cases_deaths.png", width = 12, height = 6)

# =========================== =========================== =========================== =========================== =========================== ===========================
# 3. DEMOGRAPHIC ANALYSIS
# =========================== =========================== =========================== =========================== =========================== ===========================

## 3.1 Age and Sex Distribution
demographics <- kirinyaga_data |>
  group_by(`Sex M/F`, age_years_categorical) |>
  summarise(Count = n(), .groups = "drop") |>
  filter(!is.na(`Sex M/F`), !is.na(age_years_categorical))

# Age-Sex Pyramid
ggplot(demographics, aes(x = age_years_categorical, y = ifelse(`Sex M/F` == "M", Count, -Count), 
                         fill = `Sex M/F`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c( "F" = "#FE7501","M" = "#27AAE1")) +
  labs(title = "TB Cases by Age and Sex - Kirinyaga",
       x = "Age Category",
       y = "Number of Cases",
       fill = "Gender") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
ggsave("~/OneDrive/kirinyaga_age_sex_pyramid.png", width = 10, height = 8)

## 3.2 Age Statistics
age_stats <- kirinyaga_data |>
  filter(!is.na(`Age on Registration`)) |>
  summarise(
    Mean_Age = mean(`Age on Registration`, na.rm = TRUE),
    Median_Age = median(`Age on Registration`, na.rm = TRUE),
    SD_Age = sd(`Age on Registration`, na.rm = TRUE),
    Min_Age = min(`Age on Registration`, na.rm = TRUE),
    Max_Age = max(`Age on Registration`, na.rm = TRUE)
  )
print(age_stats)

# =========================== ===========================  =========================== =========================== =========================== ===========================
# 4. TB TYPE ANALYSIS
# =========================== =========================== =========================== =========================== =========================== ===========================

## 4.1 Pulmonary vs Extra-Pulmonary
tb_type <- kirinyaga_data |>
  group_by(Year, `Type of TB P/EP`) |>
  summarise(Cases = n(), .groups = "drop") |>
  filter(!is.na(`Type of TB P/EP`))

ggplot(tb_type, aes(x = factor(Year), y = Cases, fill = `Type of TB P/EP`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = " Temporal Distribution of Tuberculosis TB - Kirinyaga 2021-2026",
       x = "Year",
       y = "Number of Cases",
       fill = "TB Type") +
  theme_minimal() +
  scale_fill_manual(values = my_colors)+
  geom_text(aes(label = Cases), position = position_dodge(width = 0.9), vjust = -0.5)+
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16, face = "bold")
  )

ggsave("~/OneDrive/TIBU output/kirinyaga_tb_type.png", width = 10, height = 6)

## 4.2 Patient Type (New, Relapse, etc.)
patient_type <- kirinyaga_data |>
  group_by(Year, `Type of patient`) |>
  summarise(Cases = n(), .groups = "drop") |>
  filter(!is.na(`Type of patient`))

ggplot(patient_type, aes(x = factor(Year), y = Cases, fill = `Type of patient`)) +
  geom_bar(stat = "identity") +
  labs(title = "TB Patient Classification - Kirinyaga",
       x = "Year",
       y = "Number of Cases",
       fill = "Patient Type") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16, face = "bold"),
    legend.position = "right")+
  scale_fill_manual(values = my_colors
                    )

ggsave("~/OneDrive/TIBU output/kirinyaga_patient_type.png", width = 10, height = 6)

# ===========================
# 5. TREATMENT OUTCOMES ANALYSIS
# ===========================

## 5.1 Overall Outcomes
outcomes_summary <- kirinyaga_data |>
  filter(!is.na(`Treatment Outcome`)) |>
  group_by(`Treatment Outcome`) |>
  summarise(Count = n()) |>
  mutate(Percentage = round(Count / sum(Count) * 100, 1)) |>
  arrange(desc(Count))

print(outcomes_summary)

## 5.2 Outcomes by Year
outcomes_yearly <- kirinyaga_data |>
  filter(!is.na(`Treatment Outcome`), !is.na(Year)) |>
  group_by(Year, `Treatment Outcome`) |>
  summarise(Count = n(), .groups = "drop")

ggplot(outcomes_yearly, aes(x = factor(Year), y = Count, fill = `Treatment Outcome`)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "TB Treatment Outcomes Over Time - Kirinyaga",
       subtitle = "Proportion of outcomes by year",
       x = "Year",
       y = "Percentage",
       fill = "Treatment Outcome") +
  theme_minimal() +
  scale_fill_manual(values = my_colors)+
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16, face = "bold"),
    legend.position = "right")


ggsave("~/OneDrive/TIBU output/kirinyaga_outcomes_proportion.png", width = 12, height = 6)

## 5.3 Treatment Success Rate
success_rate <- kirinyaga_data |>
  filter(!is.na(`Treatment Outcome`), Year < 2026) |>  # Exclude incomplete 2025
  mutate(
    Success = ifelse(`Treatment Outcome` %in% c("Cured (C)", "Treatment Completed (TC)"), 
                     "Successful", "Unsuccessful")
  ) |>
  group_by(Year, Success) |>
  summarise(Count = n(), .groups = "drop") |>
  group_by(Year) |>
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

ggplot(success_rate, aes(x = factor(Year), y = Percentage, fill = Success)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Successful" = "#27aae1", "Unsuccessful" = "#fe7501")) +
  geom_text(aes(label = paste0(Percentage, "%"), size = 16), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 16) +
  labs(title = "TB Treatment Success Rate - Kirinyaga (2022-2024)",
       x = "Year",
       y = "Percentage",
       fill = "Outcome") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )

ggsave("~/OneDrive/TIBU output/kirinyaga_success_rate.png", width = 10, height = 6)

# ===========================
# 6. HIV-TB CO-INFECTION
# ===========================

hiv_tb <- kirinyaga_data |>
  filter(!is.na(`HIV Testing Result`)) |>
  group_by(Year, `HIV Testing Result`) |>
  summarise(Cases = n(), .groups = "drop") |>
  group_by(Year) |>
  mutate(Percentage = round(Cases / sum(Cases) * 100, 1))

ggplot(hiv_tb, aes(x = factor(Year), y = Cases, fill = `HIV Testing Result`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(Cases, "\n(", Percentage, "%)")), 
            position = position_dodge(width = 0.9), vjust = -0.1, size = 6) +
  labs(title = "HIV Testing Results Among TB Patients - Kirinyaga",
       x = "Year",
       y = "Number of Cases",
       fill = "HIV Status") +
  theme_minimal()+
  scale_fill_manual(values = my_colors)+
  theme(
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )

ggsave("~/OneDrive/TIBU output/kirinyaga_hiv_tb.png", width = 10, height = 6)

## 6.1 ART Coverage among HIV+ TB patients
art_coverage <- kirinyaga_data |>
  filter(`HIV Testing Result` == "Positive") |>
  group_by(Year, `ART Y/N`) |>
  summarise(Count = n(), .groups = "drop") |>
  group_by(Year) |>
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

print("ART Coverage among HIV+ TB Patients:")
print(art_coverage)

# ===========================
# 7. DIAGNOSTIC METHODS
# ===========================

## 7.1 GeneXpert Results
genexpert <- kirinyaga_data |>
  filter(!is.na(`Gene Expert`)) |>
  group_by(Year, `Gene Expert`) |>
  summarise(Cases = n(), .groups = "drop")

ggplot(genexpert, aes(x = factor(Year), y = Cases, fill = `Gene Expert`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "GeneXpert Results - Kirinyaga",
       x = "Year",
       y = "Number of Cases",
       fill = "GeneXpert Result") +
  theme_minimal()+
  scale_fill_manual(values = my_colors)

ggsave("~/OneDrive/TIBU output/kirinyaga_genexpert.png", width = 10, height = 6)

## 7.2 Drug Resistance (Rifampicin)
rifampicin <- kirinyaga_data |>
  filter(!is.na(Rifampicin)) |>
  group_by(Year, Rifampicin) |>
  summarise(Cases = n(), .groups = "drop")

print("Rifampicin Resistance:")
print(rifampicin)

# Result	Meaning	and Clinical  interpretation
# Sensitive	The Mycobacterium tuberculosis organism is susceptible to Rifampicin.	Rifampicin will be effective as part of standard first‑line TB treatment (usually RHZE: Rifampicin, Isoniazid, Pyrazinamide, Ethambutol).
# Resistant	The organism shows resistance to Rifampicin.	Indicates Rifampicin‑resistant TB (RR‑TB), often a marker of Multidrug‑Resistant TB (MDR‑TB); requires a second‑line TB regimen and specialist management.

# ===========================
# 8. GEOGRAPHIC DISTRIBUTION
# ===========================

## 8.1 Cases by Sub-County
subcounty_dist <- kirinyaga_data |>
  group_by(`Sub County`, Year) |>
  summarise(Cases = n(), .groups = "drop") |>
  filter(!is.na(`Sub County`))

ggplot(subcounty_dist, aes(x = reorder(`Sub County`, Cases), y = Cases, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "TB Cases by Sub-County - Kirinyaga",
       x = "Sub-County",
       y = "Number of Cases",
       fill = "Year") +
  theme_minimal()+
theme(
  axis.title.x = element_text(size = 16, face = "bold"),
  axis.title.y = element_text(size = 16, face = "bold"),
  axis.text.x = element_text(size = 16),
  axis.text.y = element_text(size = 16)
)+
  scale_alpha_manual(values = my_colors)

ggsave("~/OneDrive/TIBU output/kirinyaga_subcounty.png", width = 10, height = 8)

## 8.2 Cases by Health Facility
top_facilities <- kirinyaga_data |>
  group_by(`Health Facility`) |>
  summarise(Cases = n()) |>
  arrange(desc(Cases)) |>
  head(15)

ggplot(top_facilities, aes(x = reorder(`Health Facility`, Cases), y = Cases)) +
  geom_bar(stat = "identity", fill = "#27aae1") +
  coord_flip() +
  geom_text(aes(label = Cases), hjust = -0.3, size=6) +
  labs(title = "Top 15 Health Facilities by TB Cases - Kirinyaga",
       x = "Health Facility",
       y = "Number of Cases") +
 theme(
    title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 16, hjust = 0.5),
    axis.text.y = element_text(size = 16)
  )+
  scale_fill_manual(values = my_colors)
 

ggsave("~/OneDrive/TIBU output/kirinyaga_facilities.png", width = 10, height =12)

# ===========================
# 9. QUARTERLY TRENDS
# ===========================

quarterly_trends <- kirinyaga_data |>
  filter(!is.na(Quarter)) |>
  group_by(Year, Quarter) |>
  summarise(Cases = n(), .groups = "drop")

ggplot(quarterly_trends, aes(x = Quarter, y = Cases, group = Year, color = factor(Year))) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Quarterly TB Case Trends - Kirinyaga",
       x = "Quarter",
       y = "Number of Cases",
       color = "Year") +
  theme_minimal()+
  theme(
    title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 16, hjust = 0.5),
    axis.text.y = element_text(size = 16),
    legend.title = element_text(size = 16,"Year"),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    
    legend.background = element_rect(fill = "lightgrey", color = "black")
  )+
  scale_color_manual(values = my_colors)

ggsave("~/OneDrive/TIBU output/kirinyaga_quarterly_trends.png", width = 10, height = 6)



## Generate the day when the treatment started 

kirinyaga_data<-kirinyaga_data|>
  separate(
    `Date of treatment started`, into = c("year","month","day"), sep = "-"
  )|>mutate(date = as.Date(paste(year, month, day, sep = "-")))
kirinyaga_data <- kirinyaga_data |>
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) |>
  arrange(date) |>
  mutate(index = row_number())|>
relocate(index, .before = 4)
# ===========================
# 10. SPUTUM CONVERSION
# ===========================

sputum_conversion <- kirinyaga_data |>
  filter(`Type of TB P/EP` == "Pulmonary TB"& `0th Month Result` %in%c ( "Pos","ND")) |>  # Only pulmonary cases
  dplyr::select(`0th Month Result`, `2/3rd Month Result`, `5th Month Result`, `6/8th Month Result`) |>
  summarise(
    Month_0_Positive = sum(`0th Month Result` == "Pos", na.rm = TRUE),
    Month_2_3_Negative = sum(`2/3rd Month Result` == "Neg", na.rm = TRUE),
    Month_5_Negative = sum(`5th Month Result` == "Neg", na.rm = TRUE),
    Month_6_8_Negative = sum(`6/8th Month Result` == "Neg", na.rm = TRUE)
  )

print("Sputum Conversion Rates:")
print(sputum_conversion)
## Visualize sputum conversion
sputum_long <- sputum_conversion |>
  pivot_longer(cols = everything(), names_to = "Month", values_to = "Count")
ggplot(sputum_long, aes(x = Month, y = Count, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(title = "Sputum Conversion Among Pulmonary TB Patients - Kirinyaga",
       x = "Month",
       y = "Number of Patients") +
  theme_minimal() +
  theme(legend.position = "none",
    
        title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(size = 16, hjust = 0.5),
        axis.text.y = element_text(size = 16)
  )+
  scale_fill_manual(values = my_colors)


sputum_analysis<-kirinyaga_data |>
  filter(`Type of TB P/EP` == "Pulmonary TB") |>
  mutate(
    # Standardize results (handle variations in data entry)
    Month_0 = case_when(
      grepl("Pos|\\+", `0th Month Result`, ignore.case = TRUE) ~ "Positive",
      grepl("Neg|\\-", `0th Month Result`, ignore.case = TRUE) ~ "Negative",
      TRUE ~ "Not Done/Unknown"
    ),
    Month_2_3 = case_when(
      grepl("Pos|\\+", `2/3rd Month Result`, ignore.case = TRUE) ~ "Positive",
      grepl("Neg|\\-", `2/3rd Month Result`, ignore.case = TRUE) ~ "Negative",
      TRUE ~ "Not Done/Unknown"
    ),
    Month_5 = case_when(
      grepl("Pos|\\+", `5th Month Result`, ignore.case = TRUE) ~ "Positive",
      grepl("Neg|\\-", `5th Month Result`, ignore.case = TRUE) ~ "Negative",
      TRUE ~ "Not Done/Unknown"
    ),
    Month_6_8 = case_when(
      grepl("Pos|\\+", `6/8th Month Result`, ignore.case = TRUE) ~ "Positive",
      grepl("Neg|\\-", `6/8th Month Result`, ignore.case = TRUE) ~ "Negative",
      TRUE ~ "Not Done/Unknown"
    )
  )

# ===========================
# 2. COHORT DEFINITION
# ===========================

# Define eligible cohort: Pulmonary TB with positive baseline sputum
eligible_cohort <- sputum_analysis |>
  filter(Month_0 == "Positive")  # Started with positive sputum

cat("Total pulmonary TB cases:", nrow(sputum_analysis), "\n")
cat("Sputum-positive cases (eligible for conversion analysis):", nrow(eligible_cohort), "\n\n")

# ===========================
# 3. TESTING COMPLETION RATES
# ===========================

testing_completion <- eligible_cohort |>
  summarise(
    Total = n(),
    Month_0_Done = sum(Month_0 != "Not Done/Unknown"),
    Month_2_3_Done = sum(Month_2_3 != "Not Done/Unknown"),
    Month_5_Done = sum(Month_5 != "Not Done/Unknown"),
    Month_6_8_Done = sum(Month_6_8 != "Not Done/Unknown")
  ) |>
  mutate(
    Pct_Month_2_3 = round(Month_2_3_Done / Total * 100, 1),
    Pct_Month_5 = round(Month_5_Done / Total * 100, 1),
    Pct_Month_6_8 = round(Month_6_8_Done / Total * 100, 1)
  )

cat("=== SPUTUM TESTING COMPLETION RATES ===\n")
print(testing_completion)

# ====================================================================================================================================================================================
# 4. CONVERSION ANALYSIS
# ===========================
# Positive sputum = TB bacteria detected in sputum (patient is infectious)
# Negative sputum = No TB bacteria detected (patient is less/not infectious)
# Conversion = Going from positive → negative
# 
# 
# Clinical Significance
# 1. Treatment effectiveness
# 
# Shows anti-TB drugs are killing the bacteria
# Expected timeline: Most patients convert by 2-3 months of treatment
# 
# 2. Infectiousness
# 
# Patients become much less contagious after conversion
# Reduces TB transmission to others
# 
# 3. Prognosis
# Early conversion (by month 2-3) = better treatment outcomes
# Delayed conversion = higher risk of treatment failure or relapse

# Monitoring Schedule
# Standard sputum testing timeline for pulmonary TB:
# Time Point Purpose:
# Month 0 (Diagnosis) Baseline - should be positive
# Month 2-3 Check for conversion & By end of 2 months: 80-85% of drug-sensitive TB patients should convert where by at the  end of 3 months: 90-95% should have convert
# Month 5 Monitor continued negativity 
# End of treatment (6-8 months) Confirm cure

# Failure to convert by month 3: Suggests possible:
 
# - Drug resistance
# - Poor adherence to treatment
# - Extensive disease
# - Need for extended treatment
# ==================================================================================================================================================================================== ==================================
# Method A: Among those who ACTUALLY GOT TESTED (denominator = tested)
conversion_tested <- eligible_cohort |>
  summarise(
    # Month 2-3
    Tested_Month_2_3 = sum(Month_2_3 %in% c("Positive", "Negative")),
    Converted_Month_2_3 = sum(Month_2_3 == "Negative", na.rm = TRUE),
    Rate_Month_2_3 = round(Converted_Month_2_3 / Tested_Month_2_3 * 100, 1),
    
    # Month 5
    Tested_Month_5 = sum(Month_5 %in% c("Positive", "Negative")),
    Converted_Month_5 = sum(Month_5 == "Negative", na.rm = TRUE),
    Rate_Month_5 = round(Converted_Month_5 / Tested_Month_5 * 100, 1),
    
    # Month 6-8
    Tested_Month_6_8 = sum(Month_6_8 %in% c("Positive", "Negative")),
    Converted_Month_6_8 = sum(Month_6_8 == "Negative", na.rm = TRUE),
    Rate_Month_6_8 = round(Converted_Month_6_8 / Tested_Month_6_8 * 100, 1)
  )

cat("\n=== CONVERSION RATES (Among those tested) ===\n")
print(conversion_tested)

# Method B: Among ALL eligible patients (denominator = all sputum-positive at baseline)
# This gives a more conservative "program-level" conversion rate
conversion_all <- eligible_cohort |>
  summarise(
    Total_Eligible = n(),
    
    # Month 2-3
    Converted_Month_2_3 = sum(Month_2_3 == "Negative", na.rm = TRUE),
    Program_Rate_Month_2_3 = round(Converted_Month_2_3 / Total_Eligible * 100, 1),
    
    # Month 5
    Converted_Month_5 = sum(Month_5 == "Negative", na.rm = TRUE),
    Program_Rate_Month_5 = round(Converted_Month_5 / Total_Eligible * 100, 1),
    
    # Month 6-8
    Converted_Month_6_8 = sum(Month_6_8 == "Negative", na.rm = TRUE),
    Program_Rate_Month_6_8 = round(Converted_Month_6_8 / Total_Eligible * 100, 1)
  )

cat("\n=== PROGRAM-LEVEL CONVERSION RATES (Among all eligible) ===\n")
print(conversion_all)

# ==================================================================================================
# 5. CONVERSION BY YEAR
# ==================================================================================================

conversion_by_year <- eligible_cohort |>
  group_by(Year) |>
  summarise(
    Total = n(),
    Tested_Month_2_3 = sum(Month_2_3 %in% c("Positive", "Negative")),
    Converted_Month_2_3 = sum(Month_2_3 == "Negative", na.rm = TRUE),
    Conversion_Rate = round(Converted_Month_2_3 / Tested_Month_2_3 * 100, 1),
    .groups = "drop"
  )

cat("\n=== CONVERSION RATES BY YEAR (Month 2-3) ===\n")
print(conversion_by_year)

# ===============================================================================================
# 6. VISUALIZATIONS
# ===============================================================================================

# Plot 1: Conversion rates over time
conversion_trend <- eligible_cohort |>
  
  # mutate(number=1)|>
  dplyr::select(Month_0, Month_2_3, Month_5, Month_6_8) |>
  pivot_longer(cols = everything(), names_to = "Time_Point", values_to = "Result") |>
  
  mutate(outcome=case_when(
    Result=="Positive"~1,
    Result=="Negative"~1,
    TRUE~1
  )) |>
  filter(Result %in% c("Positive", "Negative")) |>
  group_by(Time_Point, Result) |>
  summarise(Count = n(), .groups = "drop") |>
  group_by(Time_Point) |>
  mutate(
    Percentage = round(Count / sum(Count) * 100, 1),
    Time_Point = factor(Time_Point, 
                        levels = c("Month_0", "Month_2_3", "Month_5", "Month_6_8"),
                        labels = c("Baseline", "Month 2-3", "Month 5", "Month 6-8"))
  )

ggplot(conversion_trend, aes(x = Time_Point, y = Percentage, fill = Result)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Percentage, "%\n(n=", Count, ")")), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 6) +
  scale_fill_manual(values = c("Positive" = "#fe7501", "Negative" = "#27aae1")) +
  labs(title = "Sputum Conversion Over Treatment Period - Kirinyaga",
       subtitle = "Among pulmonary TB patients with positive baseline sputum",
       x = "Follow-up Time Point",
       y = "Percentage",
       fill = "Sputum Result") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

ggsave("~/OneDrive/TIBU output/sputum_conversion_timeline.png", width = 12, height = 6)

# Plot 2: Conversion by year
ggplot(conversion_by_year|>
         filter(Year!=2026), aes(x = factor(Year), y = Conversion_Rate)) +
  geom_bar(stat = "identity", fill = "#27aae1") +
  geom_text(aes(label = paste0(Conversion_Rate, "%"
                               # "\n(", Converted_Month_2_3, "/", Tested_Month_2_3, ")"
                               )
                               ),
            vjust = -0.5, size = 7) +
  geom_hline(yintercept = 85, linetype = "dashed", color = "#fe7501", linewidth = 1) +
  
  # ⬇ Force the correct annotate function from ggplot2
  ggplot2::annotate("text",
                    x = Inf,
                    y = 85,
                    label = "WHO Target: 85%",
                    hjust = 2.0,
                    color = "black",
                    size=9) +
  
  labs(title = "Sputum Conversion Rate by Year (Month 2-3)",
       # subtitle = "Among pulmonary TB patients tested at month 2-3",
       x = "Year",
       y = "Conversion Rate (%)") +
  ylim(0, 100) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    
  )

ggsave("~/OneDrive/TIBU output/sputum_conversion_by_year.png", width = 10, height = 6)

## Selection of key insights:




## What good conversion rates indicate:
# 
# -Effective treatment regimen
# -Good drug quality
# -Patient adherence to medications
# -Drug-sensitive TB (not resistant)


# =========================== =====================================================================================================
# 7. FACTORS AFFECTING CONVERSION
# =================================================================================================================================

# Conversion by HIV status
conversion_hiv <- eligible_cohort |>
  filter(Month_2_3 %in% c("Positive", "Negative"),
         !is.na(`HIV Testing Result`)) |>
  group_by(`HIV Testing Result`) |>
  summarise(
    Tested = n(),
    Converted = sum(Month_2_3 == "Negative"),
    Conversion_Rate = round(Converted / Tested * 100, 1)
  )

cat("\n=== CONVERSION BY HIV STATUS ===\n")
print(conversion_hiv)



# Visualize 
ggplot(conversion_hiv, aes(x = `HIV Testing Result`, y = Conversion_Rate, fill = `HIV Testing Result`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Conversion_Rate, "%\n(n=", Converted, "/", Tested, ")")),
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Positive"="#fe7501","Negative"="#27aae1","Declined"="#76b5c5")) +
  labs(title = "Sputum Conversion Rate by HIV Status (Month 2-3)",
       x = "HIV Status",
       y = "Conversion Rate (%)") +
  ylim(0, 100) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.position = "none"
  )


# Conversion by patient type
conversion_patient_type <- eligible_cohort |>
  filter(Month_2_3 %in% c("Positive", "Negative"),
         !is.na(`Type of patient`)) |>
  group_by(`Type of patient`) |>
  summarise(
    Tested = n(),
    Converted = sum(Month_2_3 == "Negative"),
    Conversion_Rate = round(Converted / Tested * 100, 1)
  ) |>
  arrange(desc(Conversion_Rate))

# Visualize sorted descending order
ggplot(conversion_patient_type, 
       aes(x = reorder(`Type of patient`, Conversion_Rate), 
           y = Conversion_Rate)) +
  
  geom_bar(stat = "identity", fill = "#27aae1") +
  
  # WHO recommendation line at 85%
  geom_hline(yintercept = 85, 
             linetype = "dashed", 
             color = "#fe7501", 
             linewidth = 1) +

  
  geom_text(
    aes(label = paste0(Conversion_Rate, "%\n(", Converted, "/", Tested, ")")),
    hjust = -0.1,
    size = 4
  ) +
  
  labs(title = "Sputum Conversion Rate by Patient Type (Month 2–3)",
       x = "Patient Type",
       y = "Conversion Rate (%)") +
  
  scale_y_continuous(limits = c(0, 110), expand = c(0, 0)) +
  coord_flip() +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.position = "none"
  ) +
  
  # WHO recommendation text
  ggplot2:: annotate("text",
                     x = 6.5, y = 85,
                     label = "WHO recommendation: ≥85%",
                     vjust = -0.02, hjust = 0.5,
                     color = "#fe7501",
                     size = 5,
                     fontface = "italic") 



ggsave("~/OneDrive/TIBU output/sputum_conversion_by_patient_type.png", width = 12, height = 6)


cat("\n=== CONVERSION BY PATIENT TYPE ===\n")
print(conversion_patient_type)


## Generate the odds ratio of conversion by HIV status and patient type using logistic regression
library(broom)

# Logistic regression model
logistic_model <- glm(I(Month_2_3 == "Negative") ~ `HIV Testing Result` + `Type of patient`+Zone+age_years_categorical+`Sex M/F`,
                      data = eligible_cohort# |> filter(Month_2_3 %in% c("Positive", "Negative"),
                                                       # !is.na(`HIV Testing Result`),
                                                       # !is.na(`Type of patient`))
,
                      family = binomial)


# Extract odds ratios and confidence intervals
odds_ratios <- tidy(logistic_model, exponentiate = TRUE, conf.int = TRUE)|>
  mutate(term=gsub(
    "`HIV Testing Result`","HIV Status: ",term),
    term=gsub("`Type of patient`","Patient Type:  ",term
  ),
  term=gsub("age_years_categorical","Age Group:  ",term),
  
            term=gsub("Zone","Location:  ",term),
  )
View(odds_ratios)
cat("\n=== LOGISTIC REGRESSION: ODDS RATIOS FOR SPUTUM CONVERSION ===\n")
print(odds_ratios)
## Plot the odds ratios
ggplot(odds_ratios |> filter(term != "(Intercept)"), 
       aes(y = term, x = estimate)) +
  geom_point(size = 4, color = "#27aae1") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "#27aae1") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Odds Ratios for Sputum Conversion (Month 2-3)",
       y = "Predictor",
       x = "Odds Ratio (log scale)") +
  scale_x_log10() +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16)
  )
# ===========================
# 8. DELAYED CONVERSION ANALYSIS
# ===========================

delayed_conversion <- eligible_cohort |>
  mutate(
    Conversion_Status = case_when(
      Month_2_3 == "Negative" ~ "Converted by Month 2-3",
      Month_2_3 == "Positive" & Month_5 == "Negative" ~ "Delayed conversion (Month 5)",
      Month_2_3 == "Positive" & Month_5 == "Positive" & Month_6_8 == "Negative" ~ "Very delayed (Month 6-8)",
      Month_2_3 == "Positive" & Month_6_8 == "Positive" ~ "Never converted",
      TRUE ~ "Unknown/Not tested"
    )
  ) |>
  group_by(Conversion_Status) |>
  summarise(Count = n()) |>
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

cat("\n=== CONVERSION TIMING SUMMARY ===\n")
print(delayed_conversion)

# Visualize conversion timing
ggplot(delayed_conversion, aes(x = reorder(Conversion_Status, -Count), y = Count, fill = Conversion_Status)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Count, " (", Percentage, "%)")), vjust = -0.5, size = 5) +
  labs(title = "Timing of Sputum Conversion Among Pulmonary TB Patients - Kirinyaga",
       x = "Conversion Status",
       y = "Number of Patients") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.position = "none"
  )+
  scale_fill_manual(values = my_colors)

# =========================== =========================== ========================== ================= =========================== ===========================
# 11. NUTRITIONAL STATUS
# ===========================

bmi_summary <- kirinyaga_data |>
  filter(!is.na(BMI), BMI > 10, BMI < 50) |>  # Filter outliers
  summarise(
    Mean_BMI = mean(BMI, na.rm = TRUE),
    Median_BMI = median(BMI, na.rm = TRUE),
    Below_18.5 = sum(BMI < 18.5, na.rm = TRUE),
    Normal = sum(BMI >= 18.5 & BMI < 25, na.rm = TRUE),
    Overweight = sum(BMI >= 25, na.rm = TRUE)
  )

print("BMI Distribution:")
print(bmi_summary)

ggplot(kirinyaga_data |> filter(!is.na(BMI), BMI > 10, BMI < 50), 
       aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "#27aae1", color = "white") +
  geom_vline(xintercept = 18.5, color = "#fe7501", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 18.5, y = 100, label = "Underweight threshold", 
           angle = 90, vjust = -0.5, color = "red",size =5) +
  labs(title = "BMI Distribution Among TB Patients - Kirinyaga",
       x = "BMI (kg/m²)",
       y = "Number of Patients") +
  theme_minimal()+
  theme(
    title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
   
   
    axis.text.x = element_text(size = 16, hjust = 0.5),
    axis.text.y = element_text(size = 16)
  )

ggsave("~/OneDrive/TIBU output/kirinyaga_bmi_distribution.png", width = 10, height = 6)

# ===========================
# 12. COMPREHENSIVE SUMMARY TABLE
# ===========================

comprehensive_summary <- kirinyaga_data |>
  # filter(Year < 2025) |>  # Exclude incomplete 2025
  group_by(Year) |>
  summarise(
    Total_Cases = n(),
    Male = sum(`Sex M/F` == "M", na.rm = TRUE),
    Female = sum(`Sex M/F` == "F", na.rm = TRUE),
    Pulmonary = sum(`Type of TB P/EP` == "P", na.rm = TRUE),
    Extra_Pulmonary = sum(`Type of TB P/EP` == "EP", na.rm = TRUE),
    HIV_Positive = sum(`HIV Testing Result` == "Positive", na.rm = TRUE),
    Cured = sum(`Treatment Outcome` == "Cured (C)", na.rm = TRUE),
    Completed = sum(`Treatment Outcome` == "Treatment Completed (TC)", na.rm = TRUE),
    Died = sum(`Treatment Outcome` == "Died (D)", na.rm = TRUE),
    LTFU = sum(`Treatment Outcome` == "Lost to follow-up (LTFU)", na.rm = TRUE),
    Success_Rate = round((Cured + Completed) / Total_Cases * 100, 1)
  )

print("=== COMPREHENSIVE ANNUAL SUMMARY ===")
print(comprehensive_summary)



# Save to CSV
write.csv(comprehensive_summary, "~/OneDrive/TIBU output/kirinyaga_comprehensive_summary.csv", row.names = FALSE)

# ===========================
# 13. KEY PERFORMANCE INDICATORS
# ===========================

cat("\n=== KEY PERFORMANCE INDICATORS (2022-2024) ===\n")

kpi <- kirinyaga_data |>
  filter(Year %in% c(2022, 2023, 2024,2025)) |>
  summarise(
    `Treatment Success Rate` = round(
      sum(`Treatment Outcome` %in% c("Cured (C)", "Treatment Completed (TC)"), na.rm = TRUE) /
        sum(!is.na(`Treatment Outcome`)) * 100, 1
    ),
    `Death Rate` = round(
      sum(`Treatment Outcome` == "Died (D)", na.rm = TRUE) /
        sum(!is.na(`Treatment Outcome`)) * 100, 1
    ),
    `LTFU Rate` = round(
      sum(`Treatment Outcome` == "Lost to follow-up (LTFU)", na.rm = TRUE) /
        sum(!is.na(`Treatment Outcome`)) * 100, 1
    ),
    `HIV Coinfection Rate` = round(
      sum(`HIV Testing Result` == "Positive", na.rm = TRUE) /
        sum(!is.na(`HIV Testing Result`)) * 100, 1
    ),
    
    
    `Treatment Failure Rate`=round(
      sum(`Treatment Outcome` %in%c( "Treatment Failed (F)","Moved to Category 4 (MT4)"), na.rm = TRUE) /
        sum(!is.na(`Treatment Outcome`)) * 100, 1
    )
    # ,
    # `Pulmonary TB Proportion` = round(
    #   sum(`Type of TB P/EP` == "P", na.rm = TRUE) /
    #     sum(!is.na(`Type of TB P/EP`)) * 100, 1
    # )
  )

print(kpi)
## plot the KPIs
kpi_long <- kpi |>
  pivot_longer(cols = everything(), names_to = "KPI", values_to = "Value")
ggplot(kpi_long, aes(x = KPI, y = Value, fill = KPI)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(Value, "%")), vjust = -0.01, size = 6) +
  labs(
    # title = "Key Performance Indicators - Kirinyaga TB Program (2022-2025)",
       x = "Performance Indicator",
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = my_colors)+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16, face = "bold"),
                              # axis.text.x = element_text(size = 16, angle = 0, hjust = 1),
                              axis.text.y = element_text(size = 16),
                              
                              )+
  
  coord_flip()
ggsave("~/OneDrive/TIBU output/kirinyaga_kpis.png", width = 12, height = 10)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("All visualizations and summary files saved to OneDrive\n")
# ===========================


## Show the distribution of TB cases in Kirinyaga county from 2022 to 2025
## How many facilities are reporting TB cases in Kirinyaga county and split by  sub county

# ===========================
# 1. NUMBER OF FACILITIES REPORTING BY SUB-COUNTY
# ===========================


# 1. Find facilities appearing in multiple sub-counties
facilities_multiple_subcounties <- kirinyaga_data |>
  filter(!is.na(`Sub County`), !is.na(`Health Facility`)) |>
  group_by(`Health Facility`) |>
  summarise(
    Number_of_SubCounties = n_distinct(`Sub County`),
    SubCounties = paste(unique(`Sub County`), collapse = " | "),
    Total_Cases = n(),
    .groups = "drop"
  ) |>
  filter(Number_of_SubCounties > 1) |>
  arrange(desc(Number_of_SubCounties), desc(Total_Cases))

cat("=== FACILITIES APPEARING IN MULTIPLE SUB-COUNTIES ===\n")
print(facilities_multiple_subcounties)
cat("\nNumber of facilities in multiple sub-counties:", nrow(facilities_multiple_subcounties), "\n")

# 2. Calculate how many "extra" counts we have
extra_counts <- facilities_multiple_subcounties |>
  summarise(
    Facilities_Counted_Multiple = n(),
    Extra_Counts = sum(Number_of_SubCounties - 1)
  )

cat("\nExtra counts due to overlap:", extra_counts$Extra_Counts, "\n")
cat("Expected: 126 - ", extra_counts$Extra_counts, " = ", 126 - extra_counts$Extra_Counts, "\n\n")

# 3. Check for data quality issues - look at specific examples
sample_duplicates <- kirinyaga_data |>
  filter(`Health Facility` %in% head(facilities_multiple_subcounties$`Health Facility`, 5)) |>
 dplyr:: select(`Health Facility`, `Sub County`, Zone, Year) |>
  arrange(`Health Facility`, `Sub County`)

cat("=== SAMPLE OF FACILITIES IN MULTIPLE SUB-COUNTIES ===\n")
print(sample_duplicates |> distinct())

# ===========================
# CORRECTED ANALYSIS
# ===========================

# Option 1: Assign each facility to its PRIMARY sub-county (most cases)
facilities_primary_subcounty <- kirinyaga_data |>
  filter(!is.na(`Sub County`), !is.na(`Health Facility`)) |>
  group_by(`Health Facility`, `Sub County`) |>
  summarise(Cases_in_SubCounty = n(), .groups = "drop") |>
  group_by(`Health Facility`) |>
  slice_max(Cases_in_SubCounty, n = 1, with_ties = FALSE) |>
  ungroup()

# Now count unique facilities per sub-county
facilities_by_subcounty_corrected <- facilities_primary_subcounty |>
  group_by(`Sub County`) |>
  summarise(
    Number_of_Facilities = n(),
    .groups = "drop"
  ) |>
  left_join(
    kirinyaga_data |>
      group_by(`Sub County`) |>
      summarise(Total_Cases = n(), .groups = "drop"),
    by = "Sub County"
  ) |>
  arrange(desc(Number_of_Facilities))

cat("=== CORRECTED: FACILITIES BY PRIMARY SUB-COUNTY ===\n")
print(facilities_by_subcounty_corrected)
cat("Sum of facilities (corrected):", sum(facilities_by_subcounty_corrected$Number_of_Facilities), "\n\n")

# Option 2: Show facilities WITH overlap clearly indicated
facilities_with_overlap <- kirinyaga_data |>
  mutate(
    `Sub County`=case_when(`Sub County`=="Mwea West"~"Kirinyaga North",
                           `Sub County`=="Mwea East"~"Kirinyaga South",
                           
                           TRUE~`Sub County`)
  )|>
  filter(!is.na(`Sub County`), !is.na(`Health Facility`)) |>
  group_by(`Sub County`,Year) |>
  summarise(
    Facilities_Total = n_distinct(`Health Facility`),
    Total_Cases = n(),
    .groups = "drop"
  ) |>
  arrange(desc(Facilities_Total))

# Add annotation about overlap
cat("=== FACILITIES BY SUB-COUNTY (WITH OVERLAP) ===\n")
print(facilities_with_overlap)
cat("\nNote: Some facilities serve multiple sub-counties\n")
cat("Total counting with overlap:", sum(facilities_with_overlap$Facilities_Total), "\n")
cat("Actual unique facilities:", n_distinct(kirinyaga_data$`Health Facility`, na.rm = TRUE), "\n\n")

# ===========================
# VISUALIZATION - CORRECTED
# ===========================

# Plot 1: Corrected (primary sub-county assignment)
ggplot(facilities_by_subcounty_corrected, 
       aes(x = reorder(`Sub County`, Number_of_Facilities), 
           y = Number_of_Facilities)) +
  geom_bar(stat = "identity", fill = "#27aae1") +
  geom_text(aes(label = Number_of_Facilities), hjust = -0.3, size = 5) +
  coord_flip() +
  labs(title = "Distribution of Facility Sites by Sub County - Kirinyaga",
       subtitle = paste0("Total Unique Facilities: 97\n",
                         "Each facility assigned to sub-county with most cases"),
       x = "Sub-County",
       y = "Number of Facilities") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14)
  )

ggsave("~/OneDrive/TIBU output/facilities_by_subcounty_corrected.png", width = 12, height = 6)

# Plot 2: Show overlap explicitly
overlap_data <- facilities_with_overlap |>
  mutate(
    `Sub County`=case_when(`Sub County`=="Mwea West"~"Kirinyaga North",
                           `Sub County`=="Mwea East"~"Kirinyaga South",
                           
                           TRUE~`Sub County`)
  )|>
  mutate(
    Unique_Facilities = facilities_by_subcounty_corrected$Number_of_Facilities[
      match(`Sub County`, facilities_by_subcounty_corrected$`Sub County`)
    ],
    Overlap = Facilities_Total - Unique_Facilities
  ) |>
  pivot_longer(cols = c(Unique_Facilities, Overlap), 
               names_to = "Type", 
               values_to = "Count")
 

ggplot(overlap_data, aes(x = reorder(`Sub County`, Facilities_Total), 
                         y = Count, 
                         fill = Type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Unique_Facilities" = "#27aae1", 
                               "Overlap" = "#fe7501"),
                    labels = c("Overlap" = "Also serving from other sub-counties",
                               "Unique_Facilities" = "Primary facilities")) +
  labs(title = "Health Facilities by Sub-County - Showing Overlap",
       subtitle = "Some facilities serve multiple sub-counties (shown in orange)",
       x = "Sub-County",
       y = "Number of Facilities",
       fill = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.position = "bottom"
  )

ggsave("~/OneDrive/TIBU output/facilities_overlap_visualization.png", width = 12, height = 8)

# ===========================
# DETAILED REPORT
# ===========================

# Create comprehensive facility report
facility_report <- kirinyaga_data |>
  mutate(
    `Sub County`=case_when(`Sub County`=="Mwea West"~"Kirinyaga North",
                           `Sub County`=="Mwea East"~"Kirinyaga South",
                           
                           TRUE~`Sub County`)
  )|>
  filter(!is.na(`Health Facility`)) |>
  group_by(`Health Facility`) |>
  summarise(
    Total_Cases = n(),
    SubCounties_Served = n_distinct(`Sub County`, na.rm = TRUE),
    SubCounty_List = paste(sort(unique(na.omit(`Sub County`))), collapse = ", "),
    Primary_SubCounty = names(sort(table(`Sub County`), decreasing = TRUE))[1],
    Years_Reporting = paste(sort(unique(Year)), collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(desc(Total_Cases))

cat("=== COMPLETE FACILITY REPORT (First 20) ===\n")
print(head(facility_report, 20))

write.csv(facility_report, "~/OneDrive/TIBU output/facility_complete_report.csv", row.names = FALSE)
write.csv(facilities_multiple_subcounties, "~/OneDrive/TIBU output/facilities_multiple_subcounties.csv", row.names = FALSE)

cat("\n=== SUMMARY ===\n")
cat("Total unique facilities: 97\n")
cat("Facilities serving only 1 sub-county:", sum(facility_report$SubCounties_Served == 1), "\n")
cat("Facilities serving 2+ sub-counties:", sum(facility_report$SubCounties_Served > 1), "\n")

cat("\n=== FACILITY ANALYSIS COMPLETE ===\n")



## Plot the cases by year for each sub county of Kirinyaga county to show the spatial and temporal case distributions
cases_by_subcounty_year <- kirinyaga_data |>
  mutate(
    `Sub County`=case_when(`Sub County`=="Mwea West"~"Kirinyaga North",
                           `Sub County`=="Mwea East"~"Kirinyaga South",
                           
                           TRUE~`Sub County`)
  )|>
  filter(!is.na(`Sub County`)) |>
  group_by(Year, `Sub County`) |>
  summarise(Total_Cases = n(), .groups = "drop")


ggplot(cases_by_subcounty_year, 
       aes(x = factor(Year), y = Total_Cases, fill = `Sub County`)) +
  
  # Bar chart
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  
  # Label each bar with its total value
  geom_text(
    aes(label = Total_Cases),
    position = position_dodge(width = 0.9),
    vjust = -0.4,     # lift text slightly above bar
    size = 5,
    fontface = "bold"
  ) +
  
  labs(
    title =paste0( "Pulmonary Tuberculosis Cases by Sub-County and Year - Kirinyaga County TIBU data\nN= ",sum(cases_by_subcounty_year$Total_Cases)),
    x = "Year",
    y = "Number of TB Cases",
    fill = "Sub-County"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "right",
    legend.title = element_text(size = 16, face = "bold")
  ) +
  scale_fill_manual(values = my_colors)

ggsave("~/OneDrive/TIBU output/tb_cases_by_subcounty_year.png", width = 12, height = 6)
## Show the above in a map of kirinyaga county with sub counties boundaries
# Note: This requires shapefiles for Kirinyaga sub-counties

## Plot the top reporting facilities in Kirinyaga county
library(dplyr)
library(ggplot2)

top_facilities <- kirinyaga_data |>
  mutate(
    `Sub County`=case_when(`Sub County`=="Mwea West"~"Kirinyaga North",
                           `Sub County`=="Mwea East"~"Kirinyaga South",
                           
                           TRUE~`Sub County`)
  )|>
  filter(!is.na(`Health Facility`)) |>
  group_by(`Health Facility`, Year) |>
  summarise(Total_Cases = n(), .groups = "drop") |>
  group_by(Year) |>
  slice_max(order_by = Total_Cases, n = 15, with_ties = FALSE) |>
  ungroup()


# Simple version (without tidytext)
ggplot(top_facilities,
       aes(x = reorder(`Health Facility`, Total_Cases), y = Total_Cases)) +
  geom_bar(stat = "identity", fill = "#27aae1") +
  geom_text(aes(label = Total_Cases), hjust = -0.3, size = 6) +
  coord_flip() +
  labs(
    title = paste0("TB Cases from Top 15 Reporting Health Facilities Kirinyaga County \n N= ",
                   n_distinct(kirinyaga_data$`Health Facility`)
                   ),
    x = "Health Facility",
    y = "Number of TB Cases"
  ) +
  theme_minimal(base_size = 12) +
  facet_wrap(~Year, scales = "free_y")+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 16, face = "bold")
  )

ggsave("~/OneDrive/TIBU output/top_reporting_facilities_kirinyaga.png", width = 22, height = 12)


## Get the total cases from top 5 facilities and see the distribution  for each year
fac_contr<-top_facilities |> 
  filter(`Health Facility` %in% c(
    "Kerugoya County Refferal Hospital",
    "Thiba Dispensary",
    "Kimbimbi Sub-District Hospital",
    "Sagana Subcounty Hospital",
    "Sagana Sub-District Hospital",
    "Kutus Dispensary",
    "Mt. Kenya (ACK) Hospital",
    "Baricho Health Centre",
    "Whiterose Medical Centre",
    "Kangaru Dispensary (Kirinyaga)",
    "Difathas Health Centre",
    "Mutithi Health Centre"
    # "Kiamutugu Health Centre",
    # 
    # "Kagumo Dispensary",
   
    
    
    
   
  )) |>
  group_by(Year) |>
  reframe(nn = sum(Total_Cases)) |>
  ungroup() |>
  left_join(
    kirinyaga_data |>
      group_by(Year) |>
      reframe(total_c = n()) |>
      ungroup(),
    by = "Year"
  ) |>
  mutate(prop = nn / total_c * 100) 


  
  ggplot(data=fac_contr,aes(x = Year, y = prop)) +
  geom_area(fill = "#2171b5", alpha = 0.15) +                        # shaded area under line
  geom_line(color = "#2171b5", linewidth = 2) +
  geom_point(aes(size = nn), color = "#08306b", alpha = 0.8) +       # bubble size = raw cases
  geom_text(aes(label = paste0(round(prop, 1), "%")),
            vjust = -1.2, size = 5, fontface = "bold",
            color = "#08306b") +
  # reference line at 50% — majority burden threshold
  geom_hline(yintercept = 50, linetype = "dashed",
             color = "red", linewidth = 0.8, alpha = 0.6) +
  ggplot2::annotate("text", x = min(top_facilities$Year), y = 51.5,
           label = "50% burden threshold", hjust = 0,
           size = 4, color = "red", alpha = 0.7) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(breaks = unique(fac_contr$Year)) +
  scale_size_continuous(name = "Raw Cases", range = c(3, 10)) +
  labs(
    title    =paste0(12
                       ," Facilities account for a major share of TB Burden in Kirinyaga County"),
    subtitle = "Proportion of total county TB cases reported by selected facilities | 2021–2025",
    x        = "Year",
    y        = "% of County TB Cases",
    caption  = "Source: MOH 706 Rev 2020 | Bubble size proportional to raw case count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(size = 16, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 13, color = "grey40", hjust = 0),
    plot.caption  = element_text(size = 10, color = "grey50", hjust = 1),
    axis.text     = element_text(size = 14),
    axis.title    = element_text(size = 16),
    legend.position = "none"
  )
  
  ggsave("~/OneDrive/TIBU output/top_12_facilities_kirinyaga_contribute slightly 50.png", width = 22, height = 12)
  # ===============================================================================================
## Compare the TB cases in Kirinyaga county to the national TB cases over the same period (2022-2025) and from Individual facilities and the aggregate KHIS DATA and individual level data
# ===============================================================================================
# 
# mortality_data<-read_parquet("mortality_data_with_CI.parquet")
mortality_data<-read_parquet("/Volumes/D/Methods-for-HTA/Data Source/mortaility_estimated.parquet")

# Compare TB deaths from TIBU vs Mortality dataset
tibu_tb_deaths <- kirinyaga_data |>
  filter(`Treatment Outcome` == "Died (D)") |>
  group_by(Year) |>
  summarise(TIBU_TB_Deaths = n(), .groups = "drop") |>
  left_join(
    mortality_data |>
      filter(county == "Kirinyaga",
           grepl("Tuberculosis",Level4, ignore.case = T)) |>
      group_by(Year_x) |>
      summarise(Kirinyaga_TB_Deaths = sum(count, na.rm = TRUE), .groups = "drop"),
    by = c("Year"="Year_x")
  )
# crvs deaths due to TB in Kirinyaga is 61 in  2024
# Make data long for easy plotting
tb_long <- tibu_tb_deaths |>
  pivot_longer(cols = c(TIBU_TB_Deaths, Kirinyaga_TB_Deaths),
               names_to = "Data_Source",
               values_to = "Deaths")

# Plot: side-by-side bars
ggplot(tb_long|>
         filter(Year!=2026), aes(x = factor(Year), y = Deaths, fill = Data_Source, color = Data_Source)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_line(aes(group = Data_Source),
            position = position_dodge(width = 0.8),
            linewidth = 1.2) +
  geom_point(aes(group = Data_Source),
             position = position_dodge(width = 0.8),
             size = 3, show.legend = FALSE) +
  geom_text(
    aes(label = Deaths),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 5,
    fontface = "bold",
    color = "black"
  ) +
  scale_fill_manual(
    values = c("Kirinyaga_TB_Deaths" = "#fe7501", "TIBU_TB_Deaths" = "#27aae1"),
    labels = c("Individual Level Estimates", "TIBU Data")
  ) +
  scale_color_manual(
    values = c("Kirinyaga_TB_Deaths" = "#fe7501", "TIBU_TB_Deaths" = "#27aae1"),
    labels = c("Individual Level Estimates", "TIBU Data")
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.12)),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = "Tuberculosis Deaths in Kirinyaga County: TIBU Data vs KHIS Individual-Level Data",
    subtitle = "TIBU reported cases consistently exceed KHIS tracker  deaths across all years",
    x        = "Year",
    y        = "Number of TB Deaths Reported at Facility",
    fill     = "",
    color    = "",
    caption  = "Source: TIBU | Individual-level data: MOH records"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(size = 18, face = "bold", hjust = 0),
    plot.subtitle      = element_text(size = 13, color = "grey40", hjust = 0),
    plot.caption       = element_text(size = 10, color = "grey50", hjust = 1),
    axis.text          = element_text(size = 16),
    axis.title         = element_text(size = 16, face = "bold"),
    legend.text        = element_text(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave("~/OneDrive/TIBU output/tb_deaths_tibu_vs_individual_kirinyaga.png", width = 14, height = 8)


## line plot for the deaths between TIBU program and KHIS individual level data
tb_long |> 
  filter(Year != 2026) |>
  ggplot(aes(x = Year, y = Deaths, color = Data_Source, linetype = Data_Source)) +
  geom_line(size = 1.5) +
  geom_point(size = 4) +
  geom_text(
    aes(label = Deaths),
    vjust = -0.5,
    size = 8,
    fontface = "bold",
    color = "black"
  ) +
  scale_color_manual(
    values = c(
      "Kirinyaga_TB_Deaths" = "#fe7501",  # Individual Level
      "TIBU_TB_Deaths"      = "#27aae1",
      "CRVS_TB_Deaths"      = "#08306b"
    ),
    labels = c(
      "Kirinyaga_TB_Deaths" = "KHIS Individual Level Estimates",
      "TIBU_TB_Deaths"      = "TIBU Data",
      "CRVS_TB_Deaths"      = "CRVS Data"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Kirinyaga_TB_Deaths" = "dashed",   # Dashed line for individual level
      "TIBU_TB_Deaths"      = "solid",
      "CRVS_TB_Deaths"      = "solid"
    ),
    labels = c(
      "Kirinyaga_TB_Deaths" = "KHIS Individual Level Estimates",
      "TIBU_TB_Deaths"      = "TIBU Data",
      "CRVS_TB_Deaths"      = "CRVS Data"
    )
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.12)),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  labs(
    title    = "Tuberculosis Deaths in Kirinyaga County: TIBU Data vs KHIS Individual-Level Data",
    # subtitle = "TIBU reported cases consistently exceed KHIS tracker deaths across all years",
    x        = "Year",
    y        = "Number of TB Deaths Reported at Facility",
    color    = "",
    linetype = "",
    caption  = "Source: TIBU | Individual-level data: MOH records"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(size = 18, face = "bold", hjust = 0),
    plot.subtitle      = element_text(size = 13, color = "grey40", hjust = 0),
    plot.caption       = element_text(size = 10, color = "grey50", hjust = 1),
    axis.text          = element_text(size = 16),
    axis.title         = element_text(size = 16, face = "bold"),
    legend.text        = element_text(size = 16),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

ggsave("~/OneDrive/TIBU output/tb_deaths_tibu_vs_individual_kirinyaga_lineplot.png",
       width = 14, height = 8)




## lOAD THE data for individual level for analysis and visualization
data<-readRDS("~/Library/CloudStorage/OneDrive-Personal/BoD/data_clean6a.rds")
kirinyaga_ind<-data|>filter(county=="Kirinyaga")
nrow(kirinyaga_ind)  # Total number of TB cases in Kirinyaga from individual level data)
# Compare total TB cases from TIBU vs Individual-level data
ind_kir<-kirinyaga_ind|>
  filter(
    Level4%in%c("TB","Tuberculosis","Pulmonary TB")
  )
nrow(ind_kir) # Total number of TB cases in Kirinyaga from individual level data where the 782 cases of pulmonary tb 

# The distribution of cases  by health facility from individual level data
top_facilities_ind <- ind_kir |>
  filter(!is.na(`Organisation unit name`)) |>
  group_by(`Organisation unit name`, Year) |>
  summarise(Total_Cases =sum(number), .groups = "drop") |>
  group_by(Year) |>
  slice_max(order_by = Total_Cases, n = 20, with_ties = FALSE) |>
  ungroup()


## Visualization of the cases  by health facility from individual level data and year facet wrapped
ggplot(top_facilities_ind,
       aes(x = reorder(`Organisation unit name`, Total_Cases), y = Total_Cases)) +
  geom_bar(stat = "identity", fill = "#27aae1") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+ # Wrap long facility names
  geom_text(aes(label = Total_Cases), hjust = -0.03, size = 6) +
  coord_flip() +
  labs(
    title = "Temporal  Distribution of TB Reported Cases from 20 Health facilities in Kirinyaga County (Individual level data) \n N=782 Cases",
    x = "Health Facility",
    y = "Number of TB Cases"
  ) +
  theme_minimal(base_size = 12) +
  facet_wrap(~Year, scales = "free_y")+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 16, face = "bold")
  )
 
ggsave("~/OneDrive/TIBU output/top_reporting_facilities_kirinyaga_individual_level.png", width = 22, height = 12)

## Merge the individual level data with TIBU data to compare the cases by health facility

## Cases by Health Facility

## TIBU total deaths by health facility
top_facilities <- kirinyaga_data |>
  group_by(`Health Facility`,
           # `MFL Code`,
           Year) |>
  summarise(Cases = n()) |>
  arrange(desc(Cases))|>
  ungroup()|>
  tidyr::pivot_wider(names_from = Year, values_from = Cases, values_fill = 0)|>
  rename_with(~paste0("tibu_", .), .cols = 2:7)|>
  mutate(
  
    `Health Facility`=gsub("Sub-District","Sub-County",`Health Facility`),
    `Health Facility`=case_when(
      `Health Facility`=="Kerugoya District Hospital"~"Kerugoya County Refferal Hospital",
      `Health Facility`=="Mt. Kenya (ACK) Hospital"~"Mt Kenya (Ack) Hospital",
      `Health Facility`== "Sagana Sub-County Hospital" ~"Sagana Subcounty Hospital" ,
      `Health Facility`=="Nucleaur Medical Diagnostic Center" ~ "Nucleaur Medical Diagnostic Centre" ,
      `Health Facility`=="Kagio Catholic Dispensary (Mary Immucate Catholic Dispensary)"~"Kagio Hospital",
      `Health Facility`=="Mary Immaculate Catholic Dispensary Makutano" ~ "Mary Immaculate Kerugoya Hospital" ,
      TRUE~`Health Facility`
    )
  )

  ## Individual level data summary by health facility and year
   fac_summary<- ind_kir|>
      group_by(
        `Organisation unit code`,
        `Organisation unit name`,Year
      )|>
      reframe(ind_data=sum(number))|>
     ungroup()|>
    tidyr::pivot_wider(names_from = Year, values_from = ind_data, values_fill = 0)|>
     rename_with(~paste0("ind_", .), .cols = 3:6)
  ## Fuzzy merge the two datasets by health facility name
  # library(fuzzyjoin)
  # merged_facilities<-fuzzyjoin::stringdist_left_join(top_facilities, fac_summary, by = c("MFL Code"="Organisation unit code"),
  #                                                    max_dist = 0, distance_col = "dist")|>
  #  dplyr:: select(-dist)
  # View(merged_facilities)
  # fac_summary|>
  #   filter(`Organisation unit name`%in% top_facilities$`Health Facility`)
  # 
  ## Codes that are both
  
  ddd<-fac_summary|>
        filter(`Organisation unit name`%in% top_facilities$`Health Facility`)
  View(ddd)
  
  fac_summary|>
    filter(!`Organisation unit name`%in% ddd$`Organisation unit name`)
  library(fuzzyjoin)
  merged_facilities<-fuzzyjoin::stringdist_full_join(top_facilities, fac_summary, by = c("Health Facility"="Organisation unit name"),
                                                     max_dist = 0, distance_col = "dist")|>
   dplyr:: select(-dist)

  ##Facilities that are only in Individual and can offer TB treatement service and not available in TIBU program
  # 1.                  10522 Kerugoya Medical Centre                          5        6        1        3
  # 2                    12604 Our Lady Of Lourdes Mutomo Hospital             25       28       10       23
  # 3                    22147 Furaha Medical Care                              0        0        0        1
  # 4                    22372 Kerugoya Fortis Medical And Cancer Centre        4        0        2        0
  # 5                    26717 Mururi Medical Center                            0        1        0        0
  ## Only 15 facilities are both in Individual case-based data but missing in TIBU case-based data
  missing_in_tibu<-fac_summary|>
    filter(!`Organisation unit code`%in% top_facilities$`MFL Code`)

## Merging the two datasets by health facility code
 fac_both<- merged_facilities |>
    # Remove missing or irrelevant rows
    filter(
      !is.na(tibu_2022),
      !is.na(ind_2023),
      !is.na(tibu_2024),
      !is.na(tibu_2025)
     
    ) |>
    # Remove unnecessary columns
   dplyr:: select(-c(`Health Facility`,`Organisation unit code`)) |>
    # Reshape to long format
    pivot_longer(
      cols = - `Organisation unit name`,
      names_to = "Year_Source",
      values_to = "Cases"
    ) |>
    # Split 'tibu_2023' → Source = tibu, Year = 2023
    separate(Year_Source, into = c("Source", "Year"), sep = "_") 
  
  ## Bar plot for the total and compare what Individual level data and program specific report among the common facilities in the data set over the years of available data
 
 fac_both|>
    group_by(Year,Source)|>
        reframe(nn=sum(Cases))|>
           ungroup()|>
              ggplot(aes(x=Year,y=nn, fill=Source))+
                 geom_bar(stat="identity", position="dodge")+
                    geom_text(aes(label=nn), position=position_dodge(width=0.9), vjust=-0.4,size=5)+
                      scale_fill_manual(
                        values = c("ind" = "#fe7501", "tibu" = "#27aae1"),
                                              labels = c("ind" = "Individual-level Data", "tibu" = "TIBU Data")
                            ) +
                            labs(title = "TB Cases in Kirinyaga County: TIBU Program vs KHIS Individual-level Data\n from the 15 Health Facilities common in both data sets",
                                x = "Year",
                                y = "Number of TB Cases",
                                fill = "Source"
                                 ) +
                                 theme_minimal(base_size = 14) +
                                    theme(
                                          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                                              axis.text = element_text(size = 12),
                                              axis.title = element_text(size = 14, face = "bold"),
                                              legend.text = element_text(size = 14),
                                              legend.position = "right"
                                          )
                                          
 
 ggsave("~/OneDrive/TIBU output/tb_cases_tibu_vs_individual_kirinyaga.png", width = 12, height = 6)

    # Plot cases by facility for facet wrapped by year
 ggplot(fac_both,aes(x=log(Cases), fill = Source))+geom_density(alpha=0.5)
ggplot(fac_both,
       aes(x = reorder( `Organisation unit name`, Cases), y = Cases, fill=Source)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) + # Wrap long facility names
  geom_text(aes(label = Cases), position=position_dodge(width=0.9), hjust =- 0.1, size = 5) +
  coord_flip() +
  labs(
    title = "TB Cases by Health Facility in Kirinyaga County: TIBU vs Individual-level Data",
    x = "Health Facility",
    y = "Number of TB Cases",
    fill = "Data Source"
  ) +
  facet_wrap(~Year, scales = "free_y")+
  scale_fill_manual(
    values = c("ind" = "#fe7501", "tibu" = "#27aae1"),
    labels = c("ind" = "Individual-level Data", "tibu" = "TIBU Data")
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    legend.title = element_text(size = 16, face = "bold")
  )

ggsave("~/OneDrive/TIBU output/tb_cases_tibu_vs_individual_kirinyaga_by_facility.png", width = 14, height = 18)



## Prevalence for the TB cases in Kirinyaga county from TIBU data and individual level data utilizing the proportion 
prop_kr<-kirinyaga_data|>
  group_by(County,
           Year,
           age_years_categorical,
           `Sex M/F`,
           )|>
  reframe(mm=sum(number))|>
mutate(
  ## Get the prevalence proportion for each gender and each age group over each year. 
  proportion=mm/sum(mm),
  .by=c(
    Year
  )
)|>
  ungroup()|>
  dplyr::select(-County,-mm )



# Summarize average proportions by year, sex, and age group
prop_gender_age <- prop_kr |>
  group_by(Year, `Sex M/F`, age_years_categorical) |>
  summarise(mean_proportion = mean(proportion, na.rm = TRUE), .groups = "drop")
ggplot(prop_gender_age, 
       aes(x = age_years_categorical, y = mean_proportion, 
           fill = as.factor(Year), group = Year)) +
  geom_ribbon(aes(ymin = 0, ymax = mean_proportion),
              alpha = 0.5, color = "white") +
  geom_line(size = 1.2, color = "white") +
  facet_wrap(~ `Sex M/F`) +
  scale_fill_viridis_d(option = "plasma", name = "Year") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of TB Cases by Age Group and Year in Kirinyaga County",
    # subtitle = "Faceted by Gender (using ribbon to show variation over years)",
    x = "Age Group",
    y = "Proportion of TB Cases"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )+
  scale_fill_manual(values = my_colors)


ggsave("~/OneDrive/TIBU output/tb_proportion_cases_kirinyaga_by_facility.png", width = 14, height = 18)

## Combine the two data sets and compare the proportions
## Prevalence for the TB cases in Kirinyaga county from TIBU data and individual level data
prop_kr |>
  mutate(
    gender = `Sex M/F`,
    gender = case_when(
      gender == "F" ~ "female",
      TRUE ~ "male"
    ),
    age_years_categorical = as.character(age_years_categorical)  # Convert to character
  ) |>
  left_join(
    ind_kir |>
      group_by(Year, gender, `Age years categorical`) |>
      reframe(total = sum(number)) |>
      mutate(
        proportions = total / sum(total),
        .by = Year
      ) |>
      ungroup() |>
      mutate(
        `Age years categorical` = as.character(`Age years categorical`),  # Fixed syntax
        Year = as.integer(Year)
      ),
    by = c("Year", "age_years_categorical" = "Age years categorical", "gender")
  ) |>
  mutate(
    # Define the correct age order
    age_years_categorical = factor(
      age_years_categorical,
      levels = c("<1", "1-2", "3-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                 "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                 "65-69", "70-74", "75-79", "80+"),
      ordered = TRUE
    )
  ) |>
  mutate(
    `Absolute difference` = round(proportions - proportion, 4),
    difference_sign = ifelse(`Absolute difference` < 0, "Negative", "Positive")  # Fixed syntax
  ) |>
  ggplot(aes(x = age_years_categorical, y = `Absolute difference`, fill = difference_sign)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c("Negative" = "#fe7501", "Positive" = "#27aae1")
  ) +
  facet_wrap(~ gender + Year) +
  labs(
    title = "Difference in proportion between KHIS Individual vs TIBU by Gender and Years Over Age.",
    x = "Age group",
    y = "Difference in proportion (KHIS - TIBU)",
    caption = "Orange = KHIS lower than Tibu | Blue = KHIS higher than Tibu"
  ) +
  theme_minimal() +
  theme(
    title=element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size=15,angle = 45, hjust = 1),
    legend.position = "none",
    axis.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 16, face = "bold"),
  
    axis.text.y = element_text(size = 15)
  )



ggsave("~/OneDrive/TIBU output/tb_case_prop_differences.png", width = 14, height = 12)


## Using the factor to adjust the prevalence of TB cases in Kirinyaga county from TIBU data and individual level data
kir_est_data<-kirinyaga_ind|>
  mutate(estimated_cases=case_when(
    Year==2022~round(number*15.07,0),
    Year==2023~round(number*6.74,0),
    Year==2024~round(number*6.80,0),
    Year==2025~round(number*6.48,0),
    TRUE~NA_real_
  ))|>
  dplyr::select(
    county,
    Year,
    gender,
    `Age years categorical`,
    Final_Code,
    Level1,Level2,Level3,Level4,Level5,Level6,`M&MCCoD  Reporting Mode`,
    number,
    estimated_cases
  )
View(kir_est_data)

## Group by the 

kirinyaga_estimates<-kir_est_data|>
  group_by(Year,gender, `Age years categorical`,Level4,`M&MCCoD  Reporting Mode`)|>
  reframe(total_estimated_cases=sum(estimated_cases),
          tot_undj=sum(number))

## Save the output as compressed file rds
saveRDS(kirinyaga_estimates, "~/Library/CloudStorage/OneDrive-Personal/BoD/kirinyaga_estimated_tb_cases.rds", compress = "xz")

## How were the cases referred to the facilities in Kirnyaga county



kirinyaga_summary <- kirinyaga_data |>
  group_by(Year, `Referred By`) |>
  summarise(cases = n(), .groups = "drop")|>
  as.data.frame()

stargazer(kirinyaga_summary, type = "text", summary = F)
## Visualize the referral cases
ggplot(kirinyaga_summary|>
         mutate(`Referred By`=gsub("\\s*\\(.*?\\)", "", `Referred By`))|>
         filter(cases>=5),
       aes(x = reorder_within(`Referred By`, cases, Year),
           y = cases,
           fill = `Referred By`)) +
  geom_bar(stat = "identity", position=position_dodge(), fill="#27aae1") +
  
  geom_text(aes(label = cases), hjust = -0.01, size = 6) +
  labs(
    title = "Tuberculosis Cases Referred by Source in Kirinyaga County from the year 2022 to 2025",
    x = "Referred by",
    y = "Number of Cases"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 16, face = "bold"),
    
    legend.position = "none"
  ) +
  coord_flip() +
  facet_wrap(~Year, scales = "free_y", ncol=1) +
  scale_x_reordered()  # Wrap long referral source names

ggsave("~/OneDrive/TIBU output/tb_cases_referred_by_source_kirinyaga.png", width = 14, height = 12)


## Link to how TIBU data is collected and reported in the facilities and how it is aggregated in the KHIS data to understand the differences in the two data sources and how they can be used together for better TB surveillance and control in Kirinyaga county
##==============================================================================
## https://tbassessment.stoptb.org/assets/docs/Digital%20TB%20Surveillance%20System%20Assessment%20All%20Country%20Reports/Kenya%20Digital%20TB%20Surveillance%20System%20Assessment%20Report.pdf
## ===============================================================================

# Where are the most cases come from in Kirinyaga county and how are they reported in the TIBU data and KHIS data
## Sub County level analysis
kir_subcounty_sum<-kirinyaga_data |>
  group_by(Year, `Sub County`) |>
  mutate(
    `Sub County`=case_when(`Sub County`=="Mwea West"~"Ndia",
                           `Sub County`%in%c("Mwea East","Kirinyaga South")~"Mwea",
                           `Sub County`=="Kirinyaga West"~"Gichugu",
                           
                           TRUE~`Sub County`)
  )|>
  summarise(Total_Cases = sum(number), .groups = "drop") |>
  arrange(desc(Total_Cases))


### Read the shape file for the subcounty admin 2 in kenya
library(sf)
library(ggmagnify)
kenya_sh<-st_read("~/Library/CloudStorage/OneDrive-Personal/BoD/ken_admin_boundaries.shp/ken_admin2.shp")
kirinyaga <- kenya_sh |>
  filter(adm1_name == "Kirinyaga")
kirinyaga$adm2_name <-dplyr:: recode(
  kirinyaga$adm2_name,
  "Mwea East" = "Mwea",
  "Mwea West" = "Ndia"
)


kirinyaga_tb <- kirinyaga |>
  left_join(kir_subcounty_sum, by = c("adm2_name" = "Sub County"))
## Ploting the data points
library(ggplot2)
library(viridis)
library(sf)
library(dplyr)
library(ggspatial)
library(grid)

# Compute subcounty centroids for labeling
kirinyaga_labels <- kirinyaga_tb |>
  st_centroid() |>
  st_coordinates() |>
  as.data.frame() |>
  cbind(kirinyaga_tb |> st_drop_geometry()) |> 
  rename(long = X, lat = Y)




# --- Base TB plot (without repeated map elements) ---

#Define the subcounties to zoom outcomes_summary
inset_counties<- kirinyaga_tb |> filter(adm2_ref_n %in% c("Gichugu", "Kirinyaga Central",              "Mwea ",             "Ndia"))
tb_map <- ggplot() +
  geom_sf(data = kenya_sh, fill = NA, color = "grey80", size = 0.5) +
  geom_sf(data=kirinyaga_tb,aes(fill = Total_Cases), color = "white") +
  geom_text(
    data = kirinyaga_labels,
    aes(x = long, y = lat, label = adm2_name),
    size = 7,
    color = "#27aae1",
    fontface = "bold",
    check_overlap = TRUE
  ) +
    facet_wrap(~ Year, ncol = 5) +
  scale_fill_stepsn(
    colors = viridis::viridis(30, direction = -1),
    n.breaks = 11,
    na.value = "white",
    transform = 'log10',
    name = NULL,
   
    guide = guide_colorbar(barheight = unit(0.8, "in"))
  ) +

  theme_void() +
  labs(
    # title = "Spatio-Temporal Distribution of TB Cases by Subcounty in Kirinyaga County",
    # subtitle = "Data by Year",
    caption = "Source: TIBU Program Data",
    x = "",
    y = ""
  ) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    
   
    plot.caption = element_text(size = 16, hjust = 1, face = "bold"),
    
    legend.position = "right",
    name = "TB Cases"

  )+
  ggmagnify:: geom_magnify(
    data = kirinyaga_tb,
    aes(from = adm1_name == "Kirinyaga"),
    to = c(xmin = 34, xmax = 37, ymin = -8, ymax = -5),
    shadow = FALSE, linewidth = 0.2, colour = "black",
    shape = "outline", aspect = "fixed", expand = 0
  )
  
## Save the map
ggsave(plot=tb_map,
       "~/OneDrive/TIBU output/tb_cases_map_kirinyaga1.png", width = 12, height =14)



# # --- Draw north arrow and scale bar ONCE for all facets ---
tb_map +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    style = north_arrow_fancy_orienteering,
    pad_x = unit(1, "cm"), pad_y = unit(1, "cm")
  )+
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    pad_x = unit(1, "cm"),
    pad_y = unit(0.5, "cm"),
    text_cex = 1.6
  ) 

  coord_sf(clip = "off")   # ensures annotations aren't clipped by facets



## Save the map
ggsave("~/OneDrive/TIBU output/tb_cases_map_kirinyaga.png", width = 14, height = 8)




## Estimate the number of  TB cases attributable to 5  risk factors HIV, Undernutrition, Alcohol use disorder , Smoking and Diabetes, Hypertension and other

kirinyaga_data1_1<-kirinyaga_data|>
  mutate(causes=case_when(
    grepl("Alcoholism|Abuse|smoking|Smoker", Comorbidity)~"Alcoholism and smoking",

  grepl("Hypertension|Diabetes|Diabetic",  Comorbidity)~"Hypertension and diabetis",
grepl("HIV",Comorbidity)~"HIV/AIDs",
    grepl("Cardiovascular Disease|Renal|COPD|Liver|Asthma", Comorbidity) ~ "Cardiovascular Disease, Asthma, COPD, Renal diseases",
    grepl("Cancer|COVID-19",Comorbidity)~"Cancer",

    TRUE~Comorbidity
  )
  )
## Get the summary of the cases by the risk factors
kirinyaga_data1_1|>
  group_by(Year, causes)|>
  summarise(cases=sum(number), .groups = "drop")|>
  arrange(desc(cases))|>
  pivot_wider(names_from = Year, values_from = cases, values_fill = 0)|>View()



## Get the aggregate KHIS data points =============================================================



tb_cases<-fread("~/Library/CloudStorage/OneDrive-Personal/BoD/TB new cases and deaths data.csv")|>
  pivot_longer(cols = -c(1:10),
               names_to = "month_year",
               values_to = "cases")|>
  separate(
    month_year, into=c("month","year",sep=" ")
  )|>
  filter(cases>0)|>
  dplyr::select(county="organisationunitname",dataname,year,month, cases)|>
  pivot_wider(
    id_cols = c(county, year, month),
    values_from = cases,
    names_from = dataname
  )|>
  mutate(county=gsub(" County","", county))|>
  ungroup()
 ## Get the temporal plots for the TB Cases tested to the confirmed positives and deaths

tb_qdata<-tb_cases |>
  mutate(total_deaths = rowSums(across(6:9), na.rm = TRUE)) |>
  group_by(year, month) |>
  reframe(
    `total confirmed positive` = sum(`MOH 706 Rev 2020_New presumptive TB cases  Number Positive`, na.rm = TRUE),
    `deaths >14 years`         = sum(`TB deaths Adult >14yrs, Male` + `TB deaths Adult >14yrs,  Female`, na.rm = TRUE),
    `deaths <14 years`         = sum(`TB deaths Children <14yrs,  Female` + `TB deaths Children <14yrs, Male`, na.rm = TRUE),
    `total deaths`             = sum(total_deaths, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(date = as.Date(paste("01", month, year), format = "%d %B %Y")) |>
  mutate(quarter = paste0(year, " Q", ceiling(as.integer(format(date, "%m")) / 3))) |>
  group_by(quarter) |>
  summarise(
    across(c(`total confirmed positive`, `deaths >14 years`, `deaths <14 years`, `total deaths`), sum),
    date = min(date)
  ) |>
  ungroup() |>
  pivot_longer(
    cols = c(`total confirmed positive`, `deaths >14 years`, `deaths <14 years`, `total deaths`),
    names_to = "metric",
    values_to = "count"
  )

pl1 <- tb_qdata |> 
  filter(quarter != "2026 Q1" & metric == "total confirmed positive") |>
  ggplot(aes(x = reorder(quarter, date), y = count, color = metric, group = metric)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_manual(values = my_colors) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))  # starts at 0, small padding at top
  ) +
  # annotate("segment",                        # dashed line to highlight the 590 point
  #          x = 1, xend = length(unique(tb_qdata$quarter)) - 1,
  #          y = 590, yend = 590,
  #          linetype = "dashed", color = "grey40", linewidth = 0.5) +
 ggplot2:: annotate("text", 
           x = 1, y = 610, 
           label = "590", hjust = 0, size = 4, color = "grey30") +
  labs(
    title = "TB Confirmed Positives Trend in Kenya",
    x     = NULL,
    y     = "Cases",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x     = element_text(angle = 90, hjust = 1, size = 14),
    axis.text.y     = element_text(size = 16),
    axis.title      = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.text     = element_text(size = 16)
  )

plt2 <- tb_qdata |> 
  filter(quarter != "2026 Q1" & metric != "total confirmed positive") |>
  ggplot(aes(x = reorder(quarter, date), y = count, color = metric, group = metric)) +
  geom_line(linewidth = 2) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_manual(values = my_colors) +
  labs(
    title = "TB Deaths Trend in Kenya",
    x     = NULL,
    y     = "Deaths",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x     = element_text(angle = 90, hjust = 1, size = 14),
    axis.text.y     = element_text(size = 16),
    axis.title      = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.text     = element_text(size = 16)
  )

# correct cowplot syntax
library(cowplot)
plot_grid(pl1, plt2, ncol = 2)
## Generate the line plots as quarters

# calculate scale factor between cases and deaths
scale_factor <- tb_qdata |>
  # filter(quarter != "2026 Q1") |>
  summarise(
    max_cases  = max(count[metric == "total confirmed positive"]),
    max_deaths = max(count[metric == "total deaths"])
  ) |>
  mutate(scale = max_cases / max_deaths) |>
  pull(scale)

# reshape to wide for dual axis
tb_wide <- tb_qdata |>
  # filter(quarter != "2026 Q1") |>
  filter(metric %in% c("total confirmed positive", "total deaths", "deaths >14 years", "deaths <14 years")) |>
  pivot_wider(names_from = metric, values_from = count)

tb_wide |>
  ggplot(aes(x = reorder(quarter, date))) +
  # left axis — confirmed positives
  geom_line(aes(y = `total confirmed positive`, color = "total confirmed positive", group = 1), linewidth = 2) +
  geom_point(aes(y = `total confirmed positive`, color = "total confirmed positive"), size = 2) +
  # right axis — deaths (scaled up to match left axis range)
  geom_line(aes(y = `total deaths` * scale_factor, color = "total deaths", group = 1), linewidth = 1.5, linetype = "dashed") +
  geom_point(aes(y = `total deaths` * scale_factor, color = "total deaths"), size = 2) +
  geom_line(aes(y = `deaths >14 years` * scale_factor, color = "deaths >14 years", group = 1), linewidth = 1.5, linetype = "dotted") +
  geom_point(aes(y = `deaths >14 years` * scale_factor, color = "deaths >14 years"), size = 2) +
  geom_line(aes(y = `deaths <14 years` * scale_factor, color = "deaths <14 years", group = 1), linewidth = 1.5, linetype = "dotted") +
  geom_point(aes(y = `deaths <14 years` * scale_factor, color = "deaths <14 years"), size = 2) +
  # dual axis
  scale_y_continuous(
    name = "TB Confirmed Positives",
    sec.axis = sec_axis(~ . / scale_factor, name = "Deaths")
  ) +
  scale_color_manual(values = my_colors) +
  labs(
    title = "TB Confirmed Positives vs Deaths in Kenya",
    x     = "Time horizon",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x        = element_text(angle = 90, hjust = 1, size = 14),
    axis.text.y        = element_text(size = 16),
    axis.title.y.left  = element_text(size = 16, face = "bold", color = my_colors["total confirmed positive"]),
    axis.title.y.right = element_text(size = 16, face = "bold", color = my_colors["total deaths"]),
    legend.position    = "bottom",
    legend.text        = element_text(size = 16)
  )



## Deaths and cases observed over time in Kenya

ggplot(tb_wide, aes(x = reorder(quarter, date))) +
  geom_line(aes(y = `total confirmed positive`, group = 1, color = "TB Confirmed Positive"), linewidth = 2) +
  geom_point(aes(y = `total confirmed positive`, color = "TB Confirmed Positive"), size = 2) +
  geom_line(aes(y = `total deaths` * scale_factor, group = 1, color = "Total Deaths"), linewidth = 1.5) +
  geom_point(aes(y = `total deaths` * scale_factor, color = "Total Deaths"), size = 2) +
  scale_y_continuous(
    name     = "TB Cases",   # left axis title
    limits   = c(0, NA),
    expand   = expansion(mult = c(0, 0.05)),
    sec.axis = sec_axis(~ . / scale_factor, name = "Deaths due to TB")  # right axis title
  ) +
  scale_color_manual(values = my_colors) +
  labs(
    title = "TB Cases vs Deaths in Kenya",
    x     = "Quarterly",
    color = NULL
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title         = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.x        = element_text(angle = 90, hjust = 1, size = 16),
    axis.text.y.left   = element_text(size = 16),
    axis.text.y.right  = element_text(size = 16),
    axis.title.y.left  = element_text(size = 16, face = "bold"),
    axis.title.y.right = element_text(size = 16, face = "bold"),
    axis.title.x       = element_text(size = 16),
    legend.position    = "right",
    legend.text        = element_text(size = 16)
  )

ggsave("~/OneDrive/TIBU output/tb_cases_vs deaths in kenya.png", width = 14, height = 8)

## get the cases by quarter by county 


tb_qdata_county<-tb_cases |>
  mutate(total_deaths = rowSums(across(6:9), na.rm = TRUE),
         county=case_when(county=="Muranga"~"Murang'a",
                          TRUE~county)) |>
  group_by(county,year, month) |>
  reframe(
    `total confirmed positive` = sum(`MOH 706 Rev 2020_New presumptive TB cases  Number Positive`, na.rm = TRUE),
    `deaths >14 years`         = sum(`TB deaths Adult >14yrs, Male` + `TB deaths Adult >14yrs,  Female`, na.rm = TRUE),
    `deaths <14 years`         = sum(`TB deaths Children <14yrs,  Female` + `TB deaths Children <14yrs, Male`, na.rm = TRUE),
    `total deaths`             = sum(total_deaths, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(date = as.Date(paste("01", month, year), format = "%d %B %Y")) |>
  mutate(quarter = paste0(year, " Q", ceiling(as.integer(format(date, "%m")) / 3))) |>
  group_by(county,year,quarter) |>
  summarise(
    across(c(`total confirmed positive`, `deaths >14 years`, `deaths <14 years`, `total deaths`), sum),
    date = min(date)
  ) |>
  ungroup() |>
  pivot_longer(
    cols = c(`total confirmed positive`, `deaths >14 years`, `deaths <14 years`, `total deaths`),
    names_to = "metric",
    values_to = "count"
  )|>
  filter(year!=2026)|>
  left_join(
    
    population_kenya<-readRDS(
      "~/Library/CloudStorage/OneDrive-Personal/BoD/population data.rds"
    )|>
      group_by(county,year)|>
      summarise(pop=sum(population), .groups = "drop"),
    by=c("county","year")
  )|>
  
  ## Generate the incidence for each county
  mutate(incidence=case_when(metric=="total confirmed positive"~count/pop*100000,
                             TRUE~count),
         annulized_incidence=incidence*4)



## Heatmap showing the cases by year for each county

tb_qdata_county |>
  filter(metric == "total confirmed positive") |>
  filter(!grepl("2026", quarter)) |>
  ggplot(aes(x = reorder(quarter, date), y = reorder(county, count), fill = count)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradientn(
    colours  = c("#e5f5f9", "#99d8c9", "#2ca25f"),
    name     = "TB Confirmed\nPositives",
    labels   = scales::comma
  ) +
  labs(
    title    = "County-Level TB Confirmed Positives in Kenya",
    subtitle = "Quarterly distribution across all counties (2021–2025)",
    x        = "Quarter",
    y        = NULL,
    caption  = "Source: MOH 706 Rev 2020 | Analysis: [Your Name]"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(size = 18, face = "bold", hjust = 0),
    plot.subtitle    = element_text(size = 14, color = "grey40", hjust = 0),
    plot.caption     = element_text(size = 11, color = "grey50", hjust = 1),
    axis.text.x      = element_text(angle = 90, hjust = 1, size = 12),
    axis.text.y      = element_text(size = 12),
    legend.title     = element_text(size = 12, face = "bold"),
    legend.text      = element_text(size = 11),
    legend.position  = "right",
    panel.grid       = element_blank()
  )
## The heat map for each county on the incidence

tb_qdata_county |>
  filter(metric == "total confirmed positive") |>
  filter(!grepl("2026", quarter)) |>
  ggplot(aes(x = reorder(quarter, date), y = reorder(county, incidence), fill = annulized_incidence)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradientn(
    colours  =  c("#e5f5f9", "#99d8c9", "#2ca25f"),
    name     = "TB Incidence per 100,000 popultion (2021-2025",
    labels   = scales::comma
  ) +
  labs(
    title    = "County-Level TB Confirmed Positives in Kenya",
    subtitle = "Quarterly distribution across all counties (2021–2025)",
    x        = "Quarter",
    y        = NULL,
    caption  = "Source: MOH 706 Rev 2020 | Analysis: [CEMA]"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(size = 18, face = "bold", hjust = 0),
    plot.subtitle    = element_text(size = 14, color = "grey40", hjust = 0),
    plot.caption     = element_text(size = 11, color = "grey50", hjust = 1),
    axis.text.x      = element_text(angle = 90, hjust = 1, size = 12),
    axis.text.y      = element_text(size = 12),
    legend.title     = element_text(size = 12, face = "bold"),
    legend.text      = element_text(size = 11),
    legend.position  = "right",
    panel.grid       = element_blank()
  )
## Chloropleth to show the high burden counties with TB over the time horizon
library(sf)
library(patchwork)
library(scales)



# compute annualized incidence per county (average across quarters)
county_incidence <- tb_qdata_county 
# join with shapefile — adjust "adm1_name"
kenya_map <- kenya_sh |>
  mutate(county = str_to_title(adm1_name)) |>   # normalize case
  left_join(county_incidence, by = "county")|>
  filter()

# top 10 counties
top10 <- county_incidence |>
  slice_max(annulized_incidence, n = 10) |>
  mutate(county = reorder(county, annulized_incidence))

# ── choropleth map ──────────────────────────────────────────────
p_map <- ggplot(kenya_map) +
  geom_sf(aes(fill = annulized_incidence), color = "white", linewidth = 0.3) +
  scale_fill_gradientn(
    colours  = c("#e5f5f9", "#99d8c9", "#2ca25f"),
    name     = "Incidence per\n100,000 (annualized)",
    na.value = "grey85",
    labels   = comma
  ) +
  labs(
    title    = "TB Annualized Incidence by County in Kenya",
    subtitle = "Per 100,000 population | 2021–2025",
    caption  = "Source: MOH 706 Rev 2020 | Population: Kenya 2019 Census"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title    = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, color = "grey40", hjust = 0.5),
    plot.caption  = element_text(size = 10, color = "grey50", hjust = 1),
    legend.title  = element_text(size = 11, face = "bold"),
    legend.text   = element_text(size = 10)
  )

# ── top 10 bar chart ────────────────────────────────────────────
p_bar <- ggplot(top10, aes(x = annulized_incidence, y = county, fill = annulized_incidence)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(annulized_incidence, 1)),
            hjust = -0.2, size = 4, fontface = "bold") +
  scale_fill_gradientn(
    colours =  c("#e5f5f9", "#99d8c9", "#2ca25f")
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.29))) +
  labs(
    title = "Top 10 Counties by\nAnnualized TB Incidence",
    x     = "Incidence per 100,000",
    y     = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title   = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.y  = element_text(size = 12),
    axis.text.x  = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    panel.grid.major.y = element_blank()
  )

# ── combine with patchwork ───────────────────────────────────────
p_map + p_bar + plot_layout(widths = c(2, 1))
ggsave("~/OneDrive/TIBU output/tb_incidence in kenya.png", width = 14, height = 8)

