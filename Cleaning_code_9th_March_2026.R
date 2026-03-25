library(brms)
library(tidyr)
library(tidybayes)   # ✅ added — needed for as_draws_df, add_epred_draws
library(dplyr)
library(janitor)
library(rstan)
options(Cores=8)
library(data.table)
library(ggplot2)
gc()
# ── Load data ─────────────────────────────────────────────────
khis_data  <- readRDS("~/Library/CloudStorage/OneDrive-Personal/BoD/data_clean6a.rds")
population <- readRDS("~/Library/CloudStorage/OneDrive-Personal/BoD/population data.rds")
# Life expectancy
lf_table <- fread("/Volumes/D/Methods-for-HTA/Data Source/Life tables/Projected life expectancy.csv") |>
  dplyr::select(-V1) |>
  mutate(
    county = case_when(
      county == "Elgeyo-Marakwet" ~ "Elgeyo Marakwet",
      county == "Tharaka-Nithi"   ~ "Tharaka Nithi",
      TRUE ~ county
    ),
    age_group = case_when(
      age_group == "1 - 5"  ~ "1-4",
      age_group == "5 - 10" ~ "5-9",
      age_group == "10 - 15" ~ "10-14",
      age_group == "15 - 20" ~ "15-19",
      age_group == "20 - 25" ~ "20-24",
      age_group == "25 - 30" ~ "25-29",
      age_group == "30 - 35" ~ "30-34",
      age_group == "35 - 40" ~ "35-39",
      age_group == "40 - 45" ~ "40-44",
      age_group == "45 - 50" ~ "45-49",
      age_group == "50 - 55" ~ "50-54",
      age_group == "55 - 60" ~ "55-59",
      age_group == "60 - 65" ~ "60-64",
      age_group == "65 - 70" ~ "65-69",
      age_group == "70 - 75" ~ "70-74",
      age_group == "75 - 80" ~ "75-79",
      age_group %in% c("80 - 85","85 - 90","90 - 95","95 -100") ~ "80+",
      TRUE ~ age_group
    ),
    age_group = factor(age_group, ordered = TRUE),
    year_le = as.character(year_le)
  )

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

## Ordered age category
age_levels <- c(
  "0 - 1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
  "35-39", "40-44", "45-49", "50-54", "55-59", "60-64","65+", "65-69",
  "70-74","75+", "75-79", "80+","81-84","85-89","90-94","95+"
)


# ── Clean KHIS data ───────────────────────────────────────────
khis_data1 <- khis_data |>
  rename(
    County        = county,
    Gender        = gender,
    AgeGroup      = `Age years categorical`,
    Disease       = Level4
  ) |>
  separate(`Keph level`, into = c("levels","FacilityLevel", sep=" "))|>
mutate(AgeGroup=factor(AgeGroup,levels =age_levels, ordered = TRUE ))
  
  
## recode the age category into 80+, 0-1 and 1-4 to merge the knbs projections
khis_data1a<-khis_data1|>  
  
  mutate(
    AgeGroup = case_when(
      AgeGroup == "<1"                              ~ "0 - 1",
      AgeGroup %in% c("1-2", "3-4")                ~ "1-4",
      AgeGroup %in% c("80-84","85-89","90-94","95+") ~ "80+",
      TRUE                                          ~ AgeGroup
    ),
    # ✅ FacilityLevel as ordered factor — preserves clinical hierarchy
    FacilityLevel = factor(FacilityLevel,
                           levels = c("2","3","4","5","6"),
                           ordered = FALSE)
  )
print(dim(khis_data1a))
# ── Aggregate to county × year × disease × age × gender  and filter out the deliveries cases in hospitals───────
khis_data2 <- khis_data1a |>
  filter(
    !is.na(County),
    County != "Nan",
    !is.na(Disease),
    !is.na(`M&MCCoD  Reporting Mode`),
    !Final_Code %in% c(
      "jb20.0","jb20.z","jb20.1","jb20",
      "jb22.0","jb22.1","jb22.z"
    )
  ) |>
  mutate(
    `M&MCCoD  Reporting Mode` = case_when(
      `M&MCCoD  Reporting Mode` == "Bid" ~ "Mortality",
      TRUE ~ `M&MCCoD  Reporting Mode`
    )
  )
print(dim(khis_data2))


## Get the summary at level 1

## CEMA color scheme 
my_colors <- c(
  "#FE7501", "#27AAE1", "#76b5c5", "#063970",
  "#141", "#eab676","#FF5733", "#C70039", "#900C3F", "#581845"
)


plot_data <- khis_data2 |>
  filter(Year > 2021) |>
  mutate(Level1 = trimws(Level1)) |>
  mutate(Level1=as.factor(Level1))|>
  filter(Level1 != "X Extension Codes") |>
  group_by(Year, Level1, `M&MCCoD  Reporting Mode`) |>
  summarise(cases = sum(number, na.rm = TRUE), .groups = "drop") |>
  ungroup() |>
  rename(Type=`M&MCCoD  Reporting Mode`)

# Level 1 plot
top_causes <- plot_data |>
  group_by(Level1) |>
  summarise(total = sum(cases)) |>
  pull(Level1)

plot_data_top <- plot_data |> filter(Level1 %in% top_causes& Level1!=" X Extension Codes" )|>
  mutate(Level1=factor(Level1, levels = c( "Non-communicable"  ,"Communicable,Maternal, Child, Nutrition", "Injury" ), ordered = T))|>na.omit()
## Bar graph and line graph
plot1 <- ggplot(plot_data_top|>filter(Year<2026), aes(x = as.numeric(as.character(Year)))) +
  # Bars (using fill)
  geom_col(
    aes(y = cases, fill = Level1),
    position = position_dodge(width = 0.9),
    width = 0.8
  ) +
  # Lines and points (using color)
  geom_line(
    aes(y = cases, color = Level1, group = Level1),
    position = position_dodge(width = 0.9),
    size = 1
  ) +
  geom_point(
    aes(y = cases, color = Level1, group = Level1),
    position = position_dodge(width = 0.9),
    size = 2
  ) +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ Type, scales = "free_y", ncol = 2) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    strip.text = element_text(face = "bold", size = 16),
    panel.grid.major.x = element_blank(),
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  ) +
  labs(
    y = "Reported cases",
    x = "Year",
    caption = "Data: KHIS | Excludes extension codes"
  ) +
  # Use the same color palette for both fill and color
  scale_fill_manual(values = my_colors) +
  scale_color_manual(values = my_colors)

ggsave(plot = plot1,
       filename = "~/Library/CloudStorage/OneDrive-Personal/BoD/plots/Level1_combined.png",
       dpi = 300,
       width = 12,
       height = 10)


## Get the mortalities by age group and gender and extract the top 20 cause of morbidity and mortality

# ---- 1. Define top N per Level1 ----
top_n <- list(
  "Communicable, Maternal, Child, Nutrition" = 8,
  "Non-communicable" = 12,
  "Injury" = 3
)

# ---- 2. Aggregate and standardize Level1 ----
top_causes_data <- khis_data2 %>%
  filter(Year > 2021 & Year < 2026) %>%
  mutate(
    Level1_std = case_when(
      grepl("Maternal, Child, Nutrition", Level1, ignore.case = TRUE) ~ "Communicable, Maternal, Child, Nutrition",
      grepl("Non-communicable", Level1, ignore.case = TRUE) ~ "Non-communicable",
      grepl("Injury", Level1, ignore.case = TRUE) ~ "Injury",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Level1_std)) %>%
  group_by(Year, Level1_std, Disease, AgeGroup, Gender) %>%
  summarise(total = sum(number), .groups = "drop")


top_causes_data <- top_causes_data %>%
  mutate(
    AgeGroup = factor(AgeGroup,
                      levels = age_levels,
                      ordered = TRUE)
  )
# 2. Create ordered cause_display with proper ranking per group
library(data.table)
top_causes_data <- top_causes_data %>%
  group_by(Level1_std) %>%
  mutate(
    overall = sum(total),
    rank    = dense_rank(desc(overall))
  ) %>%
  ungroup() %>%
  mutate(
    cause_display = case_when(
      Level1_std == "Communicable, Maternal, Child, Nutrition" & rank <= top_n[["Communicable, Maternal, Child, Nutrition"]] ~ sprintf("%d. %s", rank, Disease),
      
      
      Level1_std == "Non-communicable" & rank <= top_n[["Non-communicable"]] ~ sprintf("%d. %s", rank, Disease),
      Level1_std == "Non-communicable" & rank >  top_n[["Non-communicable"]] ~ "Other Non-communicable",
      
      Level1_std == "Injury" & rank <= top_n[["Injury"]] ~ sprintf("%d. %s", rank, Disease),
      
      
      TRUE ~ Disease   # fallback — should not happen
    )
  )
  # Get the actual top diseases per group to build nice ordered levels
top_diseases <- top_causes_data %>%
  filter(rank <= top_n[Level1_std]) %>%
  distinct(Level1_std, rank, Disease) %>%
  mutate(label = sprintf("%d. %s", rank, Disease)) %>%
  arrange(Level1_std, rank)


library(tibble)
top_n_df <- tibble(
  Level1_std = names(top_n),
  n = unlist(top_n)
)

top_diseases <- top_causes_data %>%
  inner_join(top_n_df, by = "Level1_std") %>%
  filter(rank <= n) %>%
  distinct(Level1_std, rank, Disease) %>%
  mutate(label = sprintf("%d. %s", rank, Disease)) %>%
  arrange(Level1_std, rank)

# Build ordered levels: numbered → Other per group
cmnn_levels <- c(
  top_diseases %>% filter(Level1_std == "Communicable, Maternal, Child, Nutrition") %>% pull(label),
  "Other CMNN"
)

ncd_levels <- c(
  top_diseases %>% filter(Level1_std == "Non-communicable") %>% pull(label),
  "Other Non-communicable"
)

inj_levels <- c(
  top_diseases %>% filter(Level1_std == "Injury") %>% pull(label),
  "Other Injury"
)

cmnn_levels <- c(top_diseases %>% filter(Level1_std == "Communicable, Maternal, Child, Nutrition") %>% pull(label), "Other CMNN")
ncd_levels  <- c(top_diseases %>% filter(Level1_std == "Non-communicable") %>% pull(label), "Other Non-communicable")
inj_levels  <- c(top_diseases %>% filter(Level1_std == "Injury") %>% pull(label), "Other Injury")

all_levels <- unique(c(cmnn_levels, ncd_levels, inj_levels))


top_causes_data <- top_causes_data %>%
  mutate(
    cause_display = factor(cause_display, levels = all_levels, ordered = FALSE)
  )
top_causes_data <- top_causes_data %>%
  mutate(
    cause_display = case_when(
      Level1_std == "Communicable, Maternal, Child, Nutrition" & rank <= top_n_df$n[top_n_df$Level1_std == "Communicable, Maternal, Child, Nutrition"] ~ sprintf("%d. %s", rank, Disease),
      Level1_std == "Communicable, Maternal, Child, Nutrition" & rank >  top_n_df$n[top_n_df$Level1_std == "Communicable, Maternal, Child, Nutrition"] ~ "Other CMNN",
      Level1_std == "Non-communicable" & rank <= top_n_df$n[top_n_df$Level1_std == "Non-communicable"] ~ sprintf("%d. %s", rank, Disease),
      Level1_std == "Non-communicable" & rank >  top_n_df$n[top_n_df$Level1_std == "Non-communicable"] ~ "Other Non-communicable",
      Level1_std == "Injury" & rank <= top_n_df$n[top_n_df$Level1_std == "Injury"] ~ sprintf("%d. %s", rank, Disease),
      Level1_std == "Injury" & rank >  top_n_df$n[top_n_df$Level1_std == "Injury"] ~ "Other Injury",
      TRUE ~ Disease
    )
  ) %>%
  mutate(cause_display = factor(cause_display, levels = all_levels))
# 3. Define separate color palettes
library(RColorBrewer)

cols_cmnn <- brewer.pal(9, "Reds")[c(3,4,5,6,7,8,9,2)]   # 8 reds + darker for other if needed
cols_ncd  <- brewer.pal(9, "Blues")[c(3:9,2,8)]           # try to get 13 distinct-ish blues
cols_inj  <- brewer.pal(5, "Greens")[c(3,4,5,2)]          # 4 greens

# Build named vector in the exact order of cause_display levels
color_map <- c(
  setNames(cols_cmnn[1:top_n[["Communicable, Maternal, Child, Nutrition"]]], 
           levels(top_causes_data$cause_display)[1:top_n[["Communicable, Maternal, Child, Nutrition"]]]),
  
  setNames(cols_ncd[1:top_n[["Non-communicable"]]], 
           levels(top_causes_data$cause_display)[(top_n[["Communicable, Maternal, Child, Nutrition"]]+2):(top_n[["Communicable, Maternal, Child, Nutrition"]]+1+top_n[["Non-communicable"]])]),
  setNames(cols_ncd[length(cols_ncd)], "Other Non-communicable"),
  
  setNames(cols_inj[1:top_n[["Injury"]]], 
           levels(top_causes_data$cause_display)[(length(cols_cmnn)+length(cols_ncd)+2):length(levels(top_causes_data$cause_display))-1]),
  setNames(cols_inj[length(cols_inj)], "Other Injury")
)

# 4. Plot ── very similar to reference
p <- ggplot(top_causes_data|>filter(Gender%in%c("male","female")), aes(x = AgeGroup, y = total, fill = cause_display)) +
  geom_col(position = "stack", width = 0.85) +
  facet_grid(Gender ~ ., switch = "y", labeller = labeller(Gender = c(Female = "female", Male = "male"))) +
  scale_fill_manual(values = color_map, name = NULL) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Burden of Disease by Age and Gender (2022–2025)",
    subtitle = "Top causes within CMNN, NCDs and Injuries – others grouped",
    y        = "Reported cases / deaths",
    x        = "Age group",
    caption  = "Data source: KHIS | Top causes shown numbered; others aggregated per group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    legend.text      = element_text(size = 10, margin = margin(b = 2)),
    legend.key.height = unit(0.9, "lines"),
    legend.key.width  = unit(0.6, "lines"),
    axis.text.x      = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y      = element_text(size = 11),
    strip.text       = element_text(face = "bold", size = 14),
    strip.background = element_rect(fill = "grey95"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.title.position = "plot"
  ) +
  guides(fill = guide_legend(ncol = 1, reverse = FALSE))

print(p)

# Save
ggsave("~/Library/CloudStorage/OneDrive-Personal/BoD/plots/top_causes_age_gender_numbered_legend.png",
       plot = p, width = 16, height = 11, dpi = 320)



## Now we do summarises for cases and find the case fatality ratio within that age category in that county over that year
khis_data2a <- khis_data2 |>
  group_by(County, Year, AgeGroup, Gender, Disease,
           `M&MCCoD  Reporting Mode`, FacilityLevel) |>
  summarise(Cases = sum(number, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from  = `M&MCCoD  Reporting Mode`,
    values_from = Cases,
    values_fill = 0        # numeric, not '0'
  )|>
  rename(Deaths=Mortality,
         Cases=Morbidity)|>
  mutate(
    CFR = round(Deaths / (Cases + Deaths), 3),
    Source="KHIS_ind",
    Year=as.integer(Year),
    AgeGroup=factor(AgeGroup,
                    levels = c("0 - 1"  ,
                               "1-4"  ,
                               "5-9" ,
                               "10-14" ,
                               "15-19" ,
                               "20-24",
                               "25-29",
                               "30-34" ,
                               "35-39" ,
                               "40-44" ,
                               "45-49", 
                               "50-54",
                               "55-59" ,
                               "60-64",
                               "65-69" ,
                               "70-74" ,
                               "75-79",
                               "80+" ),
                    ordered = T)
  )


print(sum(khis_data2a$Deaths, na.rm = T))
print(sum(khis_data2a$Cases, na.rm=T))
# ── Join population ────────────────────────────────────────────

khis_df <- khis_data2a |>
  left_join(
    pop_data,
    by = c(
              "Year",
           "AgeGroup",
           "Gender",
           "County"
           )
  )
## Read The TIBU data

tibudata<-readRDS("~/Library/CloudStorage/OneDrive-Personal/BoD/tibu-data.rds")|>
  dplyr::select(- `Health Facility` )|>
  mutate(Disease="Tuberculosis")


## HIV DATA NASCOP nssdc

hiv_df<-vroom::vroom("~/Downloads/merged_data.csv")|>
  janitor::clean_names()|>
  filter(year_diagnosis_clean>2021)
dim(
  hiv_df)
## Mutate new column in hiv_data1 
hiv_data1<-hiv_df|>
  
  mutate(
    cases=1
        )|>
  janitor::clean_names()|>
  filter(year_diagnosis_clean>2021)
glimpse(hiv_data1)
## Some characteristics check  
cat("The total number of facilities/ sites that program is run in the country were:  ", n_distinct(hiv_data1$facility_name))
cat("Total cases reported over the time frame were :  ", nrow(hiv_data1))
tbl <- hiv_data1 |>
  count(year_diagnosis_clean)

cat("The cases distributed by year were:\n",
    paste(tbl$year_diagnosis_clean, ":", tbl$n, collapse = ", "),
    "\n")

## To get the Keph Level we can get from CENSUS 2023 for facilities
library(stringr)
khfa_data<-vroom::vroom("/Volumes/D/Immunization_estimations/Inputs/khfa_data.csv")|>
  
  dplyr::select(kmfl,county,sub_county,keph_level,facility) |>
  
  mutate( 
    facility=str_to_title(facility),
          kmfl=gsub("n_|n","",kmfl),
          kmfl = gsub("[^0-9]", "", kmfl),     # keep digits only
          kmfl = substr(kmfl, 1, 5))|>
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
  )|>
  mutate( kmfl=as.integer(kmfl))|>
  mutate(
    # kmfl=case_when(
    #   facility=="Aga khan syokimau"~"30788",
    #   facility=="Village of hope clinic"~"28369",
    #   facility=="Tenri"~"25212",
    #   facility=="st.veronica medical clinic (yatta)"~"29642",
    #   facility=="St.Crispin Afya Hospital"~"28630",
    #   facility=="Sower Medical Clinic"~"22281",
    #   facility=="Silver Care Medical Clinic LTD"~"32882",
    #   facility=="Shining Hope for Communities Mathare Medical Clinic"~"29280",
    #   facility=="Mother Teresa Rodon medical clinic"~"28342",
    #   facility=="Kosoiywo Dispensary"~"25415",
    #   facility=="Kiptenden Dispensary"~"20535",
    #   facility=="Jesus Responding Clinic"~"14603",
    #   facility=="ivingoni dispensary"~"19891",
    #   facility=="Gitima-ini Dispensary"~"27325",
    #   facility=="Gari Despensary"~"18866",
    #   facility=="GALAXY MEDICAL CENTRE"~"22205",
    #   facility=="Ell borr Dispensary"~"23829",
    #   facility=="Domal Dispensary"~"23819",
    #   facility=="Diani Beach Hospital"~"25357",
    #   facility=="Damsa Dispensary"~"22267",
    #   facility=="Ballah Dispensary"~"20601",
    #   facility=="Agha Khan Health Services (Mtwapa)"~"24036",
    #   facility=="Afya Bora clinic"~'23130',
    #   facility=="ADRA Health Centre"~"23746",
    #   facility=="Meldy Clinic"~"20599",
    #   facility=="Kesegon Dispensary"~"22584",
    #   facility=="Mary Magdalene Medical Centre"~"21460",
    #   facility=="Tudor Health Care (Suba)"~"24993",
    #   facility=="Agakhan Hosp"~"40732",
    #   facility=="Equity Afia Kitui"~"28397",
    #   facility=="AAR Healthcare Rongai"~"28752",
    #   facility=="Uwezo Healthcare HC"~"21076",
    #   facility=="Kianjogu Dispensary"~"10114",
    #   facility=="God's Favour Medical Centre"~"24430",
    #   facility=="Kimunyu Medical Clinic"~'10612',
    #   facility=="Langata Highrise Dispensary"~"28664",
    #   facility=="LAILA MEDICAL CENTRE"~"28556",
    #   facility=="Destiny Medical Centre Ruiru"~"40941",
    #   
    #   TRUE~kmfl
    # ),
    
    facility=case_when(facility=="Damsa Dispensary"~"Damasa Dispensary",
                       facility=="Domal Dispensary"~"Dormal Dispensary",
                       facility=="ADRA Health Centre"~"Midrar Medical Centre",
                       facility=="Meldy Clinic"~"Medisol Clinic",
                       facility=="Kesegon Dispensary"~"Kesogon Dispensary(Cherangany)",
                       facility=="Mary Magdalene Medical Centre"~"Mary Magdaline Medical Clinic",
                       facility=="Tudor Health Care (Suba)"~"Tudor Health Clinic",
                       TRUE~facility))
  
  
## hiv_data1 SUBSET ===
hiv_data2<-hiv_data1|>
  group_by(county,site_code,facility_name,current_age_group,sex,year_diagnosis_clean,is_dead)|>reframe(Cases=sum(cases))|>
  mutate(is_dead=case_when(is_dead=="TRUE"~'Mortality',
                           TRUE~'Morbidity'))|>
  pivot_wider(names_from = is_dead,
              values_from =Cases,
              values_fill = 0)|>
 dplyr:: rename(
      County= county,
      AgeGroup=current_age_group,
      Gender=sex,
      Year=year_diagnosis_clean,
      Cases=Morbidity,
      Deaths=Mortality
    
         )|>
  mutate(
    Gender=str_to_lower(Gender),
    CFR=round(Deaths/(Deaths+Cases),3)
  )|>
  ## Add keph level for the facility
left_join(
  haf_fac,
    
  
  by=c( "site_code"="kmfl")
)
## Health facilities that did not merge well
library(dplyr)
notmerged<-hiv_data2|>
  distinct(site_code,facility_name,.keep_all = T)|>
  filter(is.na(keph_level))|>
  dplyr::select(-c(keph_level,facility))


 dfqa<-notmerged|>
  left_join(
    haf_fac|>
      mutate( kmfl=as.integer(kmfl)),
    by=c("County"= "county","facility_name"="facility")
  )|>
 dplyr:: select(-kmfl)|>
  filter(!is.na(keph_level))
 
 
## Now combine iwth the data 3                                                                                                                                                                                                               ))
hiv_data3<-hiv_data2|>
  filter(!is.na(keph_level))|>
dplyr::  select(-facility) ## Data with full information


## Remove those that are in data set dfqa
dfsq<-notmerged|>
  filter(!site_code%in%dfqa$site_code)|>
  fuzzyjoin::stringdist_left_join(haf_fac, 
                                  by=c("County"="county","facility_name"="facility","site_code"="kmfl"),
                                  max_dist=1,
                                  method = "soundex")|>
  mutate(
  keph_level=case_when(
    facility%in%c(
      "Sagana Subcounty Hospital",
      "Kianyaga Sub-County Hospital",
      "Silvercrest Medcare Hospital",
      "Kianyaga Sub-County Hospital",
      "Kianyaga Sub-County Hospital"
    )~"Level 4",
    facility%in% c( "Rukanga Dispensary","Kiangai Dispensary","Thiba Dispensary",
                             "Njegas Dispensary","Kutus Dispensary",'Kibirigwi Dispensary',
                             "Kerugoya Catholic Dispensary.","Karumandi Dispensary","Kagumo Dispensary",
                             "Gathambi Dispensary","Ciagini Dispensary","Whiterose Medical Centre","Nguka Dispensary",
                             "Kibirigwi Dispensary","Ciagini Dispensary","Kagumo Dispensary","Karumandi Dispensary","Medlife Health Care"
    )~"Level 3",
    
   facility%in%c("Queens and Kings Accurate Health Centre","Nucleaur Medical Diagnostic Centre",
                           "Muchagara Dispensary","Kiangombe Dispensary","Kagio Nursing Home","Joshua Memorial Mbai Dispensary",
                           "Gatwe Dispensary","GK Prison Dispensary (Kerugoya)","agumo Live Giving Dispensary/Laboratory","Gatumbi (SDA) Dispensary",
                           "GK Prison Dispensary (Gathigiriri )","Mbiri (ACK) Dispensary","Thigirichi Mukui","Jesmond (ACK) Dispensary (Mburi)","Kiandangae Dispensary",
                           "Rurii Kiandegwa Dispensary","Thigirichi Mukui","Kiandai Dispensary","Mukangu Dispensary","Vital Fort Medical Center"
    )~"Level 2",
    
    
    TRUE~keph_level
  ))|>
  dplyr::select(
    -c(
      `county.x`,      `county.y`,    facility,     kmfl
    )
  )|>
  filter(!is.na(keph_level))
DFSQ2<-notmerged|>
   filter(!site_code%in%dfsq$site_code)|>
            fuzzyjoin::stringdist_left_join(haf_fac, 
                                            by=c("County"="county","facility_name"="facility"),
                                            max_dist=2,
                                            method = "soundex")|>
  dplyr::select(-facility, -kmfl,-county.x, -county.y)|>
  filter(!is.na(keph_level))
  
DFSQ3<-notmerged|>
  filter(!site_code%in%DFSQ2$site_code)|>
  fuzzyjoin::stringdist_left_join(haf_fac, 
                                  by=c("County"="county","facility_name"="facility"),
                                  max_dist=3,
                                  method = "lv")|>
  dplyr::select(-facility, -kmfl,-county.x, -county.y)|>
  filter(!is.na(keph_level))



dfmis<-bind_rows(dfqa,dfsq,DFSQ2,DFSQ3)

finmiss<-notmerged|>
  filter(!site_code%in%dfmis$site_code)|>
  fuzzyjoin::stringdist_left_join(haf_fac, 
                                  by=c("facility_name"="facility"),
                                  max_dist=2,
                                  method = "lv")|>
  dplyr::select(-facility, -kmfl,-county.x, -county.y)|>
  filter(!is.na(keph_level))


dfmis1<-bind_rows(dfqa,dfsq,DFSQ2,DFSQ3,finmiss)


finmiss1<-notmerged|>
  filter(!site_code%in%dfmis1$site_code)|>
  fuzzyjoin::stringdist_left_join(haf_fac, 
                                  by=c("facility_name"="facility"),
                                  max_dist=3,
                                  method = "lv")|>
  dplyr::select(-facility, -kmfl,-county.x, -county.y)|>
  filter(!is.na(keph_level))
## BIND_ROWs ======
dfs_hiv<-bind_rows(hiv_data3,dfmis1,finmiss1)|>
  mutate(Source="NASCOP")|>
  left_join(
    population |>
      mutate(AgeGroup = case_when(
        Age %in% c("65-69", "70-74", "75-79", "80+") ~ "65+",
        TRUE ~ Age
      ),
      year=as.integer(year)) |>
      dplyr::select(-Age) |>
    dplyr::  rename(
        County     = county,
        Population = population,
         Year       = year,
        Gender     = gender
            )|>
      group_by(County, Year,Gender,AgeGroup)|>
      reframe(Population=sum(Population))|>
      ungroup(),
    by = c("County", "Year" , "Gender","AgeGroup")
  )|>
separate(keph_level, into = c("Level","FacilityLevel"), sep=" ")|>
  dplyr::select(-c(county,Level, site_code,facility_name))|>mutate(Disease="HIV/AIDs")

## Merge with the data from TIBU program and NASCOP DATA 
df1<-bind_rows(khis_df, tibudata,dfs_hiv)
dim(df1)
View(df1)


## Get the summary for each year by county and data source for now it is NSSDC (Nascop) vs Tracker data ==============================
df_hiv <- df1 |>
  mutate(County=case_when(County%in%c("Elgeyo-\nMarakwet",
                                      "Elgeyo \nMarakwet")~"Elgeyo Marakwet",
                          County=="Tharaka-Nithi"~"Tharaka Nithi",
                          County=="Taita-Taveta"~"Taita Taveta",
                          County=="Trans-Nzoia"~"Trans Nzoia",
                          County=="Nairobi City"~"Nairobi",
                          TRUE~County))|>
  filter(grepl("HIV|AIDs|Human immu", Disease, ignore.case = TRUE),
         between(Year,2022,2026)
         ) |>
  group_by(Year, County, Source) |>
  summarise(Cases = sum(Cases, na.rm = TRUE), .groups = "drop") |>
  reshape2::dcast(Year + County ~ Source, value.var = "Cases", fill = 0)|>
  mutate(
    ratio_NASCOP_vs_KHIS = NASCOP / KHIS_ind
    # ratio_Aggregate_vs_KHIS = Aggregate_KHIS / KHIS_tracker
  )


View(df_hiv)

## Get the KHIS Individuals on ART from and merge with the df_hiv to get the distributions


# saveRDS(df1,"~/Library/CloudStorage/OneDrive-Personal/BoD/data_for_analys_9th_march2026.rds")

# |>   # ✅ guard against log(0)
#   mutate(
#     # ✅ Reporting rate per facility level — Level 6 reports less morbidity
#     Reporting = case_when(
#       FacilityLevel == 6 ~ 0.,   # mortality-heavy, low morbidity reporting
#       FacilityLevel == 5 ~ 0.85,
#       FacilityLevel == 4 ~ 0.425,
#       FacilityLevel == 3 ~ 0.12,
#       FacilityLevel == 2 ~ 0.01,
#       TRUE                 ~ 0.50
#     ),
#     # ✅ Referral era — structural break you mentioned at 2024
#     referral_era = if_else(Year >= 2024, "post_referral", "pre_referral"),
#     referral_era = factor(referral_era,
#                           levels = c("pre_referral", "post_referral")),
#     
#     # ✅ Offset = population × reporting rate
#     offset_var   = log(population * Reporting),    # log here, not inside formula
#     Year_scaled  = as.numeric(scale(Year))         # ✅ scale year — helps convergence
#   )
# 
# # ── Disease-gender biological constraints ─────────────────────
# # ✅ Remove biologically impossible combinations
# khis_df <- khis_df |>
#   filter(
#     !(Disease == "Cervical Cancer"  & Gender == "Male"),
#     !(Disease == "Prostate Cancer"  & Gender == "Female"),
#     !(Disease == "Ovarian Cancer"   & Gender == "Male"),
#     !(Disease == "Endometrial Cancer" & Gender == "Female")
#     # add more as needed
#   )

# ── Subset diseases for model fitting ─────────────────────────
diseases_to_fit <- c("Asthma", "Tuberculosis", "Diabetes mellitus")

khis_model_df <- khis_df |>
  filter(Disease %in% diseases_to_fit)                                     # ✅ drop unused factor levels

dim(khis_model_df)

# ── Model formula ──────────────────────────────────────────────
formula <- bf(
  Cases ~
    offset(offset_var) +                           # ✅ pre-computed log(pop × reporting)
    Disease +
    Year_scaled +                                  # ✅ scaled year
    AgeGroup +
    Gender +
    referral_era +                                 # ✅ 2024 structural break
    (1 | County) +                                 # county baseline
    (1 | County:Year) +                            # county × time
    (1 | Disease:County) +                         # disease spatial variation
    (1 | FacilityLevel)                            # facility level variance
)

# ── Priors ─────────────────────────────────────────────────────
priors <- c(
  prior(normal(0, 1),          class = "b"),
  prior(normal(0, 1),          class = "Intercept"),
  prior(student_t(3, 0, 2.5),  class = "sd"),
  # ✅ negbinomial shape — handles overdispersion better than Poisson
  prior(gamma(2, 0.1),         class = "shape")
)

# ── Fit model ──────────────────────────────────────────────────
fit <- brm(
  formula  = formula,
  data     = khis_model_df,
  family   = negbinomial(),                        # ✅ negbinomial > Poisson for count data
  chains   = 4,
  iter     = 4000,
  warmup   = 1500,
  cores    = 4,
  prior    = priors,
  control  = list(adapt_delta = 0.97,              # ✅ higher — multilevel needs this
                  max_treedepth = 12),
  file     = "~/Library/CloudStorage/OneDrive-Personal/BoD/model/fit"                    # ✅ cache model — avoids refitting
)

# ── Model diagnostics ──────────────────────────────────────────
summary(fit)
plot(fit)                                          # trace plots — check convergence
pp_check(fit, ndraws = 100)                        # ✅ posterior predictive check

# ── Extract FacilityLevel random effects ───────────────────────
# ✅ FacilityLevel is random — use ranef(), not grep for b_ fixed effects
facility_re <- ranef(fit)$FacilityLevel |>
  as.data.frame() |>
  rownames_to_column("FacilityLevel") |>
  rename(
    Estimate = Estimate.Intercept,
    lower_95 = Q2.5.Intercept,
    upper_95 = Q97.5.Intercept
  ) |>
  mutate(RateRatio = exp(Estimate),
         RR_lower  = exp(lower_95),
         RR_upper  = exp(upper_95))

# ── Plot facility level rate ratios ───────────────────────────
facility_re |>
  ggplot(aes(x = FacilityLevel, y = RateRatio)) +
  geom_pointrange(aes(ymin = RR_lower, ymax = RR_upper),
                  size = 0.8, color = "#2A9D8F") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  scale_y_continuous(trans = "log10") +
  labs(
    title = "Facility Level Effect on Disease Burden (Rate Ratio)",
    y     = "Rate Ratio — log10 scale",
    x     = "KePH Level"
  ) +
  theme_minimal(base_size = 13)

# ── Posterior fitted values & incidence ───────────────────────
posterior_fitted <- fitted(fit, scale = "response")   # Estimate, Q2.5, Q97.5, Est.Error

khis_data2 <- khis_model_df |>
  mutate(
    est_mean             = round(posterior_fitted[, "Estimate"], 0),
    est_lower            = posterior_fitted[, "Q2.5"],
    est_upper            = posterior_fitted[, "Q97.5"],
    Incidence_per100k    = 1e5 * est_mean   / Population,
    Incidence_lower100k  = 1e5 * est_lower  / Population,
    Incidence_upper100k  = 1e5 * est_upper  / Population
  )

# ── Incidence summary by disease × county × year ──────────────
incidence_summary <- khis_data2 |>
  group_by(Disease, County, Year) |>
  summarise(
    est_cases        = round(sum(est_mean), 0),       # ✅ sum not mean for counts
    incidence_mean   = sum(est_mean)   / sum(Population) * 1e5,
    incidence_lower  = sum(est_lower)  / sum(Population) * 1e5,
    incidence_upper  = sum(est_upper)  / sum(Population) * 1e5,
    .groups          = "drop"
  ) |>
  arrange(Disease, County, Year)

# ── Plot incidence trends ──────────────────────────────────────
incidence_summary |>
  ggplot(aes(x = Year, y = incidence_mean,
             color = County, fill = County, group = County)) +
  geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = incidence_lower, ymax = incidence_upper),
              alpha = 0.15, color = NA) +
  facet_wrap(~ Disease, scales = "free_y") +
  scale_color_viridis_d(option = "turbo") +
  scale_fill_viridis_d(option  = "turbo") +
  labs(
    title    = "Estimated Disease Incidence per 100,000",
    subtitle = "Shaded = 95% Credible Interval",
    y        = "Incidence per 100,000",
    x        = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

View(incidence_summary)

