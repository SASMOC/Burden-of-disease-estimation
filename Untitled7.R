library(brms)
library(tidyr)
library(tidybayes)   # ✅ added — needed for as_draws_df, add_epred_draws
library(dplyr)
library(janitor)
library(rstan)
library(skimr)
library(viridis)
library(scales)
library(forcats)
library(stringr)
library(ggplot2)
options(Cores=8)
gc()
my_colors <- c(
  "#FE7501", "#27AAE1", "#76b5c5", "#063970",
  "#141", "#eab676","#FF5733", "#C70039", "#900C3F", "#581845"
)
# ── Load data ─────────────────────────────────────────────────
khis_data  <- readRDS("~/Library/CloudStorage/OneDrive-Personal/BoD/data_clean6a.rds")
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

# ── Clean KHIS data ───────────────────────────────────────────
khis_data1 <- khis_data |>
  rename(
    County        = county,
    Gender        = gender,
    AgeGroup      = `Age years categorical`,
    Disease       = Level4
  ) |>
  separate(`Keph level`, into = c("levels","FacilityLevel", sep=" "))|>
  mutate(`Organisation unit code`=case_when(`Organisation unit name`=="Ram Medical Clinic"~ 25558,
                                            
                                            `Organisation unit name`=="Progress Medical Services Annex"~17587,
                                            `Organisation unit name`=="Tenri"~25212,
                                            `Organisation unit name`=="Maasai Mara Medical Clinic"~25190,
                                            `Organisation unit name`=="Nyatike Community Health Unit (Migori)"~713236,
                                            `Organisation unit name`=="Manjengo Community Unit"~601643,
                                            `Organisation unit name`=="Texas Cancer Centre (Duplicate)"~26170,
                                            `Organisation unit name`=="Progress Medical Services Annex"~26264,
                                            `Organisation unit name`=="Kosoiywo Dispensary"~25415,
                                            TRUE~`Organisation unit code`
                                            
                                            )
         )
  
  
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
  )|>
  filter(!if_all(13:20, is.na))|>
  rename(Outcome=`M&MCCoD  Reporting Mode`)

print(dim(khis_data2))



## Get the distribution of gender age, in each year for level 2, 3 and 4 over each year

khis_data2 <- khis_data2 |> select(-` `)
# Structure of the dataset
glimpse(khis_data2)

# Summary statistics for numeric columns (if any)
summary(khis_data2)

# quick overview of missingness and types
skim(khis_data2)
# Count missing values per column
missing_counts <- khis_data2 %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing")

print(missing_counts)
khis_data2 %>%

  count(Year) %>%
  ggplot(aes(x = Year, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Total Records by Year", y = "Number of Cases") +
  theme_minimal()


plotq<-khis_data2 %>%
  filter(Year>2021)%>%
  count(Outcome, Year) %>%
  ggplot(aes(x = Outcome, y = n, fill = Outcome)) +
  geom_col() +
  labs(title = "Records by Reporting Mode", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Mortality"="#fe7501",
                               "Morbidity"="#27AAE1"))+
facet_wrap(~Year, scale="free_x", ncol=5)+
  theme( title = element_blank(),
            axis.title = element_text(size = 16, face = "bold"),
         axis.text = element_text(size = 15),
         strip.text = element_text(size = 16, face = "bold"),
         axis.text.x = element_blank(),
         legend.text = element_text(size = 15),
         legend.title = element_blank(),
         legend.position = "bottom")


ggsave(plot=plotq, 
       filename = "~/Library/CloudStorage/OneDrive-Personal/BoD/plots/Cases.png",
       width = 12,
       height = 10, 
       dpi = 300
       )
## Top 20 diseases


topplot<-khis_data2 %>%
  filter(Year > 2021) %>%
  count(Disease, Year) %>%
  group_by(Year) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(Disease_wrapped = str_wrap(Disease, width = 30)) %>%  # wrap at 30 chars
  ggplot(aes(x = n, y = fct_reorder(Disease_wrapped, n))) +
  geom_col(fill = "#27aae1") +
  labs(
    title = "Top 10 Diseases per Year",
    x = "Count",
    y = "Disease"
  ) +
  theme_minimal() +
  facet_wrap(~Year, scales = "free_x", ncol = 5)+
  theme( title = element_blank(),
         axis.title = element_text(size = 16, face = "bold"),
         axis.text.y = element_text(size = 15),
         strip.text = element_text(size = 16, face = "bold"),
       
         legend.text = element_text(size = 15),
         axis.text.x = element_text(angle = 90, size = 15,
                                    vjust = 0.2))
ggsave(plot=topplot, 
       filename = "~/Library/CloudStorage/OneDrive-Personal/BoD/plots/top_20_plot.png",
       width = 12,
       height = 10, 
       dpi = 300
)
  
## Distribution of facilities by ownership and loction
khfa<-data.table::fread("/Volumes/D/Immunization_estimations/Inputs/khfa_data.csv")
  
hfa1<-khfa|> 
  dplyr::select(kmfl,county,sub_county,keph_level,facility,facility_owner, location,catchment_pop,grep("bed|cots", names(khfa))) |>
  
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
  )) |>
  rowwise()|>
  mutate(total_beds=sum(across(10:21)
                      ,na.rm = T),
         
         total_beds1=case_when(total_beds>2500~functional_general_beds,
                              TRUE~total_beds),
         
         total_beds1=case_when(facility%in%c("Pastor Machage Memorial Hospital",
                                             "Queenteric Annex Hospital")~sum(across(11:20)),
                               TRUE~total_beds1)
         )


## Distribution of the beds and cots  in  health facility by keph level 
sum_fac_keph_level<-hfa1|>
  group_by(
    keph_level
  )|>
  summarise(
    beds=sum(total_beds1,na.rm = T),
    .groups = "keep"
  )

print(sum_fac_keph_level)

khis_hfa1<-khis_data2|>
  left_join(hfa1|>
              mutate(kmfl=as.numeric(kmfl))|>
              mutate(
             kmfl=case_when(
                   facility=="Aga khan syokimau"~30788,
                  facility=="Village of hope clinic"~28369,
                 facility=="Tenri"~25212,
               facility=="st.veronica medical clinic (yatta)"~29642,
                 facility=="St.Crispin Afya Hospital"~28630,
                 facility=="Sower Medical Clinic"~22281,
                  facility=="Silver Care Medical Clinic LTD"~32882,
               facility=="Shining Hope for Communities Mathare Medical Clinic"~29280,
                 facility=="Mother Teresa Rodon medical clinic"~28342,
                 facility=="Kosoiywo Dispensary"~25415,
                  facility=="Kiptenden Dispensary"~20535,
                 facility=="Jesus Responding Clinic"~14603,
              facility=="ivingoni dispensary"~19891,
               facility=="Gitima-ini Dispensary"~27325,
                facility=="Gari Despensary"~18866,
                 facility=="GALAXY MEDICAL CENTRE"~22205,
                 facility=="Ell borr Dispensary"~23829,
                facility=="Domal Dispensary"~23819,
                 facility=="Diani Beach Hospital"~25357,
                   facility=="Damsa Dispensary"~22267,
                 facility=="Ballah Dispensary"~20601,
                  facility=="Agha Khan Health Services (Mtwapa)"~24036,
                  facility=="Afya Bora clinic"~23130,
                  facility=="ADRA Health Centre"~23746,
                  facility=="Meldy Clinic"~20599,
                  facility=="Kesegon Dispensary"~22584,
                  facility=="Mary Magdalene Medical Centre"~21460,
                  facility=="Tudor Health Care (Suba)"~24993,
                  facility=="Agakhan Hosp"~40732,
                   facility=="Equity Afia Kitui"~28397,
                   facility=="AAR Healthcare Rongai"~28752,
                   facility=="Uwezo Healthcare HC"~21076,
                   facility=="Kianjogu Dispensary"~10114,
                   facility=="God's Favour Medical Centre"~24430,
                   facility=="Kimunyu Medical Clinic"~10612,
                   facility=="Langata Highrise Dispensary"~28664,
                   facility=="LAILA MEDICAL CENTRE"~28556,
                   facility=="Destiny Medical Centre Ruiru"~40941,
                    facility=="Abicom"~30687, 
              facility=="New Hope Medical Centre (Machakos)"~31386,
              facility=="Yala Subcounty Hospital (Siaya)"~14175,
              
              TRUE~kmfl
                 ),
                
                facility=case_when(facility=="Damsa Dispensary"~"Damasa Dispensary",
                                   facility=="Domal Dispensary"~"Dormal Dispensary",
                                   facility=="ADRA Health Centre"~"Midrar Medical Centre",
                                   facility=="Meldy Clinic"~"Medisol Clinic",
                                   facility=="Kesegon Dispensary"~"Kesogon Dispensary(Cherangany)",
                                   facility=="Mary Magdalene Medical Centre"~"Mary Magdaline Medical Clinic",
                                   facility=="Tudor Health Care (Suba)"~"Tudor Health Clinic",
                                   TRUE~facility))|>
              
            dplyr::  select(-c(8:24)),
            by=c("Organisation unit code"="kmfl")
              )
# check the missing locations and ownership
hf<-readxl::read_excel("~/Library/CloudStorage/OneDrive-Personal/Documents/facilities_classified.xlsx")

khis_hfa2 <- khis_hfa1 %>%
  left_join(
    hf %>%
     dplyr:: select(`Organisation unit name`, `Organisation unit code`, location, facility_owner),
    by = c("Organisation unit name", "Organisation unit code")
  ) %>%
  mutate(
    location = coalesce(location.x, location.y),
    facility_owner = coalesce(facility_owner.x, facility_owner.y)
  ) %>%
  dplyr::select(-location.x, -location.y, -facility_owner.x, -facility_owner.y)|>
  mutate(
    facility_owner=case_when(
      facility_owner%in%c("Faith Based",
                          "Faith-based")~"Faith-Based Organization(FBOs)",
      facility_owner%in%c("Non-Governmental Organization")~"Non-Governmental Organization(NGOs)",
      facility_owner%in%c(
        "Government",
        "Government/Public Entity"
      )~"Public/Government",
      
      TRUE~facility_owner
    )
    
  )


## Get the summary distributions of facilities by ownerships and location

fac_summary <- khis_hfa2 %>%
  distinct(`Organisation unit code`, Year, .keep_all = TRUE) %>%
  filter(Year > 2021) %>%
  count(location, Year, facility_owner) %>%
  
  # convert to wide
  pivot_wider(
    names_from = facility_owner,
    values_from = n,
    values_fill = 0
  ) %>%
  
  # total facilities per group
  mutate(
    total = `Faith-Based Organization(FBOs)`+`Non-Governmental Organization(NGOs)`+ Private +`Public/Government`
  ) %>%
  filter(!is.na(location))|>
  # proportions (VERY IMPORTANT)
  mutate(
    prop_gov = `Public/Government`/ total,
    prop_private = Private / total,
    prop_fbo = `Faith-Based Organization(FBOs)` / total,
    prop_NGOs=`Non-Governmental Organization(NGOs)`/total
  )
## The plot for it is 
owner_levels <- c(
  "Public/Government",
  "Private",
  "Faith-Based Organization(FBOs)",
  "Non-Governmental Organization(NGOs)"
)
loc_plot1<-khis_hfa2 %>%
  distinct(`Organisation unit code`, Year, .keep_all = TRUE) %>%
  filter(Year > 2021) %>%
  count(location, Year, facility_owner) %>%
  group_by(location, Year) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  na.omit()|>
  
  ggplot(aes(x = factor(Year), y = prop, fill =facility_owner)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = percent(prop, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 7,
    color = "white"
  )+
  
  facet_wrap(~location) +
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Distribution of Health Facilities by Ownership and Location (2022–Present)",
    x = "Year",
    y = "Proportion of Facilities",
    fill = "Ownership"
   
  ) +
  
  theme_minimal()+
  scale_fill_manual(values = my_colors,breaks = owner_levels)+

theme(
  
  title = element_blank(),
       axis.title = element_text(size = 16, face = "bold"),
       axis.text = element_text(size = 15),
       strip.text = element_text(size = 16, face = "bold"),
       # axis.text.x = element_blank(),
       legend.text = element_text(size = 16),
       # legend.title = element_blank(),
       legend.position = "bottom"
       )



owner_level <- c(
 

  "Non-Governmental Organization",
  "Faith Based",
  "Private",
  "Government/Public Entity"
  
)
loc_plot2<-hfa1%>%


  count(location,facility_owner) %>%
  group_by(location) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  na.omit()|>
  mutate(facility_owner = factor(facility_owner, levels = owner_level, ordered = TRUE)) %>%
  
  ggplot(aes(x=location, y = prop, fill=facility_owner)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = percent(prop, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    size = 7,
    color = "white"
  )+
  
  
  scale_y_continuous(labels = scales::percent_format()) +
  
  labs(
    title = "Distribution of Health Facilities by Ownership and Location",
    x = "Kenya Health Facility Census: 2023",
    y = "Proportion of Facilities",
    fill = "Ownership",
    caption = "Source: KHIS individual tracker data| KHFA 2023"
  ) +
  
  theme_minimal()+
  
  scale_fill_manual(values =c(`Government/Public Entity`="#fe7501",Private="#27aae1",`Faith Based` = "#063970",`Non-Governmental Organization`="#76b5c5"), 
                    breaks = owner_level)+
  
  theme(
    plot.title = element_blank(),
    axis.title.y =element_blank(),
    axis.text.x = element_text(size=18, face="bold"),
    axis.text.y = element_blank(),
    # axis.ticks.x = element_text(),
    strip.text = element_text(size = 16, face = "bold"),
    # axis.line.x.top = element_line(color = "black", size = 0.5),
    # axis.ticks.x.top = element_line(color = "black", size = 0.5),
    axis.text.x.top = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16, hjust = 0),
    legend.title =element_blank(),
    legend.position = "none",
    
    plot.caption = element_text(size = 12, face = "bold", hjust = 2
                              )
  ) +
  scale_x_discrete(position = "top")
library(patchwork)
loc_plot<-loc_plot1+loc_plot2
ggsave(plot=loc_plot,
       filename = "~/Library/CloudStorage/OneDrive-Personal/BoD/plots/loc_plot.png",
       width = 14,
       height = 12,
       dpi = 300)

# hfak<-khis_hfa1|>
#   filter(is.na(location))|>
#   distinct(`Organisation unit name`, `Organisation unit code`,location, facility_owner)
# data.table::fwrite(hfak,"hfak missing.csv")

## Morbidity vs Mortality by Disease



plot_data <- khis_hfa2 %>%
  # filter(Disease %in% top_diseases) %>%
  count(Disease, Outcome,Year) %>%
  group_by(Disease, Year) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(Disease = fct_reorder(Disease, -total, .desc = TRUE))

ggplot(plot_data, aes(x = n, y =Outcome)) +
  geom_col(position = "dodge") +
  labs(title = "Top Diseases: Morbidity vs Mortality", x = "Count") +
  theme_minimal() +
  scale_fill_viridis_d()
## the case fatalities being

# Compute CFR for top diseases
cfr_data <- khis_hfa2 %>%
  # filter(Disease %in% top_diseases) %>%
  count(Disease, Outcome, Year) %>%
  pivot_wider(names_from = Outcome, values_from = n, values_fill = 0) %>%
  mutate(
    total = Morbidity + Mortality,
    CFR = Mortality / total
  ) %>%
  arrange(desc(total)) %>%
  mutate(Disease = fct_reorder(Disease, -CFR, .desc = TRUE))  # keep descending order of total cases

# View the CFR table
print(cfr_data)

# Plot CFR as a bar chart
ggplot(cfr_data, aes(x = CFR, y = Disease)) +
  geom_col(fill = "coral", width = 0.7) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title = "Case Fatality Ratio (CFR) for Top Diseases",
    subtitle = "Deaths as a percentage of total reported cases",
    x = "Case Fatality Ratio (%)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10)
  )

# Calculate proportion within each year and level
khis_prop <- khis_data2 |>
  group_by(Year, Level1,Gender, AgeGroup) |>
  summarise(cases = sum(number, na.rm = TRUE), .groups = "drop_last") |>
  mutate(proportion = cases / sum(cases) * 100)

## Gender distribution


# Prepare summary with percentages
gender_summary <- khis_hfa2 %>%
  filter(Year > 2021,
         !Gender %in% c("intersex", NA),
         !is.na(Gender),
         !is.na(location)) %>%
  count(Year, location, Outcome, Gender) %>%
  group_by(Year, location, Outcome) %>%
  mutate(pct = n / sum(n),
         pct_label = if_else(pct > 0.05, sprintf("%.0f%%", pct*100), "")) %>%   # label bigger slices
  ungroup()

# Plot - 100% stacked bars, facet by Outcome & location (or vice versa)
ggplot(gender_summary, aes(x = factor(Year), y = pct, fill = Gender)) +
  geom_col(position = "fill") +                    # or "stack" if you want absolute heights
  geom_text(aes(label = pct_label), 
            position = position_fill(vjust = 0.5), 
            color = "white", fontface = "bold", size = 4) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = my_colors, breaks = c("male", "female")) +
  facet_grid(Outcome ~ location, switch = "y") +   # or facet_wrap(~Outcome + location, ncol = 3)
  labs(
    title    = "Gender Distribution of Records (2022–Feb 2026)",
    subtitle = "Proportions by Year, Location, and Outcome",
    x        = "Year",
    y        = "Percentage of records",
    fill     = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text       = element_text(size = 16, face = "bold"),
    axis.text        = element_text(size = 15, face = "bold"),
    legend.position  = "top",
    panel.grid.major.x = element_blank()
  )

ggplot(gender_summary, aes(x = Year, y = pct, color = Gender, group = Gender)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = my_colors) +
  facet_grid(Outcome ~ location, scales = "free_y", switch = "both") +
  labs(
    title = "Gender Proportions Over Time by Location and Outcome",
    y = "Percentage male / female",
    x = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")



library(ggalluvial)

ggplot(gender_summary,
       aes(axis2 = location,  axis1 = Gender,axis3 = Outcome, y = n)) +
  geom_alluvium(aes(fill = Gender)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c( "Gender","location", "Outcome"), expand = c(0.1, 0.1)) +
  facet_wrap(~ Year, scales = "free_x", ncol=5) +
  scale_fill_manual(values = my_colors) +
  theme_minimal() +
  labs(title = "Flow of Records: Location → Outcome → Gender")+
  
  theme(
    plot.title = element_blank(),
    axis.title.y =element_blank(),
    axis.text.x = element_text(size=18, face="bold"),
    axis.text.y = element_blank(),
    # axis.ticks.x = element_text(),
    strip.text = element_text(size = 16, face = "bold"),
    # axis.line.x.top = element_line(color = "black", size = 0.5),
    # axis.ticks.x.top = element_line(color = "black", size = 0.5),
    axis.text.x.top = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 16, hjust = 0),
    legend.title =element_blank(),
    legend.position = "none",
    
    plot.caption = element_text(size = 12, face = "bold", hjust = 2
    )
  )




## Gender distribution overall
ggplot(khis_prop, aes(x = Year, y = AgeGroup, fill = proportion)) +
  geom_tile() +
  facet_grid( Level1 ~ Gender) +
  scale_fill_viridis_c() +
  labs(title = "Proportion of Cases by Gender, Age Group, and GBD Level 2",
       fill = "%") +
  theme_minimal()
## Age distribution overall
khis_hfa2 %>%
  count(AgeGroup) %>%
  ggplot(aes(x = AgeGroup, y = n)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Records by Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Top 20 counties in terms of reporting

khis_hfa2 %>%
  count(County, sort = TRUE) %>%
  # head(20) %>%
  ggplot(aes(x = n, y = fct_reorder(County, n))) +
  geom_col(fill = "#fe7501") +
  labs(title = "Distribution of records by counties over the five years", x = "Count", y = "County") +
  theme_minimal()+
  geom_text(aes(label = n,family = "serif"))+
  theme(
    plot.title = element_blank(),
    axis.title.y =element_blank(),
    axis.text.x = element_text(size=16, face="bold"),
    axis.text.y = element_text(size=16),
  
    strip.text = element_text(size = 16, face = "bold"),
 
    legend.text = element_text(size = 16, hjust = 0),
    legend.title =element_blank(),
    legend.position = "none",
  
    plot.caption = element_text(size = 12, face = "bold", hjust = 2
    )
  )
## Get the disrtibution by population density
library(tidytext)

plot_fun <- function(data, level_name) {
  data |>
    filter(Level1 == level_name) |>
    
    ggplot(
      aes(
        x = cases_per_100k,
        y = reorder_within(County, cases_per_100k, Year),
        fill = category
      )
    ) +
    geom_col() +
    scale_y_reordered() +
    
    scale_fill_manual(
      values = c(
        "High (>200)" = "red",
        "Medium (50–200)" = "orange",
        "Low (<50)" = "#27aae1"
      ),
      name = "Cases per 100k"
    ) +
    
    labs(
      x = "Cases per 100,000",
      y = " ",
      title = level_name
    ) +
    
    facet_wrap(~Year, scales = "free_y", ncol=5)+
    theme(axis.title = element_text(size = 16, face = "bold"),
          legend.position = "top",
          axis.text = element_text(size = 15),
          strip.text = element_text(size = 18, face = "bold")
          )
}
## Plot data preprocessing

plot_data <- khis_hfa2 |>
  filter(!str_detect(Level1, "Extension Codes"),
         Year > 2021, Year < 2026) |>
  count(County, Level1, Year) |>
  ungroup() |>
  
  left_join(
    pop_data |>
      filter(Year >= 2022 & Year < 2026) |>
      group_by(County, Year) |>
      reframe(po = sum(Population)) |>
      ungroup(),
    by = c("County", "Year")
  ) |>
  
  mutate(
    cases_per_100k = n / po * 1e+05,
    
    category = case_when(
      cases_per_100k > 200 ~ "High (>200)",
      cases_per_100k > 50 & cases_per_100k <= 200 ~ "Medium (50–200)",
      TRUE ~ "Low (<50)"
    ),
    
    category = factor(
      category,
      levels = c("High (>200)", "Medium (50–200)", "Low (<50)"),
      ordered = TRUE
    )
  )

## Preparing plots for MCNN, NCDs and Injury
plot_comm <- plot_fun(plot_data, "Communicable,Maternal, Child, Nutrition")
plot_noncomm <- plot_fun(plot_data, "Non-communicable")
plot_injury <- plot_fun(plot_data, "Injury")
## Saving the plots
ggsave(plot = plot_comm, 
       filename = "~/Library/CloudStorage/OneDrive-Personal/BoD/plots/communicable disease.png",
       width = 13,
       height = 15,
       dpi = 300)
ggsave(plot = plot_noncomm, 
       filename = "~/Library/CloudStorage/OneDrive-Personal/BoD/plots/non-communicable disease.png",
       width = 13,
       height = 15,
       dpi = 300)
ggsave(plot = plot_injury ,
       filename = "~/Library/CloudStorage/OneDrive-Personal/BoD/plots/plot_injury.png",
       width = 13,
       height = 15,
       dpi = 300)

plot_comm / plot_noncomm / plot_injury


## 
khis_hfa2 |>
  filter(!str_detect(Level1, "Extension Codes")& Year>2021&Year<2026) |>
  count(County, Level1,Year)|>
  ungroup() |>
  
  ## Attach the population
  left_join(
    pop_data |>
      filter(Year>=2022 & Year<2026) |>
      group_by(County,Year) |>
      reframe(po = sum(Population)) |>
      ungroup(),
    by = c("County",
           "Year")
  ) |>
  
  mutate(
    cases_per_100k = n / po * 1e+05,
    
    ## Category thresholds
    category = case_when(
      cases_per_100k > 100 ~ "High (>100)",
      cases_per_100k > 50&cases_per_100k<=100 ~ "Medium (50–100)",
      TRUE ~ "Low (<50)"
    ),
    
    category = factor(
      category,
      levels = c("High (>100)", "Medium (50–100)", "Low (<50)"),
      ordered = TRUE
    ),
    Level1=factor(Level1, levels = c("Communicable,Maternal, Child, Nutrition","Non-communicable","Injury"), ordered = TRUE)
  ) |>
  
  ggplot(
    aes(
      x = cases_per_100k,
      y = reorder_within(County, cases_per_100k, Level1),
      fill = category
    )
  ) +
  geom_col() +
  
  ## Fix axis labels after reorder_within
  scale_y_reordered() +
  
  scale_fill_manual(
    values = c(
      "High (>100)" = "red",
      "Medium (50–100)" = "orange",
      "Low (<50)" = "#27aae1"
    ),
    name = "Cases per 100k"
  ) +
  
  labs(
    x = "Cases per 100,000",
    y = " "
  ) +
  
  facet_wrap(~Level1+Year, scales = "free_y")

## Univariable analysis

# Identify top 10 diseases overall
top_diseases <- khis_hfa2 %>%
  count(Disease, sort = TRUE) %>%
  head(20) %>%
  pull(Disease)

khis_hfa2 %>%
  filter(Disease %in% top_diseases) %>%
  count(Disease, Gender) %>%
  ggplot(aes(x = n, y = Disease, fill = Gender)) +
  geom_col(position = "dodge") +
  labs(title = "Top 20 Diseases by Gender", x = "Count") +
  theme_minimal() +
  scale_fill_viridis_d()

top_diseases1 <- khis_hfa2 %>%
  count(Disease,Outcome, sort = TRUE) %>%
  head(20) %>%
  pull(Disease)

khis_hfa2 %>%
  filter(Disease %in% top_diseases1) %>%
  count(Disease, Gender) %>%
  ggplot(aes(x = n, y = Disease, fill = Gender)) +
  geom_col(position = "dodge") +
  labs(title = "Top 20 Diseases by Gender", x = "Count") +
  theme_minimal() +
  scale_fill_viridis_d()

top5_diseases <- khis_data2 %>%
  count(Disease, sort = TRUE) %>%
  head(5) %>%
  pull(Disease)

khis_data2 %>%
  filter(Disease %in% top5_diseases) %>%
  count(Year, Disease) %>%
  filter((Year>2019))|>
  ggplot(aes(x = Year, y = n, color = Disease, group = Disease)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Trend of Top 5 Diseases", y = "Number of Cases") +
  theme_minimal() +
  scale_color_viridis_d()
## Age pyramid for specific disease
disease_of_interest <- "Pneumonia"   # replace with actual disease name

pyramid_data <- khis_data2 %>%
  filter(Disease == disease_of_interest) %>%
  count(Year, Gender, AgeGroup) %>%
  mutate(
    cases_plot = ifelse(Gender == "male", -n, n)
  )

ggplot(pyramid_data, aes(x = AgeGroup, y = cases_plot, fill = Gender)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~Year) +
  scale_y_continuous(labels = abs) +
  labs(title = paste(disease_of_interest, "Age-Sex Pyramid"),
       y = "Number of Cases", x = "Age Group") +
  theme_minimal() +
  scale_fill_manual(values = c("female" = "#e41a1c", "male" = "#377eb8"))

## Geographical distribution

# For a specific disease, show cases by county over years
khis_data2 %>%
  filter(Disease == disease_of_interest) %>%
  count(Year, County) %>%
  ggplot(aes(x = Year, y = County, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = paste(disease_of_interest, "Cases by County and Year"),
       fill = "Count") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))


## # Proportion of missing gender or age per county/year
khis_data2 %>%
  group_by(County, Year) %>%
  summarise(
    missing_gender = mean(is.na(Gender) | Gender == "") * 100,
    missing_age = mean(is.na(AgeGroup)) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(cols = starts_with("missing"), names_to = "variable", values_to = "percent") %>%
  ggplot(aes(x = Year, y = County, fill = percent)) +
  geom_tile() +
  facet_wrap(~variable) +
  scale_fill_viridis_c() +
  labs(title = "Missing Data by County and Year", fill = "% missing") +
  theme_minimal()

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

tibudata<-readRDS("~/Library/CloudStorage/OneDrive-Personal/BoD/tibu-data.rds")
## Merge with the data from TIBU program
df1<-bind_rows(khis_df, tibudata)




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