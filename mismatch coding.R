# Load Required Libraries
pacman::p_load(ggplot2, ggthemes, scales, tidyverse, readxl, dplyr, lubridate, rlang, EpiCurve, ISOweek, writexl, openxlsx)

library(ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(rlang)
library(scales)
library(EpiCurve)
library(ISOweek)
library(writexl)
library(openxlsx)

#load dataset

# Create a new table with only the selected columns
site_table <- Measles_WW[, c("RECORD_ID", "SAM_SITE_ID", "SITE_NAME", 
                       "SITE_LONGITUDE", "SITE_LATITUDE", "SITE_PROV", "DISTRICT_NAME", "LOCAL_MUNICIPALITY", "OTHER_DISTRICT")]

# View the new table
head(site_table)
view(site_table)

# Summarize districts per site
site_summary <- site_table %>%
  group_by(SITE_NAME) %>%
  summarize(DISTRICT_NAME = paste(unique(DISTRICT_NAME), collapse = ", "),
            n_districts = n_distinct(DISTRICT_NAME)) %>%
  arrange(desc(n_districts))

site_summary

# Summarize site info
site_summary <- site_table %>%
  group_by(SITE_NAME) %>%
  summarize(
    DISTRICT_NAME = paste(unique(DISTRICT_NAME), collapse = ", "),
    LOCAL_MUNICIPALITY = paste(unique(LOCAL_MUNICIPALITY), collapse = ", "),
    LONGITUDE = paste(unique(SITE_LONGITUDE), collapse = ", "),
    LATITUDE = paste(unique(SITE_LATITUDE), collapse = ", "),
    n_districts = n_distinct(DISTRICT_NAME),
    .groups = "drop"
  ) %>%
  arrange(desc(n_districts))

# View more than 50 rows in console
options(dplyr.print_max = 100)  # or whatever number you want
site_summary

# Check for sites with multiple districts
site_check <- site_table %>%
  group_by(SITE_NAME) %>%
  summarize(
    n_districts = n_distinct(DISTRICT_NAME),
    districts = paste(unique(DISTRICT_NAME), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(status = ifelse(n_districts == 1, "one match", ">1 match")) %>%
  arrange(desc(n_districts))

# View all sites
site_check

# View only mismatches
site_check %>% filter(status == ">1 match")

write_xlsx(site_table, "C:/Users/NtsakoM/Documents/site_table.xlsx")

# Get the names of sites with mismatches
mismatch_names <- site_check %>%
  filter(status == ">1 match") %>%
  pull(SITE_NAME)

# Extract full details from the original site_table
mismatched_sites <- site_table %>%
  filter(SITE_NAME %in% mismatch_names) %>%
  select(SITE_NAME, DISTRICT_NAME, LOCAL_MUNICIPALITY, SITE_LONGITUDE, SITE_LATITUDE)

# View the results
mismatched_sites

# Get site names with mismatched districts
mismatch_names <- site_check %>%
  filter(status == ">1 match") %>%
  pull(SITE_NAME)

# Pull all rows from the original data for mismatched sites
mismatched_sites <- site_table %>%
  filter(SITE_NAME %in% mismatch_names) %>%
  select(SITE_NAME, DISTRICT_NAME, LOCAL_MUNICIPALITY, SITE_LONGITUDE, SITE_LATITUDE)

# View the mismatched rows
mismatched_sites

# Arrange mismatched sites by site name
mismatched_sites <- mismatched_sites %>%
  arrange(SITE_NAME)

# View the results
mismatched_sites

write.xlsx(mismatched_sites, "C:/Users/NtsakoM/Documents/mismatched_sites_sorted.xlsx" rowNames = FALSE)

#-----------------------------
# Step 1: Identify sites with multiple districts
measles_ww_check <- Measles_WW %>%
  group_by(SITE_NAME) %>%
  summarize(
    n_districts = n_distinct(DISTRICT_NAME),
    districts = paste(unique(DISTRICT_NAME), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(status = ifelse(n_districts == 1, "one match", ">1 matches")) %>%
  arrange(desc(n_districts))

# Step 2: Extract only the mismatched site names
mismatch_names <- measles_ww_check %>%
  filter(status == ">1 matches") %>%
  pull(SITE_NAME)

# Step 3: Create a new dataset with only mismatched rows from measles_ww
measles_ww_mismatch <- Measles_WW %>%
  filter(SITE_NAME %in% mismatch_names)

# Step 4: View or save
View(measles_ww_mismatch)

n_distinct(Measles_WW$SITE_NAME)

unique_sites <- unique(Measles_WW$SITE_NAME)
unique_sites

site_counts <- Measles_WW %>%
  group_by(SITE_NAME) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

site_counts
write.xlsx(site_counts, "C:/Users/NtsakoM/Documents/site_counts.xlsx")
write.xlsx(site_counts, "C:/Users/NtsakoM/Downloads/site_counts.xlsx", rowNames = FALSE)

#----------------------------------------------
names(Measles_WW)

#OR Tambo International Airport
# Create a subset with only the wrong district entries for OR Tambo
Measles_WW_wrong_tambo <- Measles_WW %>%
  filter(
    SITE_NAME == "OR Tambo International Airport" &
      tolower(trimws(DISTRICT_NAME)) != "ekurhuleni mm"
  )

# View the results
View(Measles_WW_wrong_tambo)


#Daspoort Wastewater Treatment Works
# Create a subset with only the wrong district entries for Daspoort
Measles_WW_wrong_daspoort <- Measles_WW %>%
  filter(
    SITE_NAME == "Daspoort Wastewater Treatment Works" &
      tolower(trimws(DISTRICT_NAME)) != "tshwane mm"
  )

# View results
View(Measles_WW_wrong_daspoort)

#Olifantsfontein
# Create a subset with only the wrong district entries for Olifantsfontein
Measles_WW_wrong_olifantsfontein <- Measles_WW %>%
  filter(
    SITE_NAME == "Olifantsfontein" &
      tolower(trimws(DISTRICT_NAME)) != "ekurhuleni mm"
  )

# View results
View(Measles_WW_wrong_olifantsfontein)

#Atteridgeville Train Station
# Create a subset with only the wrong district entries for Atteridgeville Train Station
Measles_WW_wrong_atteridgeville <- Measles_WW %>%
  filter(
    SITE_NAME == "Atteridgeville Train Station" &
      tolower(trimws(DISTRICT_NAME)) != "tshwane mm"
  )

# View results
View(Measles_WW_wrong_atteridgeville)

#Midstream
# Create a subset with only the wrong district entries for Midstream
Measles_WW_wrong_midstream <- Measles_WW %>%
  filter(
    SITE_NAME == "Midstream" &
      tolower(trimws(DISTRICT_NAME)) != "ekurhuleni mm"
  )

# View results
View(Measles_WW_wrong_midstream)

#Sasol Garage
# Create a subset with only the wrong district entries for Sasol Garage
Measles_WW_wrong_sasol <- Measles_WW %>%
  filter(
    SITE_NAME == "Sasol Garage" &
      tolower(trimws(DISTRICT_NAME)) != "ekurhuleni mm"
  )

# View the results
View(Measles_WW_wrong_sasol)

#Tembisa Mall
# Create a subset with only the wrong district entries for Tembisa Mall
Measles_WW_wrong_tembisa_mall <- Measles_WW %>%
  filter(
    SITE_NAME == "Tembisa Mall" &
      tolower(trimws(DISTRICT_NAME)) != "ekurhuleni mm"
  )

# View the results
View(Measles_WW_wrong_tembisa_mall)

#namane drive
Measles_WW_wrong_namane <- Measles_WW %>%
  filter(
    SITE_NAME == "Namane Drive" &
      tolower(trimws(DISTRICT_NAME)) != "ekurhuleni mm"
  )

# View the results
View(Measles_WW_wrong_namane)

#moloreng
Measles_WW_wrong_moloreng <- Measles_WW %>%
  filter(
    SITE_NAME == "Moloreng" &
      tolower(trimws(DISTRICT_NAME)) != "ekurhuleni mm"
  )

# View the results
View(Measles_WW_wrong_moloreng)

#hartebeesfontein
Measles_WW_wrong_hartebeesfontein_waterworks <- Measles_WW %>%
  filter(
    SITE_NAME == "Hartebeesfontein Waterworks" &
      tolower(trimws(DISTRICT_NAME)) != "ekurhuleni mm"
  )

# View the results
View(Measles_WW_wrong_hartebeesfontein_waterworks)

#Bloemspruit Wastewater Treatment Works
Measles_WW_wrong_bloemspruit_wastewater_treatment_works <- Measles_WW %>%
  filter(
    SITE_NAME == "Bloemspruit Wastewater Treatment Works" &
      tolower(trimws(DISTRICT_NAME)) != "mangaung mm"
  )

# View the results
View(Measles_WW_wrong_hartebeesfontein_waterworks)

#Rooiwal Wastewater Treatment Works
Measles_WW_wrong_rooiwal_wastewater_treatment_works <- Measles_WW %>%
  filter(
    SITE_NAME == "Rooiwal Wastewater Treatment Works" &
      tolower(trimws(DISTRICT_NAME)) != "tshwane mm"
  )

# View the results
View(Measles_WW_wrong_rooiwal_wastewater_treatment_works)

#central wastewater
# Create a subset with only the wrong district entries for Central Wastewater Treatment Works (KZN)
Measles_WW_wrong_central <- Measles_WW %>%
  filter(
    SITE_NAME == "Central Wastewater Treatment Works (KZN)" &
      tolower(trimws(DISTRICT_NAME)) != "ethekwini mm"
  )

# View the results
View(Measles_WW_wrong_central)

#goudkoppies wastewater
# Create a subset with only the wrong district entries for Goudkoppies Wastewater Treatment Works
Measles_WW_wrong_goudkoppies <- Measles_WW %>%
  filter(
    SITE_NAME == "Goudkoppies Wastewater Treatment Works" &
      tolower(trimws(DISTRICT_NAME)) != "johannesburg mm"
  )

# View the results
View(Measles_WW_wrong_goudkoppies)

#tembisa hospital east and west
Measles_WW_wrong_tembisa_east_west <- Measles_WW %>%
  filter(
    SITE_NAME == "Tembisa Hospital East and West" &
      tolower(trimws(DISTRICT_NAME)) != "ekurhuleni mm"
  )

# View the results
View(Measles_WW_wrong_tembisa_east_west)

#bushkoppies inlet
Measles_WW_wrong_bushkoppies_inlet <- Measles_WW %>%
  filter(
    SITE_NAME == "Bushkoppies Inlet" &
      tolower(trimws(DISTRICT_NAME)) != "johannesburg mm"
  )

# View the results
View(Measles_WW_wrong_bushkoppies_inlet)

#northern wastewater
Measles_WW_wrong_northern_wastewater <- Measles_WW %>%
  filter(
    SITE_NAME == "Northern Wastewater Treatment Works (GP)" &
      tolower(trimws(DISTRICT_NAME)) != "johannesburg mm"
  )

# View the results
View(Measles_WW_wrong_northern_wastewater)

#motsoaledi
Measles_WW_wrong_motsoaledi <- Measles_WW %>%
  filter(
    SITE_NAME == "Motsoaledi" &
      tolower(trimws(DISTRICT_NAME)) != "johannesburg mm"
  )

# View the results
View(Measles_WW_wrong_motsoaledi)

#rustenburg wastewater
Measles_WW_wrong_rustenburg <- Measles_WW %>%
  filter(
    SITE_NAME == "Rustenburg Wastewater Treatment Works" &
      tolower(trimws(DISTRICT_NAME)) != "bojanala platinum dm"
  )

# View the results
View(Measles_WW_wrong_rustenburg)

#boitekong
Measles_WW_wrong_boitekong <- Measles_WW %>%
  filter(
    SITE_NAME == "Boitekong" &
      tolower(trimws(DISTRICT_NAME)) != "bojanala platinum dm"
  )

# View the results
View(Measles_WW_wrong_boitekong)

#jozini
Measles_WW_wrong_jozini <- Measles_WW %>%
  filter(
    SITE_NAME == "Jozini Wastewater Treatment Plant" &
      tolower(trimws(DISTRICT_NAME)) != "umkhanyakude dm"
  )

# View the results
View(Measles_WW_wrong_jozini)

#musina
Measles_WW_wrong_musina <- Measles_WW %>%
  filter(
    SITE_NAME == "Musina WWTW (in town)" &
      tolower(trimws(DISTRICT_NAME)) != "vhembe dm"
  )

# View the results
View(Measles_WW_wrong_musina)

#nancefield
Measles_WW_wrong_nancefield <- Measles_WW %>%
  filter(
    SITE_NAME == "Nancefield" &
      tolower(trimws(DISTRICT_NAME)) != "vhembe dm"
  )

# View the results
View(Measles_WW_wrong_nancefield)

#airline 2
#nancefield
Measles_WW_wrong_airtline_two <- Measles_WW %>%
  filter(
    SITE_NAME == "Airline 2" &
      tolower(trimws(DISTRICT_NAME)) != "ekurhuleni mm"
  )

# View the results
View(Measles_WW_wrong_airtline_two)


# Merge all wrong district subsets into one table
#------------------------------
Measles_WW_wrong_all <- bind_rows(
  Measles_WW_wrong_tambo,
  Measles_WW_wrong_daspoort,
  Measles_WW_wrong_olifantsfontein,
  Measles_WW_wrong_atteridgeville,
  Measles_WW_wrong_midstream,
  Measles_WW_wrong_sasol,
  Measles_WW_wrong_tembisa_mall,
  Measles_WW_wrong_namane,
  Measles_WW_wrong_moloreng,
  Measles_WW_wrong_hartebeesfontein_waterworks,
  Measles_WW_wrong_bloemspruit_wastewater_treatment_works,
  Measles_WW_wrong_rooiwal_wastewater_treatment_works,
  Measles_WW_wrong_central,
  Measles_WW_wrong_goudkoppies,
  Measles_WW_wrong_tembisa_east_west,
  Measles_WW_wrong_bushkoppies_inlet,
  Measles_WW_wrong_northern_wastewater,
  Measles_WW_wrong_motsoaledi,
  Measles_WW_wrong_rustenburg,
  Measles_WW_wrong_boitekong,
  Measles_WW_wrong_jozini,
  Measles_WW_wrong_musina,
  Measles_WW_wrong_nancefield,
  Measles_WW_wrong_airtline_two
)

# View the merged table
View(Measles_WW_wrong_all)
Measles_new <- Measles_WW_wrong_all
View(Measles_new)

#number of wrong entries per site
table(Measles_new$SITE_NAME)

#by district
table(Measles_new$DISTRICT_NAME)

write.xlsx(Measles_new, "C:/Users/NtsakoM/Downloads/Measles_new.xlsx")


#---------------------------------------------------
#check of sample ids
subset(waste, sample_site_identification_number == "CST-WGS-25-1008")
view(waste)

sample_row <- subset(waste, sample_site_identification_number == "CST-WGS-25-1008")

# View the result
View(sample_row)




