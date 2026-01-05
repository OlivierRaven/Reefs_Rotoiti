# 1. Import_data
# Explanation of this script ------------------------------------------------

# 
# 
#
#


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/1. Reefs Rotoiti")

# Define the list of packages
packages <- c("tidyverse", "dplyr", "ggplot2","readxl", "writexl","readr")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------
Site_info <- read_excel("Data_raw/Data_Reefs_Rotoiti.xlsx")
Monitoring_data <- read_excel("Data_raw/Data_Reefs_Rotoiti.xlsx", sheet = "Monitoring_data")
Reef_data <- read_excel("Data_raw/Data_Reefs_Rotoiti.xlsx", sheet = "Reef_data")  %>% select(-starts_with("..."))
Weed_data <- read_excel("Data_raw/Data_Reefs_Rotoiti.xlsx", sheet = "Weed_data")  %>% select(-starts_with("..."))
Fish_data <- read_excel("Data_raw/Data_Reefs_Rotoiti.xlsx", sheet = "Fish_data") %>% select(-starts_with("..."))


# Combine monitoring, cpue, and natural habitat data ---------------------------
# Calculate CPUE and BCUE by Site and Species and net_Type
CPUE_BCUE <- Fish_data %>%
  filter(!is.na(Species)) %>%
  group_by(Monitoring_ID, Species,Net_type) %>%
  reframe(
    Total_Individuals = sum(Amount, na.rm = T),
    Total_Weight = sum(Weight_g, na.rm = TRUE),
    Total_Effort = first(Amount_nets),
    CPUE = Total_Individuals / Total_Effort,      
    BCUE = Total_Weight / Total_Effort,
    Mean_Length = mean(Length_mm, na.rm = TRUE),
    Min_Length = ifelse(all(is.na(Length_mm)), NA, min(Length_mm, na.rm = TRUE)),
    Max_Length = ifelse(all(is.na(Length_mm)), NA, max(Length_mm, na.rm = TRUE)),
    Mean_Weight = mean(Weight_g, na.rm = TRUE),
    Min_Weight = ifelse(all(is.na(Weight_g)), NA, min(Weight_g, na.rm = TRUE)),
    Max_Weight = ifelse(all(is.na(Weight_g)), NA, max(Weight_g, na.rm = TRUE)))

CPUE_BCUE_weighted <- CPUE_BCUE %>%
  group_by(Monitoring_ID, Species) %>%
  summarise(
    Total_Individuals = sum(Total_Individuals, na.rm = T),
    Total_Weight = sum(Total_Weight, na.rm = TRUE),
    Weighted_CPUE_numerator = sum(CPUE * Total_Effort, na.rm = TRUE),
    Weighted_BCUE_numerator = sum(BCUE * Total_Effort, na.rm = TRUE),
    Total_Effort_sum = sum(Total_Effort, na.rm = TRUE),
    
    Mean_Length = mean(Mean_Length, na.rm = TRUE),
    Min_Length = ifelse(all(is.na(Min_Length)), NA, min(Min_Length, na.rm = TRUE)),
    Max_Length = ifelse(all(is.na(Max_Length)), NA, max(Max_Length, na.rm = TRUE)),
    
    Mean_Weight = mean(Mean_Weight, na.rm = TRUE),
    Min_Weight = ifelse(all(is.na(Min_Weight)), NA, min(Min_Weight, na.rm = TRUE)),
    Max_Weight = ifelse(all(is.na(Max_Weight)), NA, max(Max_Weight, na.rm = TRUE)) ) %>%
  ungroup() %>%  
  mutate(
    Total_Effort_sum = ifelse(Monitoring_ID %in% c("96_0", "101_0", "117_1", "119_1"), 3, 4),  
    Weighted_CPUE = Weighted_CPUE_numerator / Total_Effort_sum,
    Weighted_BCUE = Weighted_BCUE_numerator / Total_Effort_sum )

# Create Presence/Absence Columns Based on Species Naming
species_presence_absence <- Fish_data %>%
  filter(!is.na(Species)) %>%
  distinct(Monitoring_ID, Species) %>% 
  mutate(Presence = 1) %>%       
  pivot_wider(
    names_from = Species, 
    values_from = Presence, 
    values_fill = list(Presence = 0), 
    names_prefix = "Presence_") %>%
  mutate(Predator_Fish_Presence = pmax(Presence_Trout, Presence_Eel, Presence_Catfish))


# Reshape CPUE_BCUE_weighted so each species becomes a column
CPUE_BCUE_weighted_summary <- CPUE_BCUE_weighted %>%
  pivot_wider(names_from = Species,
    values_from = c(Total_Individuals, Weighted_CPUE, Weighted_BCUE, Total_Weight, Mean_Length, Mean_Weight, Min_Length, Max_Length, Min_Weight, Max_Weight, Weighted_CPUE_numerator, Weighted_BCUE_numerator, Total_Effort_sum),
    names_sep = "_", values_fill = list(Total_Individuals = 0, Weighted_CPUE = 0, Weighted_BCUE = 0))

# Create a metadata table with Parameter-Unit mappings
Monitoring_data <- Monitoring_data %>%  select(-Site_ID)
unit_metadata <- Monitoring_data %>%
  select(Parameter, Unit) %>%
  distinct()

# Reshape Monitoring_data so each Monitoring becomes a column
Monitoring_summary <- Monitoring_data %>%
  select(-Group, -Notes, -Unit) %>%
  pivot_wider(names_from = c(Parameter),values_from = Value,values_fill = list(Value = NA))

# Reshape Reef_data so each Reef_Type becomes a column
Reef_summary <- Reef_data %>%
  select(-Unit, -Notes, -Site_ID)%>%
  pivot_wider(names_from = c(Parameter, Reef_ID), values_from = Value,names_sep = ".")

# Reshape Weed_data so each Weed_Type becomes a column
Weed_summary <- Weed_data %>%
  complete(Monitoring_ID, Weed_Type, Native_Status, fill = list(Percentage_Cover = 0)) %>%
  group_by(Monitoring_ID, Weed_Type, Native_Status) %>%
  summarise(Total_Cover = sum(Percentage_Cover, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = c(Weed_Type, Native_Status), values_from = Total_Cover, values_fill = 0)


# Pivot wider without modifying column names
Monitoring_Reef_data <- Site_info %>%
  left_join(Monitoring_summary, by = "Monitoring_ID") %>%
  left_join(Reef_summary, by = c("Monitoring_ID")) %>%
  left_join(Weed_summary %>% select(Monitoring_ID, Emergent_Native,Emergent_Non_Native,Submerged_Native, Submerged_Non_Native), by = c("Monitoring_ID")) %>%
  left_join(CPUE_BCUE_weighted_summary, by = "Monitoring_ID")%>%
  left_join(species_presence_absence, by = "Monitoring_ID")

# Extract additional date and time components
Monitoring_Reef_data <- Monitoring_Reef_data %>%
  mutate(Presence_rocks = if_else(Cobble > 1 | Boulders > 1, 1, 0),
         Slope_5m = 5/Distance_5m,
    Date = as.Date(Date_Time),                      
    Time = format(Date_Time, "%H:%M:%S"),
    Year = year(Date_Time),
    Month = month(Date_Time, label = TRUE),          
    Day = day(Date_Time),                             
    Season = case_when(                               
      Month %in% c("Dec", "Jan", "Feb") ~ "Summer",
      Month %in% c("Mar", "Apr", "May") ~ "Autumn",
      Month %in% c("Jun", "Jul", "Aug") ~ "Winter",
      Month %in% c("Sep", "Oct", "Nov") ~ "Spring",
      TRUE ~ NA_character_),
    Date_Time_Numeric = as.numeric(Date_Time))

str(Monitoring_Reef_data)

Monitoring_Reef_data[c(         "Bottom_visible","Water_clarity","Depth_10m","Slope","Riparian_vegetation","Overhanging_trees","Erosion","Sructure" ,"Bedrock", "Boulders", "Cobble", "Gravel", "Sand", "Mud", "Organic_matter","Rock_size", "Emergent_Native", "Emergent_Non_Native", "Submerged_Non_Native", "Wood_cover", "Temperature", "DO_mgl", "DO_percent", "Conductivity","Specific_conductivity","pH","Vegetation_nearby","Submerged_Native")] <-   #
  sapply(Monitoring_Reef_data[c("Bottom_visible","Water_clarity","Depth_10m","Slope","Riparian_vegetation","Overhanging_trees","Erosion","Sructure" ,"Bedrock", "Boulders", "Cobble", "Gravel", "Sand", "Mud", "Organic_matter","Rock_size", "Emergent_Native", "Emergent_Non_Native", "Submerged_Non_Native", "Wood_cover", "Temperature", "DO_mgl", "DO_percent", "Conductivity","Specific_conductivity","pH","Vegetation_nearby","Submerged_Native")], as.numeric)  

# Determine the Dominant Habitat Types of each site 
habitat_classification <- Monitoring_Reef_data %>%
  filter(Monitoring ==0 ) %>% # needs fixing as now the other monitoring's get NA
  select(Site_ID, DHT, Lake, 
         Bedrock, Boulders, Cobble, Gravel, Sand, Mud, Organic_matter, 
         Emergent_Native, Emergent_Non_Native, Submerged_Native, Submerged_Non_Native, Wood_cover) %>%
  pivot_longer(cols = c(Bedrock, Boulders, Cobble, Gravel, Sand, Mud, Organic_matter, Emergent_Native, Emergent_Non_Native, Submerged_Native, Submerged_Non_Native, Wood_cover), 
               names_to = "Type", values_to = "Percentage") %>%
  group_by(Site_ID) %>%
  summarise(
    Rocky_Percentage = sum(Percentage[Type %in% c("Bedrock", "Boulders", "Cobble")], na.rm = TRUE),
    Sand_Percentage = sum(Percentage[Type == "Sand"], na.rm = TRUE),
    Mud_Percentage  = sum(Percentage[Type %in% c("Mud", "Organic_matter")], na.rm = TRUE),
    Emergent_Percentage = sum(Percentage[Type %in% c("Emergent_Native")], na.rm = TRUE),
    # Substrate index calculation
    Substrate_index = sum(
      0.08 * Percentage[Type == "Bedrock"] +
        0.07 * Percentage[Type == "Boulders"] +
        0.06 * Percentage[Type == "Cobble"] +
        0.04 * Percentage[Type == "Gravel"] +
        0.03 * Percentage[Type == "Sand"] +
        0.02 * Percentage[Type == "Organic_matter"] +
        0.01 * Percentage[Type == "Mud"],
      na.rm = TRUE),.groups = "drop") %>%
  mutate(Habitat_Type = case_when(
    Rocky_Percentage > 25 ~ "Rocky",
    Emergent_Percentage > 25 ~ "Emergent Macrophyte",
    Sand_Percentage >= Mud_Percentage ~ "Sandy",
    TRUE ~ "Muddy")) %>%
  select(Site_ID, Habitat_Type, Substrate_index)

# Save as CSV
write.csv(habitat_classification, "Data_mod/habitat_classification.csv", row.names = FALSE)


#Merge Habitat_Type back into Monitoring_CPUE_data
Monitoring_Reef_data <- Monitoring_Reef_data %>%
  left_join(habitat_classification, by = c("Site_ID"))


# Save as CSV
write.csv(Monitoring_Reef_data, "Data_mod/Monitoring_Reef_data.csv", row.names = FALSE)

# Save as Excel file
write_xlsx(Monitoring_Reef_data, "Data_mod/Monitoring_Reef_data.xlsx")






