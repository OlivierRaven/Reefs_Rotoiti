# 2. Analise the data
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
packages <- c("sf", "mgcv","tidyverse", "dplyr", "ggplot2","readxl", "writexl","readr")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------
Monitoring_CPUE_data <- read_csv("Data_mod/Monitoring_Reef_data.csv")

Rotoiti <- st_read("Data_raw/Lake Rotoiti.gpkg")
Rotoiti_outline <- st_boundary(Rotoiti)
Rotoiti_outline <- st_transform(Rotoiti_outline, crs = 2193)

# Start analysis ---------------------------------------------------------------
# View the structure and summary of the data set
names(Monitoring_CPUE_data)
str(Monitoring_CPUE_data)
summary(Monitoring_CPUE_data)

# Remove rows where Date_Time is NA
M_C_data <- Monitoring_CPUE_data %>% filter(!is.na(Date_Time))

# Create a new column with only the date
M_C_data$Date <- as.Date(M_C_data$Date_Time)

# Convert Date_Time to numeric (seconds since 1970-01-01)
M_C_data$Date_Time_Numeric <- as.numeric(M_C_data$Date_Time)


# "Presence_Kōura","Total_Individuals_Kōura","Weighted_CPUE_Kōura", "Weighted_BCUE_Kōura"

M_C_data_sf <- M_C_data %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

ggplot() +
  geom_sf(data=Rotoiti_outline)+
  geom_sf(data=M_C_data_sf, aes(fill=Weighted_CPUE_Kōura), shape=21, size=4)+
  scale_fill_viridis_c()

ggplot() +
  geom_sf(data=Rotoiti_outline)+
  geom_sf(data=M_C_data_sf, aes(fill=DO_mgl), shape=21, size=4)




ggplot(M_C_data, aes(Monitoring, Weighted_CPUE_Kōura, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge")+
    geom_point(shape=21,col="black")
 
  


# Sediment analysis
# Define a custom color palette
sediment_colors <- c(
  "Bedrock" = "gray0",
  "Boulders" = "gray20",
  "Cobble" = "gray40",
  "Gravel" = "gray",
  "Sand" = "gold",
  "Mud" = "saddlebrown",
  "Organic_matter" = "darkgreen",
  "Turf" = "forestgreen")

# Select relevant columns and reshape the data
sediment_data <- M_C_data %>%
  select(Site_ID, DHT,Lake, Site, Bedrock, Boulders, Cobble, Gravel, Sand, Mud, Organic_matter, Turf) %>%
  pivot_longer(cols = c(Bedrock, Boulders, Cobble, Gravel, Sand, Mud, Organic_matter, Turf), names_to = "Sediment_Type", values_to = "Percentage") 

# Plot the data
ggplot(sediment_data, aes(x = factor( Site_ID  ), y = Percentage, fill = Sediment_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = sediment_colors) +
  labs(
    title = "Sediment Composition by Site",
    x = "Site ID",
    y = "Percentage (%)",
    fill = "Sediment Type") +
  facet_wrap(~DHT, scales = "free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0))



