# 1.5 Fish_data
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
#Monitoring_data <- read_excel("Data_raw/Data_Reefs_Rotoiti.xlsx", sheet = "Monitoring_data")
#Reef_data <- read_excel("Data_raw/Data_Reefs_Rotoiti.xlsx", sheet = "Reef_data")  %>% select(-starts_with("..."))
#Weed_data <- read_excel("Data_raw/Data_Reefs_Rotoiti.xlsx", sheet = "Weed_data")  %>% select(-starts_with("..."))
Fish_data <- read_excel("Data_raw/Data_Reefs_Rotoiti.xlsx", sheet = "Fish_data") %>% select(-starts_with("..."))


# Save raw fish data combined with Sites
Fish_data_2 <- Fish_data %>% 
  select(-Site_ID) %>%
  left_join(Site_info, by = "Monitoring_ID") %>% 
  filter(!is.na(Monitoring_ID))

# Create a season column from Date_Time_out
Fish_data_2 <- Fish_data_2 %>%
  mutate(Season = case_when(
    month(Date_Time_out) %in% c(12, 1, 2) ~ "Summer",
    month(Date_Time_out) %in% c(3, 4, 5) ~ "Autumn",
    month(Date_Time_out) %in% c(6, 7, 8) ~ "Winter",
    month(Date_Time_out) %in% c(9, 10, 11) ~ "Spring"),
    Season = factor(Season, levels = c("Summer", "Autumn", "Winter", "Spring")))


write.csv(Fish_data_2, "Data_mod/Fish_Reef_data.csv", row.names = FALSE)


names(Fish_data_2)

# make summary of the Fish data
Fish_data_2_summary <- Fish_data_2 %>%
  filter(!is.na(Monitoring), !is.na(Site)) %>%                  
  distinct(Monitoring, Monitoring_ID, Net_type, .keep_all = TRUE) %>% 
  group_by(Site, Monitoring) %>%                                
  summarise(
    Sites_Monitored = n_distinct(Monitoring_ID),         
    Total_Nets = sum(Amount_nets, na.rm = TRUE),  
    .groups = "drop")



# Explore all Fish data-------------------------------------------------------------
####### Explore Fish_data & Calculate CPUE
names(Fish_data_2)

species_summary <- Fish_data %>%
  filter(!is.na(Species)) %>%  # Remove rows where Species is NA
  group_by(Species) %>%
  summarize(
    Total_Records = n(),  
    Sites_Present = n_distinct(Site_ID),
    Avg_Length = mean(Length_mm, na.rm = TRUE),  
    Avg_Weight = mean(Weight_g, na.rm = TRUE),   
    Total_Amount = sum(Amount, na.rm = TRUE),    
    Total_Weight = sum(Weight_g, na.rm = TRUE),  
    Sites_Present = n_distinct(Site_ID),
    .groups = 'drop')
species_summary

ggplot(Fish_data %>% 
         filter(!Species %in% c("Bullies", "Common_smelt", "Trout", "Eel")) %>% 
         filter(!is.na(Species)) %>% 
         filter(!is.na(Length_mm) & !is.na(Weight_g)),
       aes(Length_mm, Weight_g, col = Sex)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Species, scales = "free")

ggplot(Fish_data %>% 
         filter(!Species %in% c("Bullies", "Common_smelt", "Trout", "Eel")) %>% 
         filter(!is.na(Species)) %>% 
         filter(!is.na(Length_mm)),
       aes(x = Length_mm, fill = Sex)) +
  geom_histogram(binwidth = 10, position = "dodge") +
  facet_wrap(~Species, scales = "free") +
  labs(title = "Histogram of Fish Length by Species and Sex") 


#Summarize total animals per site and season
site_season_data <- Fish_data_2 %>%
  group_by(Site, Monitoring, Season, Species) %>%
  summarise(Total_Animals = sum(Amount, na.rm = TRUE), .groups = "drop")

# Choose 2 sites to plot
sites_to_plot <- c("9", "10") # replace with actual site names

filtered_data <- site_season_data %>%
  #filter(ID %in% sites_to_plot)%>% 
  filter(!Species %in% c("Bullies", "Common_smelt"), !is.na(Species))

# Create a palette for species (adjusts to number of unique species)
species_colors <- c("Bullies"="#1f78b4","Eel"="#654321","Morihana"="#33a02c","Common_smelt"="#a6cee3","Kōaro"="#ff7f00","Kōura"="#b15928","Mosquitofish"="#e31a1c","Catfish"="#6a3d9a","Trout"="#fdbf6f")

# Plot
ggplot(filtered_data, aes(Season, Total_Animals, fill = Species)) +
  geom_bar(stat = "identity", position = "stack", col="black") +
  facet_wrap(~Site) +
  scale_fill_manual(values = species_colors) 
  labs(title = paste("Species Caught at sites: 9 & 10"),x = "Season", y = "Number of animals",fill = "Species") 



# Koura ------------------------------------------------------------------------
# Calculate the total count
Koura_data <- Fish_data_2 %>%
  filter(Species == "Kōura")

# Calculate the koura numbers in different ways
total_count <- Koura_data %>%
  nrow()

total_count_by_Site <- Koura_data %>%
  group_by(Site, Monitoring) %>%
  summarise(total_Kōura = n())

Fish_data_2_summary <- Fish_data_2_summary %>%
  left_join(total_count_by_Site, by = c("Site", "Monitoring"))

total_count_by_site_ID <- Koura_data %>%
  group_by(Site_ID, Monitoring) %>%
  summarise(total_Kōura = n())

CPUE_by_Site <- Fish_data_2_summary %>%
  group_by(Site, Monitoring) %>% 
  summarise(
    total_Kōura = sum(total_Kōura, na.rm = TRUE),
    total_sites = sum(Sites_Monitored, na.rm = TRUE),
    CPUE = total_Kōura / total_sites)

# Calculate mean length and weight for each habitat type and lake
mean_koura_stats <- Koura_data %>%
  group_by(Site, Monitoring) %>%
  summarise(mean_length_mm = mean(Length_mm, na.rm = TRUE),sd_length_mm = sd(Length_mm, na.rm = TRUE),n_length = sum(!is.na(Length_mm)),sem_length_mm = sd_length_mm / sqrt(n_length),
            mean_weight_g = mean(Weight_g, na.rm = TRUE),sd_weight_g = sd(Weight_g, na.rm = TRUE),n_weight = sum(!is.na(Weight_g)),sem_weight_g = sd_weight_g / sqrt(n_weight),
            .groups = "drop")

# Calculate size class counts
size_class_counts <- Koura_data %>%
  #filter(Species == "Kōura", !is.na(Length_mm)) %>%
  mutate(Size_Class = case_when(
    Length_mm < 23 ~ "Small",
    Length_mm >= 23 & Length_mm < 30 ~ "Medium",
    Length_mm >= 30 ~ "Large")) %>%
  group_by(Size_Class) %>%
  summarise(Count = n())

# Extract counts for each size class
small_count <- size_class_counts %>% filter(Size_Class == "Small") %>% pull(Count)
medium_count <- size_class_counts %>% filter(Size_Class == "Medium") %>% pull(Count)
large_count <- size_class_counts %>% filter(Size_Class == "Large") %>% pull(Count)



# Create the histogram with the total count in the title
ggplot(Koura_data, aes(Length_mm, Weight_g, col=Sex))+
  geom_point()+
  geom_smooth()

ggplot(Koura_data %>%
         filter(!is.na(Length_mm)),
       aes(Length_mm, fill=Sex)) +
  #geom_density()+
  geom_histogram(binwidth = 1, color = "black",position = "dodge", na.rm = TRUE) +
  labs(x = "Length (mm)", 
       y = "Count", 
       title = paste("Histogram of Kōura Lengths (Total Count:", total_count,")"))

ggplot(Koura_data %>%
         filter(!is.na(Length_mm), !is.na(Sex)),
       aes(x = Length_mm, fill = Sex)) +
  geom_histogram(binwidth = 3, color = "black", position = "dodge", na.rm = TRUE) +
  labs(x = "OCL Length (mm)", 
       y = "Count", 
       fill = "Gender", 
       title = paste("Histogram of Kōura Lengths by Gender (Total Count:",total_count,")")) +
  facet_wrap(~Net_type ) + # Create facets for Gender
  theme(legend.position = "top")




# Create the plot
ggplot(Fish_data_2 %>% 
         filter(Species == "Kōura") %>% 
         filter(!is.na(Length_mm), !is.na(Sex)),
       aes(x = Length_mm, fill = Sex)) +
  geom_histogram(binwidth = 3, color = "black", position = "dodge", na.rm = TRUE) +
  labs(x = "OCL Length (mm)", 
       y = "Count", 
       fill = "Gender", 
       title = "Histogram of Kōura Lengths (Total Count:",total_count,")") +
  facet_grid(Site ~ Monitoring) + # Ensure each Lake has its own row/column
  geom_text(data = Fish_data_2_summary, 
            aes(x = Inf, y = Inf, 
                label = paste("Sites:", Sites_Monitored, 
                              "\nTotal Kōura:", total_Kōura)),
            inherit.aes = FALSE,
            hjust = 1.1, vjust = 1.1, size = 3) +  # Position and size adjustments
  theme(legend.position = "top")


ggplot()



# other species


ggplot(Fish_data_2 %>% 
         filter(Species == "Catfish") %>% 
         filter(!is.na(Length_mm)),
       aes(x = Length_mm)) +
  geom_histogram(binwidth = 3, color = "black", position = "dodge", na.rm = TRUE) +
  facet_grid(Site ~ Lake) 

