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
packages <- c("patchwork", "sf", "mgcv","tidyverse", "dplyr", "ggplot2","readxl", "writexl","readr")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------
Monitoring_Reef_data <- read_csv("Data_mod/Monitoring_Reef_data.csv")

Rotoiti <- st_read("Data_raw/Lake Rotoiti.gpkg")
Rotoiti_outline <- st_boundary(Rotoiti)
Rotoiti_outline <- st_transform(Rotoiti_outline, crs = 2193)

# Start analysis ---------------------------------------------------------------
# View the structure and summary of the data set
names(Monitoring_Reef_data)
str(Monitoring_Reef_data)
summary(Monitoring_Reef_data)

MR_data <- Monitoring_Reef_data 

# Define the season order
MR_data$Season <- factor(MR_data$Season, levels = c("Summer", "Autumn", "Winter", "Spring"))
MR_data$Site_ID <- as.factor(MR_data$Site_ID)
MR_data$ID <- as.factor(MR_data$ID)
MR_data$Monitoring_ID <- as.factor(MR_data$Monitoring_ID)
#MR_data$Monitoring<- as.factor(MR_data$Monitoring)

write.csv(MR_data, "Data_mod/MR_data.csv")



# Make spatial data
MR_data_sf <- MR_data %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326)

ggplot() +
  geom_sf(data = Rotoiti_outline, color = "black") +
  geom_sf(data = MR_data_sf, aes(col = Site, shape = Site), size=5)
  #geom_label(data = MR_data_sf$ID)

# Physical parameters ----------------------------------------------------------
# Define a custom color palette
sediment_colors <- c("Bedrock" = "black","Boulders" = "gray25","Cobble" = "gray55","Gravel" = "gray80","Sand" = "gold","Mud" = "saddlebrown","Organic_matter" = "darkgreen", "Turf" = "lightgreen")
weed_colors <- c("Emergent_Native" = "darkgreen","Emergent_Non_Native" = "limegreen","Submerged_Native" = "skyblue","Submerged_Non_Native" = "royalblue4","Wood_cover" = "saddlebrown")

#Create the 'presence_rocks' variable
MR_data <- MR_data %>%
  mutate(Presence_rocks = if_else(Cobble > 1 | Boulders > 1 | Bedrock > 1, 1, 0))

# Remake Physical parameters
sediment_data <- MR_data %>%
  select(ID, Monitoring_ID, Site,DHT,Monitoring,Habitat_Type , Lake, Bedrock, Boulders, Cobble, Gravel, Sand, Mud, Organic_matter) %>%
  pivot_longer(cols = c(Bedrock, Boulders, Cobble, Gravel, Sand, Mud, Organic_matter), 
               names_to = "Sediment_Type", values_to = "Percentage") 

weed_data <- MR_data %>%
  select(ID,Monitoring_ID, Site,DHT,Monitoring,Habitat_Type , Lake, Emergent_Native,Emergent_Non_Native, Submerged_Native, Submerged_Non_Native,Wood_cover) %>%
  pivot_longer(cols = c(Emergent_Native,Emergent_Non_Native, Submerged_Native, Submerged_Non_Native,Wood_cover), 
               names_to = "Weeds", values_to = "Percentage") 

# Plot Physical parameters
Sediment_plot <- ggplot(sediment_data, aes(ID, Percentage, fill = Sediment_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = sediment_colors) +
  facet_grid(Habitat_Type ~ Site, scales = "free_x") 

Weed_plot <- ggplot(weed_data, aes(ID,Percentage, fill = Weeds)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = weed_colors) +
  facet_grid(Habitat_Type ~Site , scales = "free_x") 

#Slope_plot <- 
  ggplot(MR_data, aes(ID, Distance_deep_20m, col=Site))+
  geom_point()+
  facet_grid(Habitat_Type ~ Site, scales = "free_x")

Physical_plot <- Sediment_plot/Weed_plot
Physical_plot

ggsave("Figures/Physical_plot.png", Physical_plot, width = 12, height = 6, dpi = 300)


# Chemical parameters ----------------------------------------------------------
# Remake Chemical parameters
Chemical_data <- MR_data %>%
  select(Monitoring_ID,Season, Site, ID, Monitoring,DHT,Habitat_Type,Lake,DO_mgl,DO_percent,Conductivity,Specific_conductivity,pH, Temperature) %>%
  pivot_longer(cols = c(Temperature,DO_mgl,DO_percent,Conductivity,Specific_conductivity,pH), 
               names_to = "Variable", values_to = "Values") 

# Plot Chemical parameters
Chemical_plot <- ggplot(Chemical_data, aes(as.factor(Monitoring), Values, fill = Site)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free", nrow = 1) 

Chemical_plot

ggsave("Figures/Chemical_plot.png", Chemical_plot, width = 12, height = 5, dpi = 300)

# Reef parameters ----------------------------------------------------------
# Remake Reef parameters
Reefs_data <- MR_data %>%
  select(Monitoring_ID,ID, Site, Monitoring, DHT, Habitat_Type, Lake,
         Distance_to_shore.1, Length.1, Width.1, Area.1, Weight.1, Depth_on_R.1, Depth_on_L.1, Hight_above.1, Depth_after.1, Volume.1, Sedimentation.1, Accumulation_of_weeds.1,
         Distance_to_shore.2, Length.2, Width.2, Area.2, Weight.2, Depth_on_R.2, Depth_on_L.2, Hight_above.2, Depth_after.2, Volume.2, Sedimentation.2, Accumulation_of_weeds.2,
         Distance_to_shore.3, Length.3, Width.3, Area.3, Weight.3, Depth_on_R.3, Depth_on_L.3, Hight_above.3, Depth_after.3, Volume.3, Sedimentation.3, Accumulation_of_weeds.3) %>%
  pivot_longer(cols = matches("Distance_to_shore|Length|Width|Area|Weight|Depth_on_R|Depth_on_L|Hight_above|Depth_after|Volume|Sedimentation|Accumulation_of_weeds"),
    names_to = c("Variable", "Reef_Number"),names_sep = "\\.",values_to = "Values")%>%
  filter(Site == "Reef") 

Reefs_summary <- Reefs_data %>%
  group_by(Variable) %>%
  summarize(Mean = mean(Values, na.rm = TRUE),SEM = sd(Values, na.rm = TRUE) / sqrt(n()))

Reefs_plot <- ggplot(Reefs_data, aes(ID , Values, col = Reef_Number)) +
  geom_point() +
  facet_wrap(~Variable , scales = "free") 

Reefs_plot

ggsave("Figures/Reefs_plot.png", Reefs_plot, width = 12, height = 5, dpi = 300)



# Biological parameters --------------------------------------------------------
# Remake Biological parameters
fish_order <- c("Presence_Kōura","Presence_Eel","Presence_Kōaro","Presence_Common_smelt","Presence_Bullies","Presence_Catfish","Presence_Morihana","Presence_Mosquitofish","Presence_Trout")
fish_order_CPUE <- c("Weighted_CPUE_Kōura", "Weighted_CPUE_Eel", "Weighted_CPUE_Kōaro", "Weighted_CPUE_Common_smelt", "Weighted_CPUE_Bullies", "Weighted_CPUE_Catfish", "Weighted_CPUE_Morihana", "Weighted_CPUE_Mosquitofish", "Weighted_CPUE_Trout")
fish_order_BCUE <- c("Weighted_BCUE_Kōura", "Weighted_BCUE_Eel", "Weighted_BCUE_Kōaro", "Weighted_BCUE_Common_smelt", "Weighted_BCUE_Bullies", "Weighted_BCUE_Catfish", "Weighted_BCUE_Morihana", "Weighted_BCUE_Mosquitofish", "Weighted_BCUE_Trout")
fish_orders_all <- c(fish_order, fish_order_CPUE, fish_order_BCUE)
species_names_all <- sub("^(Presence_|Weighted_CPUE_|Weighted_BCUE_)", "", fish_orders_all)
species_colors <- c("Bullies"="#1f78b4","Eel"="#654321","Morihana"="#33a02c","Common_smelt"="#a6cee3","Kōaro"="#ff7f00","Kōura"="#b15928","Mosquitofish"="#e31a1c","Catfish"="#6a3d9a","Trout"="#fdbf6f")
fish_colors <- setNames(species_colors[species_names_all], fish_orders_all)


#fish_colors <- c("Presence_Kōura"=species_colors["Kōura"],"Presence_Eel"=species_colors["Eel"],"Presence_Kōaro"=species_colors["Kōaro"],"Presence_Common_smelt"=species_colors["Common_smelt"],"Presence_Bullies"=species_colors["Bullies"],"Presence_Catfish"=species_colors["Catfish"],"Presence_Morihana"=species_colors["Morihana"],"Presence_Mosquitofish"=species_colors["Mosquitofish"],"Presence_Trout"=species_colors["Trout"],"Weighted_CPUE_Kōura"=species_colors["Kōura"],"Weighted_CPUE_Eel"=species_colors["Eel"],"Weighted_CPUE_Kōaro"=species_colors["Kōaro"],"Weighted_CPUE_Common_smelt"=species_colors["Common_smelt"],"Weighted_CPUE_Bullies"=species_colors["Bullies"],"Weighted_CPUE_Catfish"=species_colors["Catfish"],"Weighted_CPUE_Morihana"=species_colors["Morihana"],"Weighted_CPUE_Mosquitofish"=species_colors["Mosquitofish"],"Weighted_CPUE_Trout"=species_colors["Trout"],"Weighted_BCUE_Kōura"=species_colors["Kōura"],"Weighted_BCUE_Eel"=species_colors["Eel"],"Weighted_BCUE_Kōaro"=species_colors["Kōaro"],"Weighted_BCUE_Common_smelt"=species_colors["Common_smelt"],"Weighted_BCUE_Bullies"=species_colors["Bullies"],"Weighted_BCUE_Catfish"=species_colors["Catfish"],"Weighted_BCUE_Morihana"=species_colors["Morihana"],"Weighted_BCUE_Mosquitofish"=species_colors["Mosquitofish"],"Weighted_BCUE_Trout"=species_colors["Trout"])


presence_data <- MR_data %>%
  select(Monitoring_ID,ID,Monitoring_ID., Season, Site,Monitoring, DHT,Habitat_Type, Lake, Presence_Kōura, Presence_Eel, Presence_Kōaro, Presence_Common_smelt, Presence_Bullies, Presence_Catfish, Presence_Morihana, Presence_Mosquitofish, Presence_Trout) %>%
  pivot_longer(cols = c(Presence_Kōura, Presence_Eel, Presence_Kōaro, Presence_Common_smelt, Presence_Bullies, Presence_Catfish, Presence_Morihana, Presence_Mosquitofish, Presence_Trout),names_to = "Fish_Type", values_to = "Presence") %>%
  mutate(Fish_Type = factor(Fish_Type, levels = fish_order))

CPUE_data <- MR_data %>%
  select(Monitoring_ID,ID,Monitoring_ID., Season, Site,Monitoring, DHT,Habitat_Type, Lake, Weighted_CPUE_Kōura, Weighted_CPUE_Eel, Weighted_CPUE_Kōaro, Weighted_CPUE_Common_smelt, Weighted_CPUE_Bullies, Weighted_CPUE_Catfish, Weighted_CPUE_Morihana, Weighted_CPUE_Mosquitofish, Weighted_CPUE_Trout) %>%
  pivot_longer(cols = c(Weighted_CPUE_Kōura, Weighted_CPUE_Eel, Weighted_CPUE_Kōaro, Weighted_CPUE_Common_smelt, Weighted_CPUE_Bullies, Weighted_CPUE_Catfish, Weighted_CPUE_Morihana, Weighted_CPUE_Mosquitofish, Weighted_CPUE_Trout),names_to = "Fish_Type", values_to = "CPUE") %>%
  mutate(Fish_Type = factor(Fish_Type, levels = fish_order_CPUE))

BCUE_data <- MR_data %>%
  select(Monitoring_ID,ID,Monitoring_ID.,Season,  Site,Monitoring, DHT,Habitat_Type, Lake, Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout) %>%
  pivot_longer(cols = c(Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout),names_to = "Fish_Type", values_to = "BCUE") %>%
  mutate(Fish_Type = factor(Fish_Type, levels = fish_order_BCUE))

BCUE_summary <- MR_data %>%
  select(Monitoring_ID,ID,Monitoring_ID.,Season,  Site,Monitoring, DHT,Habitat_Type, Lake, Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout) %>%
  pivot_longer(cols = c(Weighted_BCUE_Kōura, Weighted_BCUE_Eel, Weighted_BCUE_Kōaro, Weighted_BCUE_Common_smelt, Weighted_BCUE_Bullies, Weighted_BCUE_Catfish, Weighted_BCUE_Morihana, Weighted_BCUE_Mosquitofish, Weighted_BCUE_Trout),names_to = "Fish_Type", values_to = "BCUE") %>%
  mutate(Fish_Type = factor(Fish_Type, levels = fish_order_BCUE)) %>%
  group_by(Site,Habitat_Type, Fish_Type) %>%
  summarise(mean_BCUE = mean(BCUE, na.rm = TRUE),se_BCUE = sd(BCUE, na.rm = TRUE) / sqrt(n()),.groups = "drop")

Total_Weight_summary <- MR_data %>%
  select(Monitoring_ID,ID,Monitoring_ID.,Season,  Site,Monitoring, DHT, Habitat_Type, Lake,Total_Weight_Bullies, Total_Weight_Morihana, Total_Weight_Kōura,Total_Weight_Common_smelt, Total_Weight_Eel, Total_Weight_Kōaro,Total_Weight_Trout, Total_Weight_Mosquitofish, Total_Weight_Catfish) %>%
  pivot_longer(cols = c(Total_Weight_Bullies, Total_Weight_Morihana, Total_Weight_Kōura,Total_Weight_Common_smelt, Total_Weight_Eel, Total_Weight_Kōaro,Total_Weight_Trout, Total_Weight_Mosquitofish, Total_Weight_Catfish),
               names_to = "Fish_Type", values_to = "Total_Weight") %>%
  group_by(Fish_Type) %>%
  summarise(Total_Weight = sum(Total_Weight, na.rm = TRUE),.groups = "drop")


# Plot Biological parameters
ggplot(presence_data, aes(Site, Presence, fill = Fish_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = fish_colors) +
  facet_wrap( ~ Site, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(CPUE_data, aes(Site, CPUE, fill = Fish_Type)) +
  geom_boxplot()+
  scale_fill_manual(values = fish_colors) 
#facet_grid( ~ DHT, scales = "free_x") +
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(BCUE_data, aes(Season, BCUE, fill = Fish_Type)) +
  geom_boxplot()+
  scale_fill_manual(values = fish_colors)+
  facet_grid(Fish_Type~ Site, scales = "free") 
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(BCUE_summary, aes(Site, mean_BCUE, fill = Fish_Type)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_BCUE - se_BCUE, ymax = mean_BCUE + se_BCUE),position = position_dodge(width = 0.9), width = 0.2) +
  scale_fill_manual(values = fish_colors) +
 # facet_grid( ~ Season, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# Kōura plots
plot1 <- MR_data %>%
  ggplot(aes(Season, Total_Individuals_Kōura, fill = Monitoring)) +
  geom_boxplot() +
  facet_grid(~ Site, scales = "free") +
  labs(y= "Total Kōura") +
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(),legend.position = "none")

plot2 <- MR_data %>%
  ggplot(aes(Season, Weighted_CPUE_Kōura, fill = Monitoring)) +
  geom_boxplot() +
  facet_grid(~ Site, scales = "free") +
  labs(y= "CPUE Kōura") +
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())

plot3 <- MR_data %>%
  ggplot(aes(Season, Weighted_BCUE_Kōura, fill = Monitoring)) +
  geom_boxplot() +
  facet_grid(~ Site, scales = "free") +
  labs(y= "BCUE Kōura") +
  theme(legend.position = "none")#, axis.text.x = element_text(angle = 90, vjust = .3))

Koura_plot <- plot1 / plot2 / plot3
Koura_plot

ggsave("figures/Koura_plot.png", Koura_plot, width = 12, height = 5, dpi = 300)


plot1 <- MR_data %>%
  ggplot(aes(as.numeric(Monitoring), Total_Individuals_Kōura, col=Site)) +
  geom_point() +
  geom_smooth(se = F)+
  #facet_grid(~ Site, scales = "free") +
  labs(y= "Total Kōura", x="Monitoring") +
  theme_bw()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank(),legend.position = "none")

plot2 <- MR_data %>%
  ggplot(aes(as.numeric(Monitoring), Weighted_CPUE_Kōura, col=Site)) +
  geom_point() +
  geom_smooth(se = F)+
  #facet_grid(~ Site, scales = "free") +
  labs(y= "CPUE Kōura", x="Monitoring") +
  theme_bw()+
theme(axis.title.x = element_blank(),axis.text.x = element_blank())

plot3 <- MR_data %>%
  ggplot(aes(as.numeric(Monitoring), Weighted_BCUE_Kōura, col=Site)) +
  geom_point() +
  geom_smooth(se = F)+
  #facet_grid(~ Site, scales = "free") +
  labs(y= "BCUE Kōura", x="Monitoring") +
  theme_bw()+
  theme(legend.position = "none")#, axis.text.x = element_text(angle = 90, vjust = .3))

Koura_plot <- plot1 / plot2 / plot3
Koura_plot


PLOT <- MR_data %>%
  ggplot(aes(as.factor(Monitoring),Weighted_CPUE_Kōura, fill = Site)) +
  geom_boxplot(alpha = 0.6,outlier.shape = NA,color = "black",position = position_dodge(width = 0.6)) +
  geom_point(aes(color = Site),position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6), size = 2,shape = 21,stroke = 0.2, col="black") +
  geom_smooth(aes(group = Site, color = Site),method = "loess", se = FALSE) +
  #facet_grid(~ Site, scales = "free") +
  labs(y= "CPUE Kōura", x="Monitoring") +
  theme_bw()
PLOT


# Spatial plots ----------------------------------------------------------------
ggplot(MR_data_sf) +
  geom_sf(data = Rotoiti_outline, inherit.aes = FALSE) +
  geom_sf(aes(fill = Weighted_CPUE_Kōura), shape = 21, size = 4) +
  facet_grid(Season ~ ., labeller = label_both) 

ggplot() +
  geom_sf(data=Rotoiti_outline)+
  geom_sf(data=MR_data_sf, aes(fill=DO_mgl), shape=21, size=4)




ggplot(MR_data, aes(Monitoring, Weighted_CPUE_Kōura, fill = Site)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),size = 2) 

 
ggplot(MR_data, aes(Monitoring, Weighted_CPUE_Kōura, fill= Site))+
    geom_boxplot(outlier.shape = NA)+
    geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),size = 2) 

  
  
cpue<-ggplot(MR_data, aes( Monitoring, Weighted_CPUE_Kōura, fill = Site)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),size = 2) 

bcue<-ggplot(MR_data, aes( Monitoring, Weighted_BCUE_Kōura, fill = Site)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),size = 2) 

tik<-ggplot(MR_data, aes(Monitoring, Total_Individuals_Kōura, fill = Site)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),size = 2) 



cpue/bcue/tik


# Kōura data explained ---------------------------------------------------------

ggplot(MR_data,aes(Temperature,Weighted_CPUE_Kōura, col = as.factor(Monitoring))) +
  geom_point()






