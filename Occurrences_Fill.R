# Load necessary libraries
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth) # For country boundaries

# Step 1: Load Earthquake Data
earthquake_df <- read_csv("earthquake_1995-2023.csv")

# Step 2: Aggregate Earthquake Occurrences by Country
earthquake_counts_by_country <- earthquake_df %>%
  group_by(country) %>%
  summarise(occurrences = n(), .groups = 'drop') %>%
  filter(!is.na(country)) # Exclude rows where country is NA

# Step 3: Obtain Geographic Boundary Data for Countries
world <- ne_countries(scale = "medium", returnclass = "sf")

# Step 4: Merge Aggregated Earthquake Data with Geographic Data
# Ensure the country names match between earthquake data and world dataset
# This might require adjusting the names in one of the datasets for a successful merge
world_with_eq_counts <- merge(world, earthquake_counts_by_country, by.x = "admin", by.y = 'country')

# Step 5: Visualize Earthquake Occurrences by Country on a Map
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = world_with_eq_counts, aes(fill = occurrences), color = "white") +  # Fill countries based on earthquake occurrences
  scale_fill_gradient(low = "blue", high = "red", name = "Earthquake\nOccurrences") +  # Use a color scale
  labs(title = "Earthquake Occurrences by Country (1995-2023)",
       fill = "Occurrences") +
  theme_minimal() +
  theme(legend.position = "bottom")

