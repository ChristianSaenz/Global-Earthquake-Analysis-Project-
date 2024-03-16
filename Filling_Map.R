library(readr)
library(ggplot2)
library(rnaturalearth)  # Gives access to the natural earth map
library(dplyr)
library(sf)

# Load earthquake data
df <- read_csv("earthquake_1995-2023.csv")

# Ensure 'world' is loaded before you use it for spatial joins
world <- ne_countries(scale = "medium", returnclass = "sf")

# Convert earthquake data to an sf object
earthquake_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Perform a spatial join to keep only earthquakes on land
earthquake_on_land <- earthquake_sf %>%
  st_join(world, join = st_within) %>%
  filter(!is.na(country))

# Recalculate affected countries based on the spatially filtered data
affected_countries <- df %>%
  group_by(country) %>%
  summarise(n = n(), avg_magnitude = mean(magnitude, na.rm = TRUE)) %>%
  filter(n >= 1 )

#joining earthquake data and its geometry points to its country name in the world set
# It's specifically doing this by joining affected_countries with world 

world_with_eq_data <- world %>%
  left_join(affected_countries, by = c("name" = "country"))
# Filter 'world' to include only affected countries
affected_world <- world[world$name %in% affected_countries$country, ]


# Plot the entire world map with all country boundaries
ggplot(data = world) +
  geom_sf() +  # This plots the boundaries of all countries in the world dataset
  geom_sf(data = world_with_eq_data, aes(fill = avg_magnitude), color = "white", size = 0.2) + 
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey") +  # Use grey color for countries without data
  theme_minimal() +
  labs(title = "Average Earthquake Magnitude by Country (1995-2023)",
       fill = "Avg Magnitude")

