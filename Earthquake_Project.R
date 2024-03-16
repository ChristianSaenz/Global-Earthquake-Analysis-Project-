library(readr)
library(ggplot2)

library(rnaturalearth)  # Gives access to the natural earth map
library(dplyr)
library(sf)

# Load earthquake data
df <- read_csv("earthquake_1995-2023.csv")

# loading in world from naturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

# Convert earthquake data to an sf object and create a new row called geometry containing the longitude and latitude so we can use it map to countries 
# "crs = 4326, agr = "constant"" is used to identify the coordinate system which 4326 is a normalized coordinate system and we want the points of each coordinate to be constant 
earthquake_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Perform a spatial join to keep only earthquakes on land
earthquake_on_land <- earthquake_sf %>%
  st_join(world, join = st_within) %>%
  filter(!is.na(country))

# Plotting
ggplot() +
  geom_sf(data = world) +  # Plots the countries
  geom_sf(data = earthquake_on_land, aes(color = magnitude), size = 1, alpha = 0.6) +  # Plots the earthquakes
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Global Earthquake Distribution", x = " ", y = " ", color = "Magnitude")



