# 0. Load required packages
library(sf)
library(ggplot2)
library(ggspatial)

# 1. Load ONLY the London borough shapefile
boroughs <- st_read("London_Borough/London_Borough_Excluding_MHW.shp")

# 2. Quick check of the data structure
print(boroughs)  # View metadata
head(boroughs)   # View first few rows
plot(st_geometry(boroughs)) # Basic plot

# 3. Better quality plot with ggplot
ggplot() +
  geom_sf(data = boroughs, 
          fill = "lightblue", 
          color = "navy", 
          size = 0.3) +
  labs(title = "London Borough Boundaries (Excluding MHW)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 4. Load the GeoJSON point data
points <- st_read("night_data_type.geojson")

# 5. CRS don't match, transform one to match the other
if(st_crs(points) != st_crs(boroughs)) {
  points <- st_transform(points, st_crs(boroughs))
}

# 6. Check coordinate reference systems (CRS) match
print(st_crs(points))
print(st_crs(boroughs))

# Find points that intersect with Newham
library(sf)
library(dplyr)

boroughs <- boroughs %>%
  mutate(NAME = as.character(NAME))

newham <- boroughs %>%
  filter(NAME == "Newham")  # Exact match

### Simple plot ###
plot(st_geometry(newham), 
     main = "Newham Borough Boundary",
     col = "lightblue",
     border = "darkblue",
     lwd = 2)

# Ensure consistent CRS (British National Grid for London)
points <- st_transform(points, 27700)
newham <- st_transform(newham, 27700)

# Clip points to Newham boundary
newham_points <- st_intersection(points, newham)

# Create the map
ggplot() +
  # Borough background
  geom_sf(data = newham, 
          fill = "#f0f9ff", 
          color = "#3a6ea5",
          lwd = 0.3) +
  
  # Points colored by type (assuming 'type' column exists)
  geom_sf(data = newham_points, 
          aes(color = type),  # Change color aesthetic as needed
          size = 2,
          alpha = 0.8) +
  
  # Map styling
  scale_color_viridis_d(option = "plasma") +  # Color scale for point types
  labs(title = "Points Within Newham Borough",
       subtitle = paste(nrow(newham_points), "total points"),
       color = "Point Type") +
  
  # Map extras
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "tr", 
                         style = north_arrow_minimal()) +
  
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right")