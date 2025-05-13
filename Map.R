# Load libraries
library(leaflet)
library(sf)

# Assuming your data is already loaded:
# newham: Newham polygon (sf object)
# newham_pub: Public locations or features (sf object)
# map_geojson: GeoJSON data with pins (sf object or GeoJSON object)

# Convert newham and newham_pub to EPSG:4326 (if needed)
newham <- st_transform(newham, crs = 4326)
newham_pub <- st_transform(newham_pub, crs = 4326)

# Ensure map_geojson is in GeoJSON format (if it's an sf object)
map_geojson <- st_as_sf(map_geojson)  # If it's not in sf format yet

# Create a leaflet map with dark monochrome tiles and larger neon blue filled points
leaflet() %>%
  # Add a dark, monochrome map tile set (CartoDB Dark Matter)
  addProviderTiles("CartoDB.DarkMatter") %>%
  
  # Add Newham polygon with no fill and dashed white outline
  addPolygons(data = newham, 
              color = "white",        # White outline color
              weight = 2,             # Border thickness
              opacity = 1,            # Full opacity of the border
              fillColor = NA,         # No fill
              fillOpacity = 0,        # No fill opacity
              dashArray = "5,5") %>%  # Dashed border pattern (5px dashed, 5px gap)
  
  # Add larger neon blue filled pins for the public locations (increased size)
  addCircleMarkers(data = newham_pub, 
                   color = "neonblue",   # Outline color (you can use actual hex code for neon blue)
                   fillColor = "#00FFFF", # Neon blue fill color
                   fillOpacity = 1,      # Full opacity
                   radius = 10,          # Increased radius (larger pins)
                   weight = 2,           # Border thickness
                   opacity = 0.8) %>%    # Opacity of the circle's outline
  
  # Add a title (with HTML styling)
  addControl("<div style='font-size: 20px; font-weight: bold; color: white;'>Night Runners Check Points (March 2025)</div>", 
             position = "topright") %>%
  
  # Add a legend indicating "Open Check-points" with green spots
  addLegend(position = "bottomright", 
            colors = c("#00FFFF"), # Green for open check-points, neon blue for regular check-points
            labels = c("Open Check-points"), 
            title = "Check-point Status", 
            opacity = 1)

    
#####

# Check and reproject CRS if necessary
newham <- st_transform(newham, crs = 4326)  # Ensure 'newham' shapefile is in EPSG:4326
map_geojson <- st_transform(map_geojson, crs = 4326)  # Ensure 'map_geojson' is in EPSG:4326

# Extract coordinates from the map_geojson pins
crime_coords <- st_coordinates(map_geojson)

# Assuming `newham_pub` contains the coordinates of additional points to add as pins
newham_pub_coords <- st_coordinates(newham_pub)  # Extract coordinates for newham_pub pins

# Create the leaflet map
map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add OpenStreetMap as the base layer
  addPolygons(data = newham, color = "black", weight = 1, opacity = 1, fillOpacity = 0.6, fillColor = "black") %>%  # Lower the opacity of the black fill
  setView(lng = mean(crime_coords[, 1]), lat = mean(crime_coords[, 2]), zoom = 12) %>%
  addCircleMarkers(lng = crime_coords[, 1], lat = crime_coords[, 2], color = "red", radius = 5, fillOpacity = 0.6) %>%  # Plot crime data as circle markers
  addHeatmap(lng = crime_coords[, 1], lat = crime_coords[, 2], intensity = rep(1, nrow(crime_coords)), 
             blur = 20, max = 0.5, radius = 15, gradient = c('white', 'red')) %>%  # Add heatmap
  addCircleMarkers(lng = newham_pub_coords[, 1], lat = newham_pub_coords[, 2], color = "#d2f8fe", radius = 6, fillOpacity = 0.7) %>%  # Add pins from 'newham_pub' in blue
  addScaleBar(position = "bottomright")  # Add scale bar in the bottom-right corner

# Print the map
map








