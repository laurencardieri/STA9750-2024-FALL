library(tigris)
library(ggplot2)
library(sf)

# Load New York counties shapefile
ny_counties <- counties(state = "NY", cb = TRUE)

# Convert to sf object
ny_counties_sf <- st_as_sf(ny_counties)

# Plot map of New York counties
ggplot(data = ny_counties_sf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Counties of New York State",
       caption = "Source: US Census Bureau")
