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




ny_counties <- counties(state = "NY", cb = TRUE)

ny_counties_sf <- st_as_sf(ny_counties)


ggplot(data = ny_counties_sf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Counties of New York State",
       caption = "Source: US Census Bureau")



# Only total population 
estimated_total <- total_population_race |>
  filter(grepl("Estimated Total", race))


estimated_total_long <- estimated_total |>
  pivot_longer(cols = starts_with("y"),
               names_to = "year",
               values_to = "population") |>
  mutate(year = as.numeric(gsub("y", "", year))) |>
  select(-race, -NAME)


NY_pop_long <- ny_counties_sf |>
  left_join(estimated_total_long, by = "GEOID")



p <- ggplot(NY_pop_long_fixed) +
  geom_sf(aes(fill = population, group = GEOID), color = "black") +  # Keep counties stable
  scale_fill_gradient(
    name = "Population (in thousands)",
    low = "lightblue",  # Light blue for least populated
    high = "darkblue",  # Dark blue for most populated
    trans = "log",  # Log transformation for smoother scale
    labels = scales::label_number(scale = 0.001, accuracy = 0.1)  # In thousands
  ) +
  theme_bw() +
  labs(
    title = "Population by County in New York",
    subtitle = "Year: {floor(frame_time)}",
    caption = "Source: Your Dataset"
  ) +
  theme(legend.position = "right") +
  transition_time(year) +
  ease_aes('linear')

# Render the animation
animate(p)