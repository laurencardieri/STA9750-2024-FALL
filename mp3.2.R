zip_urls <- c(
  "https://cdmaps.polisci.ucla.edu/shp/districts095.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts096.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts097.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts098.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts099.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts100.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts101.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts102.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts103.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts104.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts105.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts106.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts107.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts108.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts109.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts110.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts111.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts112.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts113.zip",
  "https://cdmaps.polisci.ucla.edu/shp/districts114.zip"
)

destination_folder <- "extracted_files"

if (!dir.exists(destination_folder)) {
  dir.create(destination_folder)}

for (i in seq_along(zip_urls)) {
  zip_url <- zip_url[i]
  
  file_extract <- paste0("districts", i, ".shp")
  
  zip_name <- basename(zip_url)
  
  local_zip <- file.path(destination_folder, zip_name)
  
  download.file(zip_url, local_zip, mode = "wb")
  
  unzip(local_zip, files = file_extract, exdir = destination_folder)
  cat("extracted", file_extract, "from", zip_name, "https://cdmaps.polisci.ucla.edu/")
}

zip_files <- list.files(pattern = "\\.zip$")

dir.create("unzipped_files")

for (zip in zip_files) {
  unzip(zip, exdir = "unzipped_files")
}

shp_files <- list.files("unzipped_files", pattern = "\\.shp$", full.names = TRUE)

first_shapefile <- shp_files[[1]]

plot(first_shapefile)


shape_path <- "C:/Users/laure/OneDrive/Documents/STA9750-2024-FALL/districts095.shp"
my_shape <- st_read(shape_path)


file.exists(shape_path)




library(utils)
install.packages("sf")
library(sf)


plot(s.sf, max.plot = 20)


library(ggplot2)


library(readr)
library(sf)
library(dplyr)
library(tidyr)
library(tidyverse)
library(DT)
library(ggplot2)
library(gganimate)


president_2000 <- X1976_2020_president |>
  filter(year == '2000',
         candidate %in% c("BUSH, GEORGE W.", "GORE, AL")) |>
  group_by(state) |>
  summarize(
    highest_votes = max(candidatevotes),
    party = party_simplified[which.max(candidatevotes)]
  )


Shape_2000 <- s.sf |>
  mutate(STATENAME = toupper(trimws(STATENAME))) |>
  left_join(president_2000, join_by(STATENAME == state)) 
  

USA2000Map <- ggplot(Shape_2000, aes(geometry = geometry, fill = party),
       color = "black") +
  geom_sf() + 
  scale_fill_manual(values = c("REPUBLICAN" = "firebrick2", "DEMOCRAT" = "royalblue2")) +
  theme_minimal() +
  coord_sf(xlim = c(-180, -50), ylim = c(10,80), expand = FALSE) +
  labs(title = "2000 Presidential Election Electoral College Results", fill = "Winning Party") 

print(USA2000Map)


install.packages('plyr', repos = "http://cran.us.r-project.org")

td <- tempdir(); 
zip_contents <- unzip("districtShapes/districts106.zip", 
                      exdir = td)

district106_shp <- zip_contents[grepl("shp$", zip_contents)]
nyc_sf <- read_sf(district106_shp)
nyc_sf


USA2000results <- s.sf |>
  mutate(STATENAME = toupper(trimws(STATENAME))) |>
  left_join(president_2000, join_by(STATENAME == state)) 

USA2000Map <- ggplot(USA2000results, aes(geometry = geometry, fill = party),
                     color = "black") +
  geom_sf() + 
  scale_fill_manual(values = c("REPUBLICAN" = "firebrick2", "DEMOCRAT" = "royalblue2")) +
  theme_minimal() +
  coord_sf(xlim = c(-180, -50), ylim = c(10,80), expand = FALSE) +
  labs(title = "2000 Presidential Election Electoral College Results", fill = "Winning Party") 

print(USA2000Map)

hawaii_map <- ggplot(hawaii, aes(geometry = geometry,
                                 fill = "Winning Party"),
                     color = "black") +
  geom_sf() +
  scale_fill_manual(values = c("REPUBLICAN" = "firebrick2", "DEMOCRAT" = "royalblue2")) +
  theme_void() +
  theme(legend.position = "none") +
  coord_sf(xlim = c(-161, -154), ylim = c(18,23), expand = FALSE)



alaska_map <- ggplot(alaska, aes(geometry = geometry,
                                 fill = "Winning Party"),
                     color = "black") +
  geom_sf() +
  scale_fill_manual(values = c("REPUBLICAN" = "firebrick2", "DEMOCRAT" = "royalblue2")) +
  theme_void() +
  theme(legend.position = "none") +
  coord_sf(xlim = c(-125, -65), ylim = c(50,50), expand = FALSE)
  


whole_map_2000 <- USA2000Map + 
  annotation_custom(ggplotGrob(alaska_map),
                    xmin = -120, xmax = -130, 
                    ymin = 15, ymax = 40) +
  annotation_custom(ggplotGrob(hawaii_map),
                    xmin = -115, xmax = -100,
                    ymin = 20, ymax = 30)

print(whole_map_2000)





actual_results <- data.frame(
  year = c(1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020),
  president = c("CARTER, JIMMY", "REAGAN, RONALD", "REAGAN, RONALD", "BUSH, GEORGE H.W.",
                "CLINTON, BILL", "CLINTON, BILL", "BUSH, GEORGE W.", "BUSH, GEORGE W.",
                "OBAMA, BARACK H.", "OBAMA, BARACK H.", "TRUMP, DONALD J.", "BIDEN, JOSEPH R. JR"),
  party = c("DEMOCRAT", "REPUBLICAN", "REPUBLICAN", "REPUBLICAN", "DEMOCRAT", "DEMOCRAT",
            "REPUBLICAN", "REPUBLICAN", "DEMOCRAT", "DEMOCRAT", "REPUBLICAN", "DEMOCRAT"),
  Electoral_college_votes = c(297, 489, 525, 426, 370, 379, 271, 286, 365, 332, 304, 306))

datatable(actual_results)











