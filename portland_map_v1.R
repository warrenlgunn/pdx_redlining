library(tidyverse)
library(tidycensus)
library(rgdal)
library(leaflet)
library(RColorBrewer)
library(flextable)
library(htmltools)
library(htmlwidgets)
library(leafpop)
library(readxl)
library(dplyr)
library(geojsonio)
library(ggplot2)
library(viridis)
library(units)
library(tigris)
library(sf)
library(rmapshaper)

options(tigris_use_cache = TRUE)


# read in shapefiles ------------------------------------------------------

# define current city limits
portland <- st_read("input/pdx_city_boundaries_espg4326/pdx_city_boundaries_espg4326.shp",
                  layer = "pdx_city_boundaries_espg4326")

# import park tree canopy 2017-19
#park.trees <- readOGR("input/Parks_Tree_Inventory/Parks_Tree_Inventory.shp",
 #                   layer = "Parks_Tree_Inventory")

# Import and tidy emission facilities' data --------------------------------

# import emissions lat-long data frame and filter for Portland Metro
# DO NOT ALTER, as it is recalled in the map object
# This object includes ALL emitting facilities in the Portland Metro area,
# including those for which we do not have TRVs
portland_sites <- read_csv("input/TRAACS_permits.csv") %>%
  filter(city_text == "PORTLAND")

# import toxicity reference values
tox_ref <- read_xlsx("input/toxicity_reference_values.xlsx", 4,) %>%
  rename(pollutant_name = chemical,
         cas_code = "cas#")

# convert column values to numerics
tox_ref$chronic_cancer <- as.numeric(tox_ref$chronic_cancer)
tox_ref$chronic_noncancer <- as.numeric(tox_ref$chronic_noncancer)

# import emissions excel document
caoemissions2016 <- read_excel("input/caoemissions2016.XLSX")

# merge portland_sites and caoemissions2016
portland_emissions <- left_join(portland_sites,
               caoemissions2016,
               by = "source_number") %>%
  drop_na(pollutant_name) %>%
  drop_na(emissions_2016_lb_per_year)

# Merge portland_emissions and tox_ref
# This object now includes only emissions for which we have TRVs
ptox <- inner_join(portland_emissions, tox_ref)

# calculate toxicity risk indicator (TRI) value for each source
ptox <- transform(ptox
            ,cancerTRI = ptox$emissions_2016_lb_per_year/ptox$chronic_cancer
            ,noncancerTRI = ptox$emissions_2016_lb_per_year/ptox$chronic_noncancer
            ,acuteTRI = ptox$emissions_2016_lb_per_year/ptox$acute
            ) 

ptox_sum <- group_by(ptox
                     ,source_number) %>%
  summarise(avg_cancerTRI = sum(cancerTRI, na.rm = TRUE)
            ,avg_noncancerTRI = sum(noncancerTRI, na.rm = TRUE)
            ,avg_acuteTRI = sum(acuteTRI, na.rm = TRUE))

# import HOLC redlining map from 1934
portland_redlined <- st_read("input/ORPortland1937.geojson",
                             layer = "ORPortland1937")

# Create map --------------------------------------------------------------

# create visualization
map <- leaflet(height = 600) %>%
  #setView(-45, 112, 4) %>%
  
  # base group
  addProviderTiles(providers$CartoDB.Positron,
                   group = "Grayscale") %>%
  
  # current portland city limits
  addPolygons(data = portland, 
             fill = FALSE,
             group = "Portland City Limits") %>%
  
  # historically-redlined neighborhoods 1937
  addPolygons(data = portland_redlined,
              stroke = FALSE,
              fillColor = ~colorFactor(palette = c("#009900", "#0000FF", "#FFFF00", "#FF0000"),
                                       levels = c("A", "B", "C", "D")
                                       )(holc_grade),
              group = "Redlined Zones") %>%
  
  # emission sites as circle markers
  addCircleMarkers(
    lng = portland_sites$longitude,
    lat = portland_sites$latitude,
    radius = 3,
    popup = paste("Facility Name:", portland_sites$source_name, "<br>"
                  #"Air Toxics:", portland_emissions$pollutant_name, "<br>"
                  ),
    group = "Emitting Facilities",
    clusterOptions = markerClusterOptions()
  ) %>%
  
  # layers control
  addLayersControl(
    baseGroups = "Grayscale",
    overlayGroups = c("Portland City Limits",
                      "Redlined Zones",
                      "Emitting Facilities"
                      ),
    options = layersControlOptions(collapsed = FALSE)
  )

map
