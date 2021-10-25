setwd("~/R/redlining_project")

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
library(mapview)

options(tigris_use_cache = TRUE)


# read in shapefiles ------------------------------------------------------

# define current city limits
pdx <- st_read("input/pdx_city_boundaries_espg4326/pdx_city_boundaries_espg4326.shp"
                  ,layer = "pdx_city_boundaries_espg4326"
               ,quiet = TRUE)

# Import and tidy emission facilities' data --------------------------------

# import emissions lat-long data frame and filter for Portland Metro
# DO NOT ALTER, as it is recalled in the map object
# This object includes ALL emitting facilities in the Portland Metro area,
# including those for which we do not have TRVs
pdx_sites <- read_csv("input/TRAACS_permits.csv") %>%
  filter(city_text == "PORTLAND")

pdx_sites_sf <- st_as_sf(pdx_sites
                              ,coords = c("longitude", "latitude")
                              ,crs = 4326
                              )

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
pdx_emissions <- left_join(pdx_sites,
               caoemissions2016,
               by = "source_number") %>%
  drop_na(pollutant_name) %>%
  drop_na(emissions_2016_lb_per_year)

# Merge portland_emissions and tox_ref
# This object now includes only emissions for which we have TRVs
ptox <- inner_join(pdx_emissions, tox_ref)

# calculate toxicity risk indicator (TRI) value for each source
ptox <- transform(ptox
            ,cancerTRI = ptox$emissions_2016_lb_per_year/ptox$chronic_cancer
            ,noncancerTRI = ptox$emissions_2016_lb_per_year/ptox$chronic_noncancer
            ,acuteTRI = ptox$emissions_2016_lb_per_year/ptox$acute
            ) 

# isolate the facilities in Portland that report any emissions
ptox.sites <- ptox %>%
  distinct(source_name
           ,.keep_all = TRUE)

# sum TRIs for each category
ptox.TRIsum <- summarise(group_by(ptox, source_name)
            ,sum_cancerTRI = round(sum(cancerTRI, na.rm = TRUE)
                                   ,digits = 2)
            ,sum_noncancerTRI = round(sum(noncancerTRI, na.rm = TRUE)
                                      ,digits = 2)
            ,sum_acuteTRI = round(sum(acuteTRI, na.rm = TRUE)
                                  ,digits = 2)
            )

# merge back into ptox.sites
ptox.sites.sum <- merge(ptox.sites, ptox.TRIsum)

# import HOLC redlining map from 1934
pdx_redlined <- st_read("input/ORPortland1937.geojson"
                             ,layer = "ORPortland1937"
                             ,quiet = TRUE) %>%
  sf::st_buffer(10)

pdx_redlined$pop <- paste0(pdx_redlined$name
                                ," "
                                ,pdx_redlined$holc_grade)

holc.sites <- st_intersection(pdx_redlined
                                  ,pdx_sites_sf)

holc.sites.count <- holc.sites %>% count(holc_grade)

# transforms lat-long into an sf geometry feature to then intersect w/HOLC areas
ptox.sf <- st_as_sf(ptox
                    ,coords = c("longitude", "latitude")
                    ,crs = 4326
)

#identify intersection between HOLC neighborhoods and emitting facilities
holc.tox <- st_intersection(pdx_redlined
                              ,ptox.sf)

##trees <- st_read("input/Parks_Tree_Inventory/Parks_Tree_Inventory.shp"
  ##               ,layer = "Parks_Tree_Inventory")

moss2015 <- read_excel("input/2015_Cd_As_Se_moss_intensified_sample_0.xlsx")

moss2013 <- read.csv("input/pnw_gtr938-data_0.csv"
                     ,header = TRUE)


# Census Data -------------------------------------------------------------

demo10 <- get_decennial(geography = "county",)

moss.icons <- awesomeIcons(
  icon = 'leaf'
  ,iconColor = 'green'
  ,library = 'ion'
  ,markerColor = 'white'
)

tox.icons <- awesomeIcons(
  icon = 'cloud'
  ,iconColor = 'black'
  ,library = 'ion'
  ,markerColor = 'white'
)

# Create map --------------------------------------------------------------

# create visualization
map <- leaflet(height = 600
               ) %>%
  
  # base group
  addProviderTiles(providers$CartoDB.Positron,
                   group = "Basemap") %>%
  
  # current portland city limits
  addPolygons(data = pdx, 
             fill = FALSE,
             group = "Portland City Limits") %>%
  
  # historically-redlined neighborhoods 1937
  addPolygons(data = pdx_redlined,
              stroke = FALSE,
              fillColor = ~colorFactor(palette = c("#009900", "#0000FF", "#FFFF00", "#FF0000"),
                                       levels = c("A", "B", "C", "D")
                                       )(holc_grade)
              ,popup = ~pop
              ,highlightOptions = highlightOptions(color = "blue"
                                                   ,weight = 3
                                                   ,bringToFront = TRUE)
              ,group = "HOLC-Graded Neighborhoods"
              ) %>%
  
 ## addPolygons(data = trees
   ##           ,stroke = FALSE
     ##         ,fillColor = "green"
       ##       ,opacity = 0.5
         ##     , group = "Parks Tree Inventory"
           ##   )%>%
  
  # emission sites as circle markers
  addAwesomeMarkers(
    lng = ptox.sites$longitude,
    lat = ptox.sites$latitude,
    icon = tox.icons,
    popup = paste("Facility Name:", ptox.sites$source_name, "<br>"
                  ,"Sum Cancer TRI:", ptox.sites.sum$sum_cancerTRI, "<br>"
                  ),
    group = "Facilities w/Reported Emissions",
    clusterOptions = markerClusterOptions()
  ) %>%
  
  addCircleMarkers(
    lng = pdx_sites$longitude
    ,lat = pdx_sites$latitude
    ,radius = 2
    ,popup = paste("Facility Name:", pdx_sites$source_name, "<br>"
                   )
    ,group = "All Permitted Facilities"
    ,clusterOptions = markerClusterOptions()
    ) %>%
  
  addAwesomeMarkers(
    lng = moss2015$longitude
    ,lat = moss2015$latitude
    ,icon = moss.icons
    ,popup = paste("Cadmium:", moss2015$Cadmium, "mg/kg", "<br>"
                    ,"Arsenic:", moss2015$Arsenic,"mg/kg", "<br>"
                    ,"Selenium:", moss2015$Selenium, "mg/kg", "<br>"
                   )
    ,group = "2015 Moss Analysis"
     ) %>%
  
  addAwesomeMarkers(
    lng = moss2013$Longitude
    ,lat = moss2013$Latitude
    ,icon = moss.icons
    ,popup = paste("Phosphorus:", moss2013$P, "mg/kg", "<br>")
    ,clusterOptions = markerClusterOptions()
    ,group = "2013 Moss Analysis"
  ) %>%
  
  # layers control
  addLayersControl(
    baseGroups = "Basemap",
    overlayGroups = c("Portland City Limits",
                      "HOLC-Graded Neighborhoods",
                      "All Permitted Facilities",
                      "Facilities w/Reported Emissions"
                      ,"2015 Moss Analysis"
                      ,"2013 Moss Analysis"
                      #,"Parks Tree Inventory"
                      )
    ,options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  hideGroup(c("All Permitted Facilities"
              ,"Facilities w/Reported Emissions"
              ,"2015 Moss Analysis"
              ,"2013 Moss Analysis"))
  
map

# Histograms --------------------------------------------------------------


# histogram of the number of permitted facilities in each HOLC-grade neighborhood
ggplot(holc.sites.count
       ,aes(x = holc_grade
            ,y = n
            ,color = holc_grade)
       ) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n
                ,vjust = -0.3
                ,size = 3.5
                ,color = "black")
            ) +
  scale_fill_manual(values = c("A" = "green"
                                ,"B" = "blue"
                                ,"C" = "yellow"
                                ,"D" = "red")) +
  ggtitle("Total Number of Permitted Facilities Across All HOLC-Graded Neighborhoods") +
  labs(y = "Facility Count"
       , x = "HOLC Grade") +
  theme_minimal() +
  theme(legend.position = "none")
