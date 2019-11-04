#first run EPC_prep_data.R to get the data

library(leaflet)
library(dplyr)
#library(ggmap)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(tidyverse)
library(sf)

setwd("\\\\ds.leeds.ac.uk/staff/staff19/mednne/R_scripts/EPC_data")

#-----------------------------------------------------------------
# LSOA EPC summary data (from EPC_prep_data.R)
#-----------------------------------------------------------------
LSOA_EPC <- read.csv("data/EPC_at_LSOA.csv", header = TRUE, sep = ",")


#-----------------------------------------------------------------
# LSOA 2011 boundaries (from https://borders.ukdataservice.ac.uk/)
#-----------------------------------------------------------------
# SOURCE: 2011, 
shape_boundaries <- read_sf(dsn = "data/lsoa_boundaries_2011", layer = "england_lsoa_2011")
#select rows for Leeds and columns LSOA code and name, geometry column is 'sticky' and gets included automatically
leeds_boundaries <- shape_boundaries[grep("Leeds", shape_boundaries$name), c("code", "name")]
#rename columns - LSOA_ID will be used to match to other LSOA datasets
colnames(leeds_boundaries)[colnames(leeds_boundaries)=="code"] <- "LSOA"
colnames(leeds_boundaries)[colnames(leeds_boundaries)=="name"] <- "LSOA_name"
#optional, plot to check:
#plot(leeds_boundaries["LSOA_name"])

#!need to convert osgb36 (our data) to wgs84 (leaflet)
#EPSG:27700 to EPSG:4326
leeds_boundaries_wgs84 = st_transform(leeds_boundaries, 4326)

# merge leeds data with shape file; note this also drops the NA LSOA
leeds_data <- merge(leeds_boundaries_wgs84,LSOA_EPC,by="LSOA")



# Choropleth


#figure out best scale to use
#for now based on quantile(leeds_data$median_diff_E_efficiency)
bins <- c(min(leeds_data$median_diff_E_efficiency), 0, 1.5, 3, max(leeds_data$median_diff_E_efficiency))
pal <- colorBin("RdYlGn", domain = c(-18,28), bins = bins)
#pal <- colorBin("RdYlGn", leeds_data$median_diff_E_efficiency, 5, pretty = FALSE)

labels <- paste("<p>", leeds_data$LSOA, "</p>",
                "<p>", "median difference: ", round(leeds_data$median_diff_E_efficiency, digits = 4), "</p>",
                sep="")

m <- leaflet() %>%
  #addTiles() %>%
  #addProviderTiles(providers$Stamen.TonerLite) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.5491,lat=53.8008, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_diff_E_efficiency),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels, HTML)
  )
m

addLegend(m,position = "topright", pal = pal, values = leeds_data$median_diff_E_efficiency, data = getMapData(m))







#figure out best scale to use
bins <- c(min(leeds_data$median_percdiff_E_efficiency), -10, -5, 0, 5, 10, max(leeds_data$median_percdiff_E_efficiency))
pal <- colorBin("RdYlGn", 
                domain = c(min(leeds_data$median_percdiff_E_efficiency, max(leeds_data$median_percdiff_E_efficiency))), 
                bins = bins)
#pal <- colorBin("RdYlGn", leeds_data$median_diff_E_efficiency, 5, pretty = FALSE)

labels <- paste("<p>", leeds_data$LSOA, "</p>",
                "<p>", "median difference: ", round(leeds_data$median_percdiff_E_efficiency, digits = 4), "</p>",
                sep="")

m <- leaflet() %>%
  #addTiles() %>%
  #addProviderTiles(providers$Stamen.TonerLite) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.5491,lat=53.8008, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_percdiff_E_efficiency),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels, HTML)
  )
m

addLegend(m,position = "topright", pal = pal, values = leeds_data$median_percdiff_E_efficiency, data = getMapData(m))


