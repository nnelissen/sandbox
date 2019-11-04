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
LSOA_data <- read.csv("data/LSOA_EPC_per_year.csv", header = TRUE, sep = ",")


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
leeds_data <- merge(leeds_boundaries_wgs84,LSOA_data,by="LSOA")



# Choropleth

#figure out best scale to use
#for now EPC bands
bins <- c(1,21,39,55,69,81,92,100)
pal <- colorBin("RdYlGn", domain = c(1,100), bins = bins)
#pal <- colorBin("RdYlGn", leeds_data$median_diff_E_efficiency, 5, pretty = FALSE)


#make 1 plot for each year
#2007
labels <- paste("<p>", leeds_data$LSOA, "</p>",
                "<p>", "median difference: ", round(leeds_data$median_E_efficiency_2007, digits = 4), "</p>",
                sep="")

m <- leaflet() %>%
  #addTiles() %>%
  #addProviderTiles(providers$Stamen.TonerLite) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  #setView(lng=-1.5491,lat=53.8008, zoom = 11) %>% #base map is centred on Leeds
  setView(lng=-1.55,lat=53.82, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2007),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels, HTML)
  )
m

addLegend(m,position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2007, data = getMapData(m))


# gives error - try on laptop
# mapshot(m, file ="static_map.png")



#2008
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.55,lat=53.82, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2014)
              )
m
addLegend(m,position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2014, title = "2014", data = getMapData(m))
#addControl(m, "test title", position = "topleft",layerId = NULL,data = getMapData(m))









#2014
labels <- paste("<p>", leeds_data$LSOA, "</p>",
                "<p>", "median difference: ", round(leeds_data$median_E_efficiency_2014, digits = 4), "</p>",
                sep="")

m <- leaflet() %>%
  #addTiles() %>%
  #addProviderTiles(providers$Stamen.TonerLite) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  #setView(lng=-1.5491,lat=53.8008, zoom = 11) %>% #base map is centred on Leeds
  setView(lng=-1.55,lat=53.82, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2014),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels, HTML)
  )
m

addLegend(m,position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2014, data = getMapData(m))