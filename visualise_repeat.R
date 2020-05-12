library(leaflet)
library(dplyr)
#library(ggmap)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(tidyverse)
library(sf)
library(geosphere)



setwd("C:/Users/Toshiba/Desktop/test_R/EPC")
#read in data

#Leeds boundary data
leeds_data <- read.csv("LSOA_data_4_corr.csv", header = TRUE, sep = ",")
#mock social housing
houses <- read.csv("data/mock_LCC_socialhousing.csv", header = TRUE, sep = ",")
#boundaries
shape_boundaries <- read_sf(dsn = "data/lsoa_boundaries_2011", layer = "england_lsoa_2011")
leeds_boundaries <- shape_boundaries[grep("Leeds", shape_boundaries$name), c("code", "name")]
colnames(leeds_boundaries)[colnames(leeds_boundaries)=="code"] <- "LSOA"
colnames(leeds_boundaries)[colnames(leeds_boundaries)=="name"] <- "LSOA_name"

#!need to convert osgb36 (our data) to wgs84 (leaflet)
#EPSG:27700 to EPSG:4326
#keep the original for later operations using sf (projected coords required)
leeds_boundaries_proj <- leeds_boundaries
leeds_boundaries = st_transform(leeds_boundaries, 4326)
leeds_data <- left_join(leeds_data,leeds_boundaries)

# choropleths: multiple layers and toggle
#figure out best scale to use
bins_IMD <- c(min(leeds_data$IMDScore), 15, 30, 45, 60, 75, max(leeds_data$IMDScore))
pal_IMD <- colorBin("RdYlGn", domain = c(-18,28), bins = bins_IMD, reverse = TRUE)
labels_IMD <- paste("<p>", leeds_data$LSOA, "</p>",
                    "<p>", "IMD: ", round(leeds_data$IMDScore, digits = 4), "</p>",
                    sep="")

bins_EPC <- c(min(leeds_data$median_percdiff_E_efficiency), -10, -5, 0, 5, 10, max(leeds_data$median_percdiff_E_efficiency))
pal_EPC <- colorBin("RdYlGn", domain = c(min(leeds_data$median_percdiff_E_efficiency),max(leeds_data$median_percdiff_E_efficiency)), bins = bins_EPC)
labels_EPC <- paste("<p>", leeds_data$LSOA, "</p>",
                    "<p>", "median % difference EPC: ", round(leeds_data$median_percdiff_E_efficiency, digits = 4), "</p>",
                    sep="")


m <- leaflet() %>%
  #addTiles() %>%
  #addProviderTiles(providers$Stamen.TonerLite) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.5491,lat=53.8008, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_boundaries,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal_EPC(leeds_data$median_percdiff_E_efficiency),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels_EPC, HTML),
              group = "EPC"
  ) %>%
  addPolygons(data = leeds_boundaries,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal_IMD(leeds_data$IMDScore),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels_IMD, HTML),
              group = "IMD"
  ) %>%
  addLegend(values = leeds_data$IMDScore, group = "IMD", pal = pal_IMD,
            position = "bottomright") %>%
  addLegend(values = leeds_data$median_percdiff_E_efficiency, group = "EPC", pal = pal_EPC,
              position = "bottomright") %>%
  addLayersControl(overlayGroups = c("IMD","EPC"),
                     options = layersControlOptions(collapsed = FALSE))
m




#icons
#https://rstudio.github.io/leaflet/markers.html
getColor <- function(houses) {
  sapply(houses$market_value, function(market_value) {
    if(market_value > 110000) {
      "green"
    } else if(market_value > 100000) {
      "orange"
    } else {
      "red"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(houses)
)
leaflet(houses) %>% addTiles() %>%
  addAwesomeMarkers(~lon, ~lat, icon=icons, label=~as.character(market_value))

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.55,lat=53.82, zoom = 11) %>% #base map is centred on Leeds
  addMarkers(lng = houses$lon,
                   lat = houses$lat,
                   label = lapply(houses$assetID, HTML)
  ) %>%
  addCircleMarkers(lng = houses$lon,
                   lat = houses$lat,
                   #clusterOptions = markerClusterOptions(),
                   label = lapply(houses$assetID, HTML),
                   radius = 10,
                   color = "red",
                   stroke = FALSE,
                   fillOpacity = 0.5
  )
m


#heatmap
leaflet(houses) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addWebGLHeatmap(lng=~lon, lat=~lat, size = 6600) %>%
  addMarkers(lng = houses$lon,
             lat = houses$lat,
             label = lapply(houses$assetID, HTML))



#divide houses and plot
centre_point <- slice(houses,1)
houses <- slice(houses,2:20)

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.55,lat=53.82, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_boundaries,
              weight = 1,
              # smoothFactor = 0.5,
              color = "grey",
              # fillOpacity = 0.8,
              label = lapply(leeds_boundaries$LSOA, HTML)
  ) %>%
  addCircleMarkers(lng = houses$lon,
                   lat = houses$lat,
                   label = lapply(houses$assetID, HTML),
                   radius = 5,
                   color = "blue",
                   stroke = FALSE,
                   fillOpacity = 0.5
  ) %>%
  addCircleMarkers(lng = centre_point$lon,
                   lat = centre_point$lat,
                   label = lapply(centre_point$assetID, HTML),
                   radius = 10,
                   color = "red",
                   stroke = FALSE,
                   fillOpacity = 0.5
  )
m

  

# distance each house to centre point
distances_house_centre <- houses %>%
    rowwise() %>%
    mutate(distance = distHaversine(c(lon,lat), c(centre_point$lon,centre_point$lat)))  #distVincentyEllipsoid() is more accurate but slower, both need long and lat and produce result in metres

# from here on, all sf functions so need projected coordinates
# _m to map (back) to leaflet coord system
houses_geom <- st_as_sf(x = houses, coords = c("lon", "lat"))
st_crs(houses_geom) = 4326
houses_proj <- st_transform(houses_geom, 27700)


# distance, matrix LSOAs
st_crs(leeds_boundaries_proj) = st_crs(houses_proj)
houses_LSOA_proj <- st_intersection(houses_proj, leeds_boundaries_proj)
# quick check
houses_LSOA_proj %>%
select(assetID,LSOA) %>%
filter(assetID == 32693)

distance_matrix_houses_LSOAs <- st_distance(houses_proj, leeds_boundaries_proj)
distances <- t(distance_matrix_houses_LSOAs) #transpose
temp <- st_drop_geometry(leeds_boundaries_proj) #remove geometry
distances <- cbind(temp$LSOA,distances) #add LSOA and distances
houseids <- houses_proj %>% pull(assetID)
colnames(distances) <- c("LSOA",as.character(houseids))


# buffer around point and LSOA and plot

#convert centre point and buffer
centre_point_geom <- st_as_sf(x = centre_point, coords = c("lon", "lat"))
st_crs(centre_point_geom) = st_crs(houses_geom)
centre_point_proj <- st_transform(centre_point_geom, 27700)
buffer_centre_point <- st_buffer(centre_point_proj, dist = 50)
buffer_centre_point_m = st_transform(buffer_centre_point, 4326)

#pick LSOA and create buffer
LSOA_E01011267_proj <- leeds_boundaries_proj %>%
filter(LSOA == "E01011267")
buffer_LSOA <- st_buffer(LSOA_E01011267_proj, dist = 50)
LSOA_E01011267 = st_transform(LSOA_E01011267_proj, 4326)
buffer_LSOA_m = st_transform(buffer_LSOA, 4326)

#plot buffer
m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.55,lat=53.82, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = buffer_centre_point_m,
    weight = 1,
    # smoothFactor = 0.5,
    color = "red"
    # fillOpacity = 0.8,
  ) %>%
    addPolygons(data = buffer_LSOA_m,
    weight = 1,
    # smoothFactor = 0.5,
    color = "red"
    # fillOpacity = 0.8,
  ) %>%
  addPolygons(data = LSOA_E01011267,
    weight = 1,
    # smoothFactor = 0.5,
    color = "grey"
    # fillOpacity = 0.8,
  ) %>%
  addCircleMarkers(lng = centre_point$lon,
    lat = centre_point$lat,
    label = lapply(centre_point$assetID, HTML),
    radius = 10,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.5
  )
m



