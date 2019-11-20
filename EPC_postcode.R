library(tidyverse)
library(stringr)
library(sf)
library(leaflet)
#library(rgdal)
library(htmltools)
#library(mapview)
library(htmlwidgets)



setwd("C:/Users/Toshiba/Desktop/test_R/EPC")

# read in data 2019
EPC_2008_2019 <- read.csv("data/certificates_2008_2019.csv", header = TRUE, sep = ",")
EPC_2008_2019 <- select(EPC_2008_2019,ADDRESS1 : MECHANICAL_VENTILATION)


#pick one of the below: LSOA or postcode sector
#AREA <- "LSOA"
AREA <- "POSTSECTOR"

#a) postcode area to LSOA
if (AREA ==  "LSOA") {
  print("You've chosen LSOA")
} else if (AREA ==  "POSTSECTOR") {
  print("You've chosen POSTCODE SECTOR")
} 

if (AREA ==  "LSOA") {
  postcode_2_LSOA <- read.csv("data/PCD_OA_LSOA_MSOA_LAD_AUG19_UK_LU/PCD_OA_LSOA_MSOA_LAD_AUG19_UK_LU.csv", header = TRUE, sep = ",")
  lut <- filter(postcode_2_LSOA,ladnm == "Leeds")
  lut <- rename(lut, POSTCODE = pcds)
  lut <- rename(lut, LSOA = lsoa11cd)
  lut <- select(lut,c(POSTCODE,LSOA))
  EPC_2008_2019 <- EPC_2008_2019 %>%
    left_join(lut, by = "POSTCODE")
  rm(lut,postcode_2_LSOA)

} else if (AREA ==  "POSTSECTOR") {
  #b) postcode area to postcode sector: remove last 2 characters from POSTCODE
  EPC_2008_2019 <- mutate(EPC_2008_2019, POSTSECTOR = str_sub(as.character(EPC_2008_2019$POSTCODE), end=-3))
} 


# create numeric variable for inspection year
EPC_2008_2019$inspection_year <- 
  as.numeric(format(as.Date(EPC_2008_2019$INSPECTION_DATE, format="%Y-%m-%d"),"%Y"))


# summary values per area (LSOA or postcode sector) - one table for each year
# figure out how to do this faster, see eval, parse, sprintf, paste
# for(i in 2007:2019){
#   year <- i
#   LSOA_year <- paste("LSOA", i, sep = "_")
#   nr_houses_year <- paste("nr_houses", i, sep = "_")
#   median_E_efficiency_year <- paste("median_E_efficiency", i, sep = "_")
# 
#   eval(parse(LSOA_year " <- EPC_2008_2019"))
# 
#   LSOA_2007 <- EPC_2008_2019 %>%
#     group_by(LSOA) %>%
#     filter(inspection_year == 2007) %>%
#     summarize(nr_houses_2007 = n(),
#               median_E_efficiency_2007 = median(CURRENT_ENERGY_EFFICIENCY))
# }

if (AREA ==  "LSOA") {
  #a) LSOA
  LSOA_2007 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2007) %>%
    summarize(nr_houses_2007 = n(),
              median_E_efficiency_2007 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2008 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2008) %>%
    summarize(nr_houses_2008 = n(),
              median_E_efficiency_2008 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2009 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2009) %>%
    summarize(nr_houses_2009 = n(),
              median_E_efficiency_2009 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2010 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2010) %>%
    summarize(nr_houses_2010 = n(),
              median_E_efficiency_2010 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2011 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2011) %>%
    summarize(nr_houses_2011 = n(),
              median_E_efficiency_2011 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2012 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2012) %>%
    summarize(nr_houses_2012 = n(),
              median_E_efficiency_2012 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2013 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2013) %>%
    summarize(nr_houses_2013 = n(),
              median_E_efficiency_2013 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2014 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2014) %>%
    summarize(nr_houses_2014 = n(),
              median_E_efficiency_2014 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2015 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2015) %>%
    summarize(nr_houses_2015 = n(),
              median_E_efficiency_2015 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2016 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2016) %>%
    summarize(nr_houses_2016= n(),
              median_E_efficiency_2016 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2017 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2017) %>%
    summarize(nr_houses_2017 = n(),
              median_E_efficiency_2017 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2018 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2018) %>%
    summarize(nr_houses_2018 = n(),
              median_E_efficiency_2018 = median(CURRENT_ENERGY_EFFICIENCY))
  
  LSOA_2019 <- EPC_2008_2019 %>%
    group_by(LSOA) %>%
    filter(inspection_year == 2019) %>%
    summarize(nr_houses_2019 = n(),
              median_E_efficiency_2019 = median(CURRENT_ENERGY_EFFICIENCY))

} else if (AREA ==  "POSTSECTOR") {

  #b) postcode sector
  POSTSECTOR_2007 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2007) %>%  
    summarize(nr_houses_2007 = n(),
              median_E_efficiency_2007 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2008 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2008) %>%  
    summarize(nr_houses_2008 = n(),
              median_E_efficiency_2008 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2009 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2009) %>%  
    summarize(nr_houses_2009 = n(),
              median_E_efficiency_2009 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2010 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2010) %>%  
    summarize(nr_houses_2010 = n(),
              median_E_efficiency_2010 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2011 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2011) %>%  
    summarize(nr_houses_2011 = n(),
              median_E_efficiency_2011 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2012 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2012) %>%  
    summarize(nr_houses_2012 = n(),
              median_E_efficiency_2012 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2013 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2013) %>%  
    summarize(nr_houses_2013 = n(),
              median_E_efficiency_2013 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2014 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2014) %>%  
    summarize(nr_houses_2014 = n(),
              median_E_efficiency_2014 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2015 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2015) %>%  
    summarize(nr_houses_2015 = n(),
              median_E_efficiency_2015 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2016 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2016) %>%  
    summarize(nr_houses_2016= n(),
              median_E_efficiency_2016 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2017 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2017) %>%  
    summarize(nr_houses_2017 = n(),
              median_E_efficiency_2017 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2018 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2018) %>%  
    summarize(nr_houses_2018 = n(),
              median_E_efficiency_2018 = median(CURRENT_ENERGY_EFFICIENCY))
  
  POSTSECTOR_2019 <- EPC_2008_2019 %>%
    group_by(POSTSECTOR) %>%  
    filter(inspection_year == 2019) %>%  
    summarize(nr_houses_2019 = n(),
              median_E_efficiency_2019 = median(CURRENT_ENERGY_EFFICIENCY))
}   
  
  # full list of all areas, NA removed, to start results table
if (AREA ==  "LSOA") {
  LSOA_data <- EPC_2008_2019 %>% 
    select(LSOA) %>%
    na.omit()  %>%
    distinct(LSOA)
} else if (AREA ==  "POSTSECTOR") {
  POSTSECTOR_data <- EPC_2008_2019 %>% 
    select(POSTSECTOR) %>% 
    na.omit()  %>% 
    distinct(POSTSECTOR)
} 

# join all tables together
if (AREA ==  "LSOA") {
  LSOA_data <- LSOA_data %>%
    left_join(LSOA_2007, by = "LSOA") %>%
    left_join(LSOA_2008, by = "LSOA") %>%
    left_join(LSOA_2009, by = "LSOA") %>%
    left_join(LSOA_2010, by = "LSOA") %>%
    left_join(LSOA_2011, by = "LSOA") %>%
    left_join(LSOA_2012, by = "LSOA") %>%
    left_join(LSOA_2013, by = "LSOA") %>%
    left_join(LSOA_2014, by = "LSOA") %>%
    left_join(LSOA_2015, by = "LSOA") %>%
    left_join(LSOA_2016, by = "LSOA") %>%
    left_join(LSOA_2017, by = "LSOA") %>%
    left_join(LSOA_2018, by = "LSOA") %>%
    left_join(LSOA_2019, by = "LSOA")
} else if (AREA ==  "POSTSECTOR") {
  POSTSECTOR_data <- POSTSECTOR_data %>% 
    left_join(POSTSECTOR_2007, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2008, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2009, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2010, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2011, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2012, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2013, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2014, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2015, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2016, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2017, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2018, by = "POSTSECTOR") %>% 
    left_join(POSTSECTOR_2019, by = "POSTSECTOR")
} 

#write to csv
# if (AREA ==  "LSOA") {
#   write.csv(LSOA_data, file = "LSOA_EPC_per_year.csv",row.names=FALSE)
# } else if (AREA ==  "POSTSECTOR") {
#   write.csv(POSTSECTOR_data, file = "POSTCODE_SECTOR_EPC_per_year.csv",row.names=FALSE)
# } 







if (AREA ==  "LSOA") {
  # LSOA 2011 boundaries (from https://borders.ukdataservice.ac.uk/)
  # SOURCE: 2011
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

} else if (AREA ==  "POSTSECTOR") {
  shape_boundaries <- read_sf(dsn = "data/UK-postcode-boundaries-Jan-2015/Distribution", layer = "Sectors")
  leeds_boundaries <- shape_boundaries[grep("^LS", shape_boundaries$name), c("name")]
  colnames(leeds_boundaries)[colnames(leeds_boundaries)=="name"] <- "POSTSECTOR"
  #st_crs(leeds_boundaries)
  leeds_data <- merge(leeds_boundaries,POSTSECTOR_data,by="POSTSECTOR")
  #plot(leeds_boundaries["POSTSECTOR"])
}   

# Choropleth

#figure out best scale to use
#for now EPC bands
bins <- c(1,21,39,55,69,81,92,100)
pal <- colorBin("RdYlGn", domain = c(1,100), bins = bins)
#pal <- colorBin("RdYlGn", leeds_data$median_diff_E_efficiency, 5, pretty = FALSE)


#make 1 plot for each year
# gives error - try on laptop
#mapshot(m, file ="static_map.png")

#WORKAROUND
## install 'webshot' package
library(devtools)
#install_github("wch/webshot")
## load packages
library(webshot)

#2007
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2007, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2007, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2007)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2007, title = "2007", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2007.html", selfcontained = FALSE)
webshot("temp2007.html", file = "EPC_2007.png",
        cliprect = "viewport")


#2008
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2008, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2008, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2008)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2008, title = "2008", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2008.html", selfcontained = FALSE)
webshot("temp2008.html", file = "EPC_2008.png",
        cliprect = "viewport")


#2009
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2009, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2009, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2009)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2009, title = "2009", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2009.html", selfcontained = FALSE)
webshot("temp2009.html", file = "EPC_2009.png",
        cliprect = "viewport")



#2010
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2010, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2010, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2010)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2010, title = "2010", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2010.html", selfcontained = FALSE)
webshot("temp2010.html", file = "EPC_2010.png",
        cliprect = "viewport")



#2011
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2011, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2011, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2011)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2011, title = "2011", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2011.html", selfcontained = FALSE)
webshot("temp2011.html", file = "EPC_2011.png",
        cliprect = "viewport")



#2012
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2012, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2012, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2012)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2012, title = "2012", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2012.html", selfcontained = FALSE)
webshot("temp2012.html", file = "EPC_2012.png",
        cliprect = "viewport")



#2013
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2013, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2013, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2013)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2013, title = "2013", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2013.html", selfcontained = FALSE)
webshot("temp2013.html", file = "EPC_2013.png",
        cliprect = "viewport")



#2014
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2014, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2014, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2014)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2014, title = "2014", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2014.html", selfcontained = FALSE)
webshot("temp2014.html", file = "EPC_2014.png",
        cliprect = "viewport")



#2015
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2015, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2015, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2015)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2015, title = "2015", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2015.html", selfcontained = FALSE)
webshot("temp2015.html", file = "EPC_2015.png",
        cliprect = "viewport")



#2016
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2016, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2016, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2016)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2016, title = "2016", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2016.html", selfcontained = FALSE)
webshot("temp2016.html", file = "EPC_2016.png",
        cliprect = "viewport")



#2017
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2017, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2017, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2017)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2017, title = "2017", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2017.html", selfcontained = FALSE)
webshot("temp2017.html", file = "EPC_2017.png",
        cliprect = "viewport")



#2018
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2018, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2018, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2018)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2018, title = "2018", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2018.html", selfcontained = FALSE)
webshot("temp2018.html", file = "EPC_2018.png",
        cliprect = "viewport")



#2019
if (AREA ==  "LSOA") {
  labels <- paste("<p>", leeds_data$LSOA, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2019, digits = 4), "</p>",
                  sep="")
} else if (AREA ==  "POSTSECTOR") {
  labels <- paste("<p>", leeds_data$POSTSECTOR, "</p>",
                  "<p>", "median energy efficiency: ", round(leeds_data$median_E_efficiency_2019, digits = 4), "</p>",
                  sep="")  
} 

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = leeds_data,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(leeds_data$median_E_efficiency_2019)#,
              # highlight = highlightOptions(
              #   weight = 5,
              #   color = "#666666",
              #   fillOpacity = 0.7,
              #   bringToFront = TRUE),
              # label = lapply(labels, HTML)
  ) %>%
  addLegend(position = "bottomleft", pal = pal, values = leeds_data$median_E_efficiency_2019, title = "2019", data = getMapData(m))
m

## save html to png
saveWidget(m, "temp2019.html", selfcontained = FALSE)
webshot("temp2019.html", file = "EPC_2019.png",
        cliprect = "viewport")



