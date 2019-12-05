library(tidyverse)
library(stringr)
library(sf)
library(leaflet)
#library(rgdal)
library(htmltools)
#library(mapview)
library(htmlwidgets)



setwd("C:/Users/Toshiba/Google Drive/WORK/LIDA/LowCarbonCities/EPC")

# read in data 2019
EPC_cum_2019 <- read.csv("cumEPC2019.csv", header = TRUE, sep = ",")

EPC_LSOA <- EPC_cum_2019 %>%
  group_by(LSOA) %>%
  summarize(median_E_efficiency = median(CURRENT_ENERGY_EFFICIENCY),
            median_pot_E_efficiency = median(POTENTIAL_ENERGY_EFFICIENCY),
            median_environm_impact = median(ENVIRONMENT_IMPACT_CURRENT),
            median_pot_environm_impact = median(ENVIRONMENT_IMPACT_POTENTIAL),
            median_E_consumption = median(ENERGY_CONSUMPTION_CURRENT),
            median_pot_E_consumption = median(ENERGY_CONSUMPTION_POTENTIAL),
            median_CO2_emissions = median(CO2_EMISSIONS_CURRENT),
            median_pot_CO2_emissions = median(CO2_EMISSIONS_POTENTIAL),
            nr_houses = n_distinct(BUILDING_REFERENCE_NUMBER)
            )

EPC_LSOA <- na.omit(EPC_LSOA) 


# read in census data
census_data <- read.csv("data/census_OA_data_Yorkshire_and_Humber.csv", header = TRUE, sep = ",")
lut_oa <- read.csv("data/PCD_OA_LSOA_MSOA_LAD_AUG19_UK_LU/PCD_OA_LSOA_MSOA_LAD_AUG19_UK_LU.csv", header = TRUE, sep = ",")
lut_oa <- lut_oa %>%
  select(oa11cd,lsoa11cd,lsoa11nm,ladnm) %>%
  filter(ladnm == "Leeds") %>% 
  distinct()
lut_oa <- rename(lut_oa, OA_Code = oa11cd)
census_data <- census_data %>%
  left_join(lut_oa, by = "OA_Code")

census_data_leeds <- census_data %>%
  group_by(lsoa11cd) %>%
  summarise(nr_households = sum(N_Households))
census_data_leeds <- rename(census_data_leeds, LSOA = lsoa11cd)


# LSOA 2011 boundaries (from https://borders.ukdataservice.ac.uk/)
shape_boundaries <- read_sf(dsn = "data/lsoa_boundaries_2011", layer = "england_lsoa_2011")
#select rows for Leeds and columns LSOA code and name, geometry column is 'sticky' and gets included automatically
leeds_boundaries <- shape_boundaries[grep("Leeds", shape_boundaries$name), c("code", "name")]
#rename columns - LSOA_ID will be used to match to other LSOA datasets
colnames(leeds_boundaries)[colnames(leeds_boundaries)=="code"] <- "LSOA"
colnames(leeds_boundaries)[colnames(leeds_boundaries)=="name"] <- "LSOA_name"
#!need to convert osgb36 (our data) to wgs84 (leaflet)
#EPSG:27700 to EPSG:4326
leeds_boundaries_wgs84 = st_transform(leeds_boundaries, 4326)
# merge leeds data with shape file
EPC_LSOA <- merge(leeds_boundaries_wgs84,EPC_LSOA,by="LSOA")

#figure out best scale to use
#for now EPC bands
bins <- c(1,21,39,55,69,81,92,100)
pal <- colorBin("RdYlGn", domain = c(1,100), bins = bins)
#pal <- colorBin("RdYlGn", EPC_LSOA$median_diff_E_efficiency, 5, pretty = FALSE)

labels <- paste("<p>", EPC_LSOA$LSOA, "</p>",
                "<p>", "median energy efficiency: ", round(EPC_LSOA$median_E_efficiency, digits = 4), "</p>",
                sep="")

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = EPC_LSOA,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(EPC_LSOA$median_E_efficiency),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels, HTML)
  )
m
addLegend(m, position = "bottomleft", pal = pal, values = EPC_LSOA$median_E_efficiency)





#  CO2 
bins <- seq(from = min(EPC_LSOA$median_CO2_emissions), to = max(EPC_LSOA$median_CO2_emissions), length = 7)
pal <- colorBin("RdYlGn", domain = c(min(EPC_LSOA$median_CO2_emissions),max(EPC_LSOA$median_CO2_emissions)), bins = bins, reverse = TRUE)

labels <- paste("<p>", EPC_LSOA$LSOA, "</p>",
                "<p>", "median CO2 emissions: ", round(EPC_LSOA$median_CO2_emissions, digits = 4), "</p>",
                sep="")

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = EPC_LSOA,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(EPC_LSOA$median_CO2_emissions),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels, HTML)
  )
m
addLegend(m, position = "bottomleft", pal = pal, values = EPC_LSOA$median_CO2_emissions)





#  nr houses (building ref nrs)
bins <- seq(from = min(EPC_LSOA$nr_houses), to = max(EPC_LSOA$nr_houses), length = 5)
pal <- colorBin("Blues", domain = c(min(EPC_LSOA$nr_houses),max(EPC_LSOA$nr_houses)), bins = bins)

labels <- paste("<p>", EPC_LSOA$LSOA, "</p>",
                "<p>", "median CO2 emissions: ", round(EPC_LSOA$nr_houses, digits = 4), "</p>",
                sep="")

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = EPC_LSOA,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(EPC_LSOA$nr_houses),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels, HTML)
  )
m
addLegend(m, position = "bottomleft", pal = pal, values = EPC_LSOA$nr_houses)







#  nr houses (building ref nrs) as % of nr households
EPC_LSOA <- EPC_LSOA %>%
  left_join(census_data_leeds, by = "LSOA")
EPC_LSOA$perc_houses <- 100 * EPC_LSOA$nr_houses / EPC_LSOA$nr_households

bins <- seq(from = min(EPC_LSOA$perc_houses), to = max(EPC_LSOA$perc_houses), length = 5)
pal <- colorBin("Blues", domain = c(min(EPC_LSOA$perc_houses),max(EPC_LSOA$perc_houses)), bins = bins)

labels <- paste("<p>", EPC_LSOA$LSOA, "</p>",
                "<p>", "median CO2 emissions: ", round(EPC_LSOA$perc_houses, digits = 4), "</p>",
                sep="")

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng=-1.49,lat=53.86, zoom = 11) %>% #base map is centred on Leeds
  addPolygons(data = EPC_LSOA,
              weight = 1,
              smoothFactor = 0.5,
              color = "grey",
              fillOpacity = 0.8,
              fillColor = pal(EPC_LSOA$perc_houses),
              highlight = highlightOptions(
                weight = 5,
                color = "#666666",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels, HTML)
  )
m
addLegend(m, position = "bottomleft", pal = pal, values = EPC_LSOA$perc_houses)

hist(EPC_LSOA$perc_houses, xlab = "nr houses 2019 / nr households 2011", main = "% houses with EPC")












# Correlate with OTHER DATA
#IMD data from gov.uk Index of Multiple Deprivation 2019
all_IMD <- read_sf(dsn = "data", layer = "IMD_2019")
#so drop geometry from shape_IMD
st_geometry(all_IMD) <- NULL
#select rows for Leeds and columns LSOA code, name and IMD 
leeds_IMD <- all_IMD[grep("Leeds", all_IMD$lsoa11nm), c("lsoa11cd","IMDScore","TotPop")]
#rename columns
colnames(leeds_IMD)[colnames(leeds_IMD)=="lsoa11cd"] <- "LSOA"
colnames(leeds_IMD)[colnames(leeds_IMD)=="TotPop"] <- "total_population"
EPC_LSOA <- merge(EPC_LSOA,leeds_IMD,by="LSOA")


#fuel poverty data
all_fuelpoverty <- read.csv("data/Fuel_poverty_LSOA_2017.csv", header = TRUE, sep = ",")
leeds_fuelpoverty <- all_fuelpoverty[grep("Leeds", all_fuelpoverty$LSOA.Name), c("LSOA.Code", "Estimated.number.of.households", "Proportion.of.households.fuel.poor....")]
colnames(leeds_fuelpoverty)[colnames(leeds_fuelpoverty)=="LSOA.Code"] <- "LSOA"
colnames(leeds_fuelpoverty)[colnames(leeds_fuelpoverty)=="Estimated.number.of.households"] <- "nr_households"
colnames(leeds_fuelpoverty)[colnames(leeds_fuelpoverty)=="Proportion.of.households.fuel.poor...."] <- "perc_fuelpoor_households"
EPC_LSOA <- merge(EPC_LSOA,leeds_fuelpoverty,by="LSOA")


#electricity consumption
all_electricityconsumption <- read.csv("data/LSOA_domestic_electricity_2017.csv", header = TRUE, sep = ",")
colnames(all_electricityconsumption)[colnames(all_electricityconsumption)=="Lower.Layer.Super.Output.Area..LSOA..Code"] <- "LSOA"
colnames(all_electricityconsumption)[colnames(all_electricityconsumption)=="Total.domestic.electricity.consumption..kWh."] <- "total_electrconsump_kwh"
colnames(all_electricityconsumption)[colnames(all_electricityconsumption)=="Total.number.of.domestic.electricity.meters"] <- "nr_electr_meters"
colnames(all_electricityconsumption)[colnames(all_electricityconsumption)=="Mean.domestic.electricity.consumption...kWh.per.meter."] <- "mean_electrconsump_kwhpermeter"
colnames(all_electricityconsumption)[colnames(all_electricityconsumption)=="Median.domestic.electricity.consumption...kWh.per.meter."] <- "median_electrconsump_kwhpermeter"
leeds_electricityconsumption <- all_electricityconsumption[grep("Leeds", all_electricityconsumption$Lower.Layer.Super.Output.Area..LSOA..Name), c("LSOA", "total_electrconsump_kwh", "nr_electr_meters","mean_electrconsump_kwhpermeter","median_electrconsump_kwhpermeter")]
EPC_LSOA <- merge(EPC_LSOA,leeds_electricityconsumption,by="LSOA")


#gas consumption
all_gasconsumption <- read.csv("data/LSOA_domestic_gas_2017.csv", header = TRUE, sep = ",")
colnames(all_gasconsumption)[colnames(all_gasconsumption)=="Lower.Layer.Super.Output.Area..LSOA..Code"] <- "LSOA"
colnames(all_gasconsumption)[colnames(all_gasconsumption)=="Consumption..kWh."] <- "total_gasconsump_kwh"
colnames(all_gasconsumption)[colnames(all_gasconsumption)=="Number.of.meters"] <- "nr_gas_meters"
colnames(all_gasconsumption)[colnames(all_gasconsumption)=="Mean.consumption..kWh.per.meter."] <- "mean_gasconsump_kwhpermeter"
colnames(all_gasconsumption)[colnames(all_gasconsumption)=="Median.consumption..kWh.per.meter."] <- "median_gasconsump_kwhpermeter"
leeds_gasconsumption <- all_gasconsumption[grep("Leeds", all_gasconsumption$LSOA.Name), c("LSOA", "total_gasconsump_kwh", "nr_gas_meters","mean_gasconsump_kwhpermeter","median_gasconsump_kwhpermeter")]
EPC_LSOA <- merge(EPC_LSOA,leeds_gasconsumption,by="LSOA")


#HPSSA Dataset 46: Median price paid for residential properties by LSOA, from ONS
median_house_prices <- read.csv("data/hpssadataset46medianpricepaidforresidentialpropertiesbylsoa.csv", header = TRUE, sep = ",")
colnames(median_house_prices)[colnames(median_house_prices)=="LSOA.code"] <- "LSOA"
colnames(median_house_prices)[colnames(median_house_prices)=="Year.ending.Dec.2008"] <- "median_house_price_2008"
colnames(median_house_prices)[colnames(median_house_prices)=="Year.ending.Dec.2018"] <- "median_house_price_2018"
median_house_prices <- median_house_prices[grep("Leeds", median_house_prices$LSOA.name), c("LSOA", "median_house_price_2008", "median_house_price_2018")]
EPC_LSOA <- merge(EPC_LSOA,median_house_prices,by="LSOA")


# median_house_price_2008 and 2018 need : replaced by NA and converting to numeric
EPC_LSOA$median_house_price_2008 <- na_if(EPC_LSOA$median_house_price_2008, ":")
EPC_LSOA$median_house_price_2018 <- na_if(EPC_LSOA$median_house_price_2018, ":")
# factor to numeric
EPC_LSOA$median_house_price_2008 <- as.character(EPC_LSOA$median_house_price_2008)
EPC_LSOA$median_house_price_2008 <- gsub(",","",EPC_LSOA$median_house_price_2008)
EPC_LSOA$median_house_price_2008 <- as.numeric(EPC_LSOA$median_house_price_2008)
EPC_LSOA$median_house_price_2018 <- as.character(EPC_LSOA$median_house_price_2018)
EPC_LSOA$median_house_price_2018 <- gsub(",","",EPC_LSOA$median_house_price_2018)
EPC_LSOA$median_house_price_2018 <- as.numeric(EPC_LSOA$median_house_price_2018)
# create new columns for difference and percentage difference
EPC_LSOA <- mutate(EPC_LSOA, incr_medianhouseprice_08_18 = median_house_price_2018 - median_house_price_2008)
EPC_LSOA <- mutate(EPC_LSOA, percincr_medianhouseprice_08_18 = 100 * (median_house_price_2018 - median_house_price_2008) / median_house_price_2008)

# other data need converting to numeric too
EPC_LSOA$total_gasconsump_kwh <- as.numeric(gsub(",","",as.character(EPC_LSOA$total_gasconsump_kwh)))
EPC_LSOA$nr_gas_meters <- as.numeric(gsub(",","",as.character(EPC_LSOA$nr_gas_meters)))
EPC_LSOA$mean_gasconsump_kwhpermeter <- as.numeric(gsub(",","",as.character(EPC_LSOA$mean_gasconsump_kwhpermeter)))
EPC_LSOA$median_gasconsump_kwhpermeter <- as.numeric(gsub(",","",as.character(EPC_LSOA$median_gasconsump_kwhpermeter)))



write.csv(EPC_LSOA, file = "LSOA_data_4_corr_NEW.csv",row.names=FALSE)
EPC_LSOA <- read.csv("C:/Users/Toshiba/Desktop/test_R/EPC/LSOA_data_4_corr.csv", header = TRUE, sep = ",")


# correlations
library(GGally)
library(ggplot2)
library(Hmisc)          # produces correlation matrices with p-values
library(ppcor)          # assesses partial correlations



hist(results_LSOA$incr_medianhouseprice_08_18)
plot(results_LSOA$median_diff_E_efficiency,results_LSOA$incr_medianhouseprice_08_18)
abline(lm(results_LSOA$median_diff_E_efficiency ~ results_LSOA$incr_medianhouseprice_08_18))
cor(results_LSOA$median_diff_E_efficiency,results_LSOA$incr_medianhouseprice_08_18)

ggcorr(EPC_LSOA[,3:27],
       label=TRUE,
       label_alpha = 0.2)

ggcorr(EPC_LSOA[, 10:13],
       label = TRUE,
       label_alpha = TRUE)

qplot(x = EPC_LSOA$incr_medianhouseprice_08_18,
      y = EPC_LSOA$median_diff_E_efficiency,
      data = EPC_LSOA,
      geom = c("point","smooth"),
      method = "lm",
      alpha =I(1/5),
      se = FALSE
)

qplot(x = EPC_LSOA$incr_medianhouseprice_08_18, 
      y = EPC_LSOA$median_diff_E_efficiency, 
      data = EPC_LSOA) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Title")

ggpairs(EPC_LSOA,
        columns = c("incr_medianhouseprice_08_18","median_diff_E_efficiency","IMDScore"),
        upper = list(continuous = "smooth"),
        lower = list(continuous = "smooth")
)


pairs(EPC_LSOA[, c(5:10)])





p1 <- qplot(x = x1, y = y1, data = anscombe)
p2 <- qplot(x = x2, y = y2, data = anscombe)

rcorr(as.matrix(golf[, c(1, 3:9)]))

To assess the correlation between any two questions or create a correlation matrix across all questions we can use the cor(), cor.test(), and rcorr() (Hmisc package) functions and simply specify method = 'spearman':
  
  p3 <- qplot(x = x3, y = y3, data = anscombe)
p4 <- qplot(x = x4, y = y4, data = anscombe)

grid.arrange(p1, p2, p3, p4, ncol = 2, 
             top = textGrob("Anscombe's Quartet"))
