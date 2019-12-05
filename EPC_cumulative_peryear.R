library(tidyverse)

setwd("C:/Users/Toshiba/Google Drive/WORK/LIDA/LowCarbonCities/EPC")

# read in data 2019
EPC_2008_2019 <- read.csv("data/certificates_2008_2019.csv", header = TRUE, sep = ",")
EPC_2008_2019 <- select(EPC_2008_2019,ADDRESS1 : MECHANICAL_VENTILATION)

postcode_2_LSOA <- read.csv("data/PCD_OA_LSOA_MSOA_LAD_AUG19_UK_LU/PCD_OA_LSOA_MSOA_LAD_AUG19_UK_LU.csv", header = TRUE, sep = ",")
lut <- filter(postcode_2_LSOA,ladnm == "Leeds")
lut <- rename(lut, POSTCODE = pcds)
lut <- rename(lut, LSOA = lsoa11cd)
lut <- select(lut,c(POSTCODE,LSOA))
EPC_2008_2019 <- EPC_2008_2019 %>%
  left_join(lut, by = "POSTCODE")
rm(lut,postcode_2_LSOA)

# create numeric variable for inspection year
EPC_2008_2019$inspection_year <- 
  as.numeric(format(as.Date(EPC_2008_2019$INSPECTION_DATE, format="%Y-%m-%d"),"%Y"))


# # OPTION 1 start with all unique buildings and fill in / replace values over the years - same nr rows throughout
# # get unique building identifiers and nr of certificates/inspections for each
# unique_houses <- EPC_2008_2019 %>%
#   filter(inspection_year > 2006) %>% # one observation dates back to 2004!? - remove
#   group_by(BUILDING_REFERENCE_NUMBER) %>%
#   select(BUILDING_REFERENCE_NUMBER)
# for (yearval in 2007:2019)
# {
#   unique_houses <- unique_houses %>%
#     anti_join(filter(EPC_2008_2019,inspection_year == yearval), by = "BUILDING_REFERENCE_NUMBER") %>%
#     bind_rows(filter(EPC_2008_2019,inspection_year == yearval))
# }


# OPTION 2: build up gradually, nr of rows changes each year as more houses added
# start with all certificates from 2007
unique_houses <- EPC_2008_2019 %>%
  filter(inspection_year == 2007)
unique_houses_2007 <- unique_houses # keep variables for each year
# build up by looping around the remaining years
for (yearval in 2008:2019)
{
  unique_houses <- unique_houses %>%
    anti_join(filter(EPC_2008_2019,inspection_year == yearval), by = "BUILDING_REFERENCE_NUMBER") %>%
    bind_rows(filter(EPC_2008_2019,inspection_year == yearval))
  nam <- paste("unique_houses_", yearval, sep = "") # keep variables for each year - create new variable name
  assign(nam, unique_houses) # keep variables for each year - assign value to new variable
}


# calculate how many inspections per building - total
temp_nr_inspections_total <- EPC_2008_2019 %>% 
  group_by(BUILDING_REFERENCE_NUMBER) %>% 
  summarise(nr_inspections_total = n())
# some buildings have been converted into flats and become seperate rows - ass this is true when multiple EPCs for same year
temp_nr_inspections_year <- EPC_2008_2019 %>% 
  group_by(BUILDING_REFERENCE_NUMBER,inspection_year) %>% 
  summarise(nr_inspections_year = n())
temp_nr_inspections_year <- temp_nr_inspections_year %>% 
  group_by(BUILDING_REFERENCE_NUMBER) %>% 
  summarise(nr_inspections_year = n())
unique_houses <- left_join(unique_houses,temp_nr_inspections_total)
unique_houses <- left_join(unique_houses,temp_nr_inspections_year)

#filter(temp_nr_inspections_year,BUILDING_REFERENCE_NUMBER == 1117392568)
#write.csv(unique_houses, file = "unique_houses.csv",row.names=FALSE)
# test <- filter(EPC_2008_2019, BUILDING_REFERENCE_NUMBER == 1117392568)
# write.csv(test, file = "test.csv",row.names=FALSE)














# get values per year (not cumulative)
stats_by_year <- EPC_2008_2019 %>%
  group_by(inspection_year) %>%
  summarise(nr_inspections = n(),
            nr_houses = n_distinct(BUILDING_REFERENCE_NUMBER),
            median_energy_efficiency = median(CURRENT_ENERGY_EFFICIENCY),
            median_CO2_emissions = median(CO2_EMISSIONS_CURRENT)
            )
stats_by_year <- stats_by_year %>%
  filter(inspection_year > 2007)





# do some analyses

# get median, min and max and plot over time
# initialise empty table for summary statistics
stats_CURRENT_ENERGY_EFFICIENCY <- data.frame(
  year = double(),
  median = double(),
  minimum = double(),
  maximum = double()
)
# loop around years and add to table
for (yearval in 2008:2019)
{
  tmp <- get (paste("unique_houses_", yearval, sep = ""))
  temp<-data.frame(yearval,median(tmp$CURRENT_ENERGY_EFFICIENCY),min(tmp$CURRENT_ENERGY_EFFICIENCY),max(tmp$CURRENT_ENERGY_EFFICIENCY))
  names(temp)<-c("year","median","minimum","maximum")
  stats_CURRENT_ENERGY_EFFICIENCY <- rbind(stats_CURRENT_ENERGY_EFFICIENCY, temp)
}

par(mar=c(5.1, 4.1, 4.1, 7), xpd=TRUE)

plot(stats_CURRENT_ENERGY_EFFICIENCY$year,stats_CURRENT_ENERGY_EFFICIENCY$median, main = "Cumulative energy efficiency over time", 
     xlab = "Year", ylab = "Energy efficiency", xlim=c(2008, 2019), ylim=c(0, 120), pch = 16)
points(stats_CURRENT_ENERGY_EFFICIENCY$year,stats_CURRENT_ENERGY_EFFICIENCY$minimum, pch = 16, col = "dark red")
points(stats_CURRENT_ENERGY_EFFICIENCY$year,stats_CURRENT_ENERGY_EFFICIENCY$maximum, pch = 16, col = "dark green")

legend("topright", inset=c(-0.4,0), legend=c("median", "minimum", "maximum"),
       col=c("black", "dark red", "dark green"),pch = 16)



# same for CO2_EMISSIONS_CURRENT
stats_CO2_EMISSIONS_CURRENT <- data.frame(year = double(),median = double(),minimum = double(), maximum = double())
for (yearval in 2008:2019)
{
  tmp <- get (paste("unique_houses_", yearval, sep = ""))
  temp<-data.frame(yearval,median(tmp$CO2_EMISSIONS_CURRENT),min(tmp$CO2_EMISSIONS_CURRENT),max(tmp$CO2_EMISSIONS_CURRENT))
  names(temp)<-c("year","median","minimum","maximum")
  stats_CO2_EMISSIONS_CURRENT <- rbind(stats_CO2_EMISSIONS_CURRENT, temp)
}
par(mar=c(5.1, 4.1, 4.1, 7), xpd=TRUE)
plot(stats_CO2_EMISSIONS_CURRENT$year,stats_CO2_EMISSIONS_CURRENT$median, main = "Cumulative CO2 emissions", 
     xlab = "Year", ylab = "Median CO2 emissions", xlim=c(2008, 2019), ylim=c(3, 4), pch = 16)





# same for nr houses
stats_cum_nr_EPCs <- data.frame(year = double(), nr_inspections = double(), nr_houses = double())
for (yearval in 2008:2019)
{
  tmp <- get (paste("unique_houses_", yearval, sep = ""))
  temp <- data.frame(yearval,nrow(tmp),n_distinct(tmp$BUILDING_REFERENCE_NUMBER))
  names(temp)<-c("year", "nr_inspections", "nr_houses")
  stats_cum_nr_EPCs <- rbind(stats_cum_nr_EPCs, temp)
}
par(mar=c(5.1, 4.1, 4.1, 7), xpd=TRUE)
plot(stats_cum_nr_EPCs$year,stats_cum_nr_EPCs$nr_inspections, main = "Number of EPCs", 
     xlab = "Year", ylab = "Number of EPCs", xlim=c(2008, 2019), ylim=c(0, 250000), pch = 16, col = "dark blue")

points(stats_cum_nr_EPCs$year,stats_cum_nr_EPCs$nr_houses, pch = 16, col = "blue")
points(stats_by_year$inspection_year, stats_by_year$nr_inspections, pch = 16, col = "dark green")
points(stats_by_year$inspection_year, stats_by_year$nr_houses, pch = 16, col = "green")

legend("topright", inset=c(-0.4,0), legend=c("inspections", "houses", "inspections", "houses"),
       col=c("dark blue", "blue", "dark green", "green"),pch = 16)










par(mar=c(5.1, 4.1, 4.1, 7), xpd=TRUE)

plot(stats_CURRENT_ENERGY_EFFICIENCY$year,stats_CURRENT_ENERGY_EFFICIENCY$median, main = "Energy efficiency over time", 
     xlab = "Year", ylab = "Median energy efficiency", xlim=c(2008, 2019), ylim=c(50, 80), pch = 16,col = "blue")
points(stats_by_year$inspection_year,stats_by_year$median_energy_efficiency, pch = 16, col = "red")

legend("topright", inset=c(-0.4,0), legend=c("cumulative", "year"),
       col=c("blue", "red"),pch = 16)





par(mar=c(5.1, 4.1, 4.1, 7), xpd=TRUE)

plot(stats_CO2_EMISSIONS_CURRENT$year,stats_CO2_EMISSIONS_CURRENT$median, main = "CO2 emissions over time", 
     xlab = "Year", ylab = "Median CO2 emissions", xlim=c(2008, 2019), ylim=c(2, 5), pch = 16,col = "blue")
points(stats_by_year$inspection_year,stats_by_year$median_CO2_emissions, pch = 16, col = "red")

legend("topright", inset=c(-0.4,0), legend=c("cumulative", "year"),
       col=c("blue", "red"),pch = 16)







# nr of houses with EPC vs nr of houses
# getting the nr of houses from https://data.cdrc.ac.uk/dataset/cdrc-median-house-prices-geodata-pack-1995-2015-leeds-e08000035
# 2015 data, Leeds only
nr_houses_LSOA <- read.csv("data/CDRC_2015_Council_Tax_Bands_Geodata_E08000035.csv", header = TRUE, sep = ",")
nr_houses_LSOA <- select(nr_houses_LSOA,LSOA11CD,All_Bands)
nr_houses_LSOA <- rename(nr_houses_LSOA, LSOA = LSOA11CD)
nr_houses_LSOA <- rename(nr_houses_LSOA, nr_houses_total = All_Bands)
#total_nr_houses_Leeds <- summarise(nr_houses_LSOA,nr_houses = sum(nr_houses_total))
#343,880 houses in 2015
# cumulative nr of houses as guessed from unique building reference nr
stats_cum_nr_EPCs$nr_houses

# property type from http://ubdc.gla.ac.uk/dataset/property-type-lsoa
# for 2014, entire country
types_houses_Leeds <- read.csv("data/property-type-2014-lsoa.csv", header = TRUE, sep = ",")
types_houses_Leeds$total_nr_houses <- rowSums(types_houses_Leeds[,4:31], na.rm=TRUE)
types_houses_LSOA <- types_houses_Leeds[types_houses_Leeds$GEOG == "LSOA",]


# total_nr_houses_Leeds2 <- summarise(types_houses_LSOA,nr_houses = sum(total_nr_houses))
# 341900


# total_nr_houses_Leeds3 <- types_houses_Leeds[grep("LAU", types_houses_Leeds$GEOG), c("total_nr_houses")]
# 342080

# fewer than 800 000 houses built in England since start 2015

# https://www.gov.uk/government/statistical-data-sets/live-tables-on-house-building
# for Leeds : nr dwellings completed
# 1,840 in 2018-2019
# 1,590 in 2017-2018
# 1,700 in 2016-2017
# 1,710 in 2015-2016
# 1,510 in 2014-2015
# 1,680 in 2013-2014
# 1,120 in 2012-2013
# 1,380 in 2011-2012
# 1,410 in 2010-2011
# 1,400 in 2009-2010
# 2,910 in 2008-2009

# so 8,350 new houses 2014-2019
# or 6,840 new houses 2015-2019

# assuming around 343,880 houses in 2015 (not sure when) and 6,840 new houses 2015-2019 
# around 350,700 houses

# in 2019 we have EPC on 211,416 houses out of 350,700 houses, which is about 60%
# in 2008 we have EPC on 12,142 houses out of 343,880-11,410 = 332,470 houses, which is about 4%

# SEE EPC ENGLAND AND WALES - more data/houses?!?


write.csv(unique_houses, file = "cumEPC2019.csv",row.names=FALSE)








# for 2019 cumulative data: predict real and potential energy efficiency

# remove NA
complete_data_2019 <- select(unique_houses_2019, CURRENT_ENERGY_EFFICIENCY, POTENTIAL_ENERGY_EFFICIENCY, ENERGY_CONSUMPTION_CURRENT, LIGHTING_COST_CURRENT)
complete_data_2019 <- na.omit(complete_data_2019) 

lm_E_eff <- lm(CURRENT_ENERGY_EFFICIENCY ~ POTENTIAL_ENERGY_EFFICIENCY + ENERGY_CONSUMPTION_CURRENT, data = complete_data_2019)
summary(lm_E_eff) 

PROPERTY_TYPE
BUILT_FORM
ENERGY_CONSUMPTION_CURRENT
LIGHTING_COST_CURRENT
HEATING_COST_CURRENT
HOT_WATER_COST_CURRENT
TOTAL_FLOOR_AREA
NUMBER_HABITABLE_ROOMS
NUMBER_HEATED_ROOMS



