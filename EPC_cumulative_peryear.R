library(tidyverse)

setwd("\\\\ds.leeds.ac.uk/staff/staff19/mednne/R_scripts/EPC_data")

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



















# do some analyses

# median EPC per year across Leeds
median(unique_houses_2007$CURRENT_ENERGY_EFFICIENCY)
min(unique_houses_2007$CURRENT_ENERGY_EFFICIENCY)
max(unique_houses_2007$CURRENT_ENERGY_EFFICIENCY)

stats_CURRENT_ENERGY_EFFICIENCY <- data.frame(
  median = double(),
  minimum = double(),
  maximum = double()
  ) 

for (yearval in 2007:2019)
{
  tmp <- get (paste("unique_houses_", yearval, sep = ""))
  median(tmp$CURRENT_ENERGY_EFFICIENCY)
  stats_CURRENT_ENERGY_EFFICIENCY$median <- append(stats_CURRENT_ENERGY_EFFICIENCY, median(tmp$CURRENT_ENERGY_EFFICIENCY))
}