library(tidyverse)
library(stringr)

setwd("\\\\ds.leeds.ac.uk/staff/staff19/mednne/R_scripts/EPC_data")

# read in data 2008
#EPC_2008 <- read.csv("data/EPC/certificates_2008.csv", header = TRUE, sep = ",")
# read in data 2019
EPC_2008_2019 <- read.csv("data/certificates_2008_2019.csv", header = TRUE, sep = ",")

# pick relevant columns
#EPC_2008_subset <- select(EPC_2008,LMK_KEY,ADDRESS1,ADDRESS2,ADDRESS3,
#                          POSTCODE,BUILDING_REFERENCE_NUMBER,CURRENT_ENERGY_RATING,
#                          POTENTIAL_ENERGY_RATING,CURRENT_ENERGY_EFFICIENCY,POTENTIAL_ENERGY_EFFICIENCY,
#                          PROPERTY_TYPE,BUILT_FORM,INSPECTION_DATE)
EPC_2008_2019 <- select(EPC_2008_2019,ADDRESS1 : INSPECTION_DATE)

#postcode to LSOA converter
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
#EPC_2008_2019 <- select (EPC_2008_2019,-c(inspection_year))

# summary values per LSOA - one table for each year
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



# full list of all LSOAs, NA removed, to start results table
LSOA_data <- EPC_2008_2019 %>% 
  select(LSOA) %>% 
  na.omit()  %>% 
  distinct(LSOA)

# join all tables together
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
  
#write to csv
write.csv(LSOA_data, file = "LSOA_EPC_per_year.csv",row.names=FALSE)


