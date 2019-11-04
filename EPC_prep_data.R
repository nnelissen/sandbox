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


#find duplicates of BUILDING_REFERENCE_NUMBER
# only entries with at least 2 EPCs
subset_duplicates <- EPC_2008_2019 %>% 
  group_by(BUILDING_REFERENCE_NUMBER) %>% 
  filter(n()>1)

# building ref nr for each - nr of unique buildings
#duplicates_refs <- EPC_2008_2019 %>% count(BUILDING_REFERENCE_NUMBER) %>% filter(n > 1)

# sort by date, earliest first
subset_duplicates <- subset_duplicates[order(as.Date(subset_duplicates$INSPECTION_DATE, format="%Y-%m-%d")),]


# number instances of same house as 1,2 etc
subset_duplicates$nr_inspections <- ave(subset_duplicates$BUILDING_REFERENCE_NUMBER, subset_duplicates$BUILDING_REFERENCE_NUMBER, FUN = seq_along)

#Compare 1 to whichever is last: did it go up/down or stay the same

#create new table
unique_houses <- filter(subset_duplicates,nr_inspections == 1)
unique_houses <- rename(unique_houses, start_EPC = CURRENT_ENERGY_RATING)
unique_houses <- rename(unique_houses, start_E_efficiency = CURRENT_ENERGY_EFFICIENCY)
unique_houses <- rename(unique_houses, start_inspect_date = INSPECTION_DATE)
unique_houses <- select (unique_houses,-c(POTENTIAL_ENERGY_RATING,POTENTIAL_ENERGY_EFFICIENCY,nr_inspections))

#find the highest nr_inspections per building
max_nr_inspections <- subset_duplicates %>% group_by(BUILDING_REFERENCE_NUMBER) %>% summarise(max_nr_inspections = max(nr_inspections))
subset_duplicates <- inner_join(subset_duplicates,max_nr_inspections)
rm(max_nr_inspections)

#create temporary dataset of last EPC data and prepare to merge with unique_houses
last_EPC_temp <- filter(subset_duplicates, nr_inspections == max_nr_inspections)
last_EPC_temp <- select (last_EPC_temp,-c(ADDRESS1,ADDRESS2,ADDRESS3,POSTCODE,PROPERTY_TYPE,BUILT_FORM,nr_inspections))
last_EPC_temp <- rename(last_EPC_temp, end_EPC = CURRENT_ENERGY_RATING)
last_EPC_temp <- rename(last_EPC_temp, end_E_efficiency = CURRENT_ENERGY_EFFICIENCY)
last_EPC_temp <- rename(last_EPC_temp, end_inspect_date = INSPECTION_DATE)

#merge last_EPC_temp with unique_houses
unique_houses <- left_join(unique_houses,last_EPC_temp)
rm(last_EPC_temp)

#check if NA present
#apply(unique_houses, 2, function(x) any(is.na(x)))
unique_houses <- unique_houses[c("BUILDING_REFERENCE_NUMBER",
                        "POSTCODE", "PROPERTY_TYPE", "BUILT_FORM", 
                        "start_EPC", "start_E_efficiency", "start_inspect_date", 
                        "end_EPC", "end_E_efficiency", "end_inspect_date",
                        "max_nr_inspections", "POTENTIAL_ENERGY_RATING", "POTENTIAL_ENERGY_EFFICIENCY"
                        )]

#calculate and plot the difference in E_efficiency start to finish
unique_houses <- unique_houses %>%
  mutate(diff_E_efficiency = end_E_efficiency - start_E_efficiency)
#hist(unique_houses$diff_E_efficiency)
unique_houses <- unique_houses %>%
  mutate(percdiff_E_efficiency = 100 * (end_E_efficiency - start_E_efficiency) / start_E_efficiency)

#postcode to LSOA converter
postcode_2_LSOA <- read.csv("data/PCD_OA_LSOA_MSOA_LAD_AUG19_UK_LU/PCD_OA_LSOA_MSOA_LAD_AUG19_UK_LU.csv", header = TRUE, sep = ",")
lut <- filter(postcode_2_LSOA,ladnm == "Leeds")
lut <- rename(lut, POSTCODE = pcds)
lut <- rename(lut, LSOA = lsoa11cd)
lut <- select(lut,c(POSTCODE,LSOA))
#write.csv(lut, file = "postcode2lsoa.csv",row.names=FALSE)

#some postcodes don't get converted
#filter(postcode_2_LSOA,pcds == "LS15 5NJ")
#LS15 5NJ doesn't exist, the others are not in Leeds
#1 more LSOA than in the shapefile?


#add column LSOA based on POSTCODE
#temp <- unique_houses  # create a copy of unique_houses
# using lapply, loop over columns and match values to the look up table. store in "new".
#temp[] <- lapply(unique_houses, function(x) lut$lsoa11cd[match(x, lut$POSTCODE)])
# change column name and append to unique_houses
#temp <- rename(temp, LSOA = POSTCODE)
#temp <- select(temp,c(LSOA))
unique_houses <- unique_houses %>% 
  left_join(lut, by = "POSTCODE")
rm(lut)

#check for NA - no match between postcode and LSOA
#sum(is.na(unique_houses$LSOA))
#write dataset
#write.csv(unique_houses, file = "Multiple_EPC_houses.csv",row.names=FALSE)

#EPC values per LSOA
results_LSOA <- unique_houses %>%
  group_by(LSOA) %>%  
  summarize(nr_houses = n(),
            median_start_E_efficiency = median(start_E_efficiency),
            median_end_E_efficiency = median(end_E_efficiency),
            median_diff_E_efficiency = median(diff_E_efficiency),
            median_percdiff_E_efficiency = median(percdiff_E_efficiency),
            nr_houses_neg_diff_E_eff = sum(diff_E_efficiency < 0),
            nr_houses_pos_diff_E_eff = sum(diff_E_efficiency > 0),
            nr_houses_neu_diff_E_eff = sum(diff_E_efficiency == 0)
            )
  
#% of pos, neg and neu 
results_LSOA <- mutate(results_LSOA, perc_houses_neg_diff_E_eff = 100 * nr_houses_neg_diff_E_eff / nr_houses)
results_LSOA <- mutate(results_LSOA, perc_houses_pos_diff_E_eff = 100 * nr_houses_pos_diff_E_eff / nr_houses)
results_LSOA <- mutate(results_LSOA, perc_houses_neu_diff_E_eff = 100 * nr_houses_neu_diff_E_eff / nr_houses)

#write to csv
write.csv(results_LSOA, file = "EPC_at_LSOA.csv",row.names=FALSE)





dataset %>% 
  filter(diff_E_efficiency < 0) %>% 
  summarize(median = median(diff_E_efficiency))
