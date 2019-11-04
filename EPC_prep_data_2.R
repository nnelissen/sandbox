library(tidyverse)
library(stringr)
library(sf)

setwd("C:/Users/Toshiba/Desktop/test_R/EPC")

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
#write.csv(results_LSOA, file = "EPC_at_LSOA.csv",row.names=FALSE)






# OTHER DATA
#IMD data from gov.uk Index of Multiple Deprivation 2019
all_IMD <- read_sf(dsn = "C:/Users/Toshiba/Desktop/test_R", layer = "IMD_2019")
#so drop geometry from shape_IMD
st_geometry(all_IMD) <- NULL
#select rows for Leeds and columns LSOA code, name and IMD 
leeds_IMD <- all_IMD[grep("Leeds", all_IMD$lsoa11nm), c("lsoa11cd","IMDScore","TotPop")]
#rename columns
colnames(leeds_IMD)[colnames(leeds_IMD)=="lsoa11cd"] <- "LSOA"
colnames(leeds_IMD)[colnames(leeds_IMD)=="TotPop"] <- "total_population"
results_LSOA <- merge(results_LSOA,leeds_IMD,by="LSOA")


#fuel poverty data
all_fuelpoverty <- read.csv("C:/Users/Toshiba/Desktop/test_R/Fuel_poverty_LSOA_2017.csv", header = TRUE, sep = ",")
leeds_fuelpoverty <- all_fuelpoverty[grep("Leeds", all_fuelpoverty$LSOA.Name), c("LSOA.Code", "Estimated.number.of.households", "Proportion.of.households.fuel.poor....")]
colnames(leeds_fuelpoverty)[colnames(leeds_fuelpoverty)=="LSOA.Code"] <- "LSOA"
colnames(leeds_fuelpoverty)[colnames(leeds_fuelpoverty)=="Estimated.number.of.households"] <- "nr_households"
colnames(leeds_fuelpoverty)[colnames(leeds_fuelpoverty)=="Proportion.of.households.fuel.poor...."] <- "perc_fuelpoor_households"
results_LSOA <- merge(results_LSOA,leeds_fuelpoverty,by="LSOA")


#electricity consumption
all_electricityconsumption <- read.csv("C:/Users/Toshiba/Desktop/test_R/LSOA_domestic_electricity_2017.csv", header = TRUE, sep = ",")
colnames(all_electricityconsumption)[colnames(all_electricityconsumption)=="Lower.Layer.Super.Output.Area..LSOA..Code"] <- "LSOA"
colnames(all_electricityconsumption)[colnames(all_electricityconsumption)=="Total.domestic.electricity.consumption..kWh."] <- "total_electrconsump_kwh"
colnames(all_electricityconsumption)[colnames(all_electricityconsumption)=="Total.number.of.domestic.electricity.meters"] <- "nr_electr_meters"
colnames(all_electricityconsumption)[colnames(all_electricityconsumption)=="Mean.domestic.electricity.consumption...kWh.per.meter."] <- "mean_electrconsump_kwhpermeter"
colnames(all_electricityconsumption)[colnames(all_electricityconsumption)=="Median.domestic.electricity.consumption...kWh.per.meter."] <- "median_electrconsump_kwhpermeter"
leeds_electricityconsumption <- all_electricityconsumption[grep("Leeds", all_electricityconsumption$Lower.Layer.Super.Output.Area..LSOA..Name), c("LSOA", "total_electrconsump_kwh", "nr_electr_meters","mean_electrconsump_kwhpermeter","median_electrconsump_kwhpermeter")]
results_LSOA <- merge(results_LSOA,leeds_electricityconsumption,by="LSOA")


#gas consumption
all_gasconsumption <- read.csv("C:/Users/Toshiba/Desktop/test_R/LSOA_domestic_gas_2017.csv", header = TRUE, sep = ",")
colnames(all_gasconsumption)[colnames(all_gasconsumption)=="Lower.Layer.Super.Output.Area..LSOA..Code"] <- "LSOA"
colnames(all_gasconsumption)[colnames(all_gasconsumption)=="Consumption..kWh."] <- "total_gasconsump_kwh"
colnames(all_gasconsumption)[colnames(all_gasconsumption)=="Number.of.meters"] <- "nr_gas_meters"
colnames(all_gasconsumption)[colnames(all_gasconsumption)=="Mean.consumption..kWh.per.meter."] <- "mean_gasconsump_kwhpermeter"
colnames(all_gasconsumption)[colnames(all_gasconsumption)=="Median.consumption..kWh.per.meter."] <- "median_gasconsump_kwhpermeter"
leeds_gasconsumption <- all_gasconsumption[grep("Leeds", all_gasconsumption$LSOA.Name), c("LSOA", "total_gasconsump_kwh", "nr_gas_meters","mean_gasconsump_kwhpermeter","median_gasconsump_kwhpermeter")]
results_LSOA <- merge(results_LSOA,leeds_gasconsumption,by="LSOA")


#HPSSA Dataset 46: Median price paid for residential properties by LSOA, from ONS
median_house_prices <- read.csv("C:/Users/Toshiba/Desktop/test_R/hpssadataset46medianpricepaidforresidentialpropertiesbylsoa.csv", header = TRUE, sep = ",")
colnames(median_house_prices)[colnames(median_house_prices)=="LSOA.code"] <- "LSOA"
colnames(median_house_prices)[colnames(median_house_prices)=="Year.ending.Dec.2008"] <- "median_house_price_2008"
colnames(median_house_prices)[colnames(median_house_prices)=="Year.ending.Dec.2018"] <- "median_house_price_2018"
median_house_prices <- median_house_prices[grep("Leeds", median_house_prices$LSOA.name), c("LSOA", "median_house_price_2008", "median_house_price_2018")]
results_LSOA <- merge(results_LSOA,median_house_prices,by="LSOA")


# median_house_price_2008 and 2018 need : replaced by NA and converting to numeric
results_LSOA$median_house_price_2008 <- na_if(results_LSOA$median_house_price_2008, ":")
results_LSOA$median_house_price_2018 <- na_if(results_LSOA$median_house_price_2018, ":")
# factor to numeric
results_LSOA$median_house_price_2008 <- as.character(results_LSOA$median_house_price_2008)
results_LSOA$median_house_price_2008 <- gsub(",","",results_LSOA$median_house_price_2008)
results_LSOA$median_house_price_2008 <- as.numeric(results_LSOA$median_house_price_2008)
results_LSOA$median_house_price_2018 <- as.character(results_LSOA$median_house_price_2018)
results_LSOA$median_house_price_2018 <- gsub(",","",results_LSOA$median_house_price_2018)
results_LSOA$median_house_price_2018 <- as.numeric(results_LSOA$median_house_price_2018)
# create new columns for difference and percentage difference
results_LSOA <- mutate(results_LSOA, incr_medianhouseprice_08_18 = median_house_price_2018 - median_house_price_2008)
results_LSOA <- mutate(results_LSOA, percincr_medianhouseprice_08_18 = 100 * (median_house_price_2018 - median_house_price_2008) / median_house_price_2008)

# other data need converting to numeric too
results_LSOA$total_gasconsump_kwh <- as.numeric(gsub(",","",as.character(results_LSOA$total_gasconsump_kwh)))
results_LSOA$nr_gas_meters <- as.numeric(gsub(",","",as.character(results_LSOA$nr_gas_meters)))
results_LSOA$mean_gasconsump_kwhpermeter <- as.numeric(gsub(",","",as.character(results_LSOA$mean_gasconsump_kwhpermeter)))
results_LSOA$median_gasconsump_kwhpermeter <- as.numeric(gsub(",","",as.character(results_LSOA$median_gasconsump_kwhpermeter)))



write.csv(results_LSOA, file = "LSOA_data_4_corr.csv",row.names=FALSE)
results_LSOA <- read.csv("C:/Users/Toshiba/Desktop/test_R/EPC/LSOA_data_4_corr.csv", header = TRUE, sep = ",")


# correlations
library(GGally)
library(ggplot2)
library(Hmisc)          # produces correlation matrices with p-values
library(ppcor)          # assesses partial correlations



hist(results_LSOA$incr_medianhouseprice_08_18)
plot(results_LSOA$median_diff_E_efficiency,results_LSOA$incr_medianhouseprice_08_18)
abline(lm(results_LSOA$median_diff_E_efficiency ~ results_LSOA$incr_medianhouseprice_08_18))
cor(results_LSOA$median_diff_E_efficiency,results_LSOA$incr_medianhouseprice_08_18)

ggcorr(results_LSOA,
       label=TRUE,
       label_alpha = 0.2)

qplot(x = results_LSOA$incr_medianhouseprice_08_18,
      y = results_LSOA$median_diff_E_efficiency,
      data = results_LSOA,
      geom = c("point","smooth"),
               method = "lm",
               alpha =I(1/5),
               se = FALSE
      )

qplot(x = results_LSOA$incr_medianhouseprice_08_18, 
      y = results_LSOA$median_diff_E_efficiency, 
      data = results_LSOA) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Title")
  
ggpairs(results_LSOA,
        columns = c("incr_medianhouseprice_08_18","median_diff_E_efficiency","IMDScore"),
        upper = list(continuous = "smooth"),
        lower = list(continuous = "smooth")
        )


pairs(results_LSOA[, c(5:10)])

cor(golf$Age, golf$`Yards/Drive`, use = 'complete.obs')



p1 <- qplot(x = x1, y = y1, data = anscombe)
p2 <- qplot(x = x2, y = y2, data = anscombe)

rcorr(as.matrix(golf[, c(1, 3:9)]))

To assess the correlation between any two questions or create a correlation matrix across all questions we can use the cor(), cor.test(), and rcorr() (Hmisc package) functions and simply specify method = 'spearman':
  
p3 <- qplot(x = x3, y = y3, data = anscombe)
p4 <- qplot(x = x4, y = y4, data = anscombe)

grid.arrange(p1, p2, p3, p4, ncol = 2, 
             top = textGrob("Anscombe's Quartet"))
