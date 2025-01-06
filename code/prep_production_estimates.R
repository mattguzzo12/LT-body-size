############
#
# IGR Production Estimates for Guzzo Rennie paper
# ELA Lake 373 LT
# HMacLeod Nov 16, 2024
#
############

library(tidyverse)
library(readr)

#############IGR Production Code ######################

abundance.dat <- read_csv("data/373_fall_markrecap_rennie.csv") %>%
  rename(Year = year)

FishData <- read_csv("data/373_LT_DB_2016.csv") %>%
  rename(Year = YEAR)

###Determine the total count of fish per year to join with the abundance dataframe

TotYrCount <- FishData %>% # count of age classes for each year
  group_by(Year) %>% 
  summarise(totalN_perYr = n()) 

abun.dat2 <- left_join(abundance.dat, TotYrCount, by="Year") %>%
  rename(Abundance = n, 
         varAbundance = se)

##Combine abundance and fish data

LTData <- FishData %>%
  group_by(Year, AGE) %>%
  filter(!is.na(AGE)) %>%
  left_join(abun.dat2, by = "Year")

#Group all individuals less than 21 years of age and calculate mean weight
Dat_most <- LTData %>% 
  filter(AGE < 21) %>%  #all fish less than 21 years old because you want to include the 20 year old fish too
  group_by(Year, AGE) %>% 
  summarise(meanWeight = mean(WEIGHT), 
            varMeanWeight = var(WEIGHT),
            totalN_perAge = n())

#Calculate mean weight for individuals in time t greater than age 20 because there are few individuals in these age groups
Dat_20plus <- LTData %>% 
  filter(AGE > 20) %>%   #all fish greater than 12 years old
  group_by(Year) %>% 
  summarise(meanWeight = mean(WEIGHT), 
            varMeanWeight = var(WEIGHT),
            totalN_perAge = n()) %>% 
  mutate(AGE = 20) %>% 
  select(Year,AGE,meanWeight,totalN_perAge,varMeanWeight) #this just changes the order of columns so that the row_bind will work

Dat_20plus$AGE[Dat_20plus$AGE == 20] <- 22   #Change the age ID for grouped fish > 20   

#Calculate mean weight for individuals in time t+1 greater than age 21
Dat_21plus <- LTData %>% 
  filter(AGE > 21) %>%   #all fish greater than 21 years old
  group_by(Year) %>% 
  summarise(meanWeight = mean(WEIGHT), 
            varMeanWeight = var(WEIGHT),
            totalN_perAge = n()) %>% 
  mutate(AGE = 21) %>% #giving bogus number to 21+ of '21' (keeps cells as.numeric to facilitate other computations..can rename later)
  select(Year,AGE,meanWeight,totalN_perAge, varMeanWeight) #this just changes the order of columns so that the row_bind will work

Dat_21plus$AGE[Dat_21plus$AGE == 21] <- 23  #Change the age ID for grouped fish > 21

#Now merge all dataframes together and do final mutate calculations

datFinal <- bind_rows(Dat_most,Dat_20plus, Dat_21plus) %>% #join all dataframes together
  arrange(Year,AGE) %>% #sort data in this order so that age's will line back up
  group_by(Year) %>% 
  complete(AGE = 1:23) %>%
  left_join(abun.dat2, by="Year") %>% ##Add abundance est and variance to datFinal  
  mutate(proportion = totalN_perAge/totalN_perYr, # proportion of fish/age group/sampling year
         varProportion = proportion*(1-proportion), #variance of proportion
         density = proportion*Abundance, # density of the age class calculated using abundance est. 
         varDensity = (Abundance^2*varProportion)+(proportion^2*varAbundance)-(proportion^2*varAbundance), #variation of density estimate
         biomass = density*meanWeight) %>% #biomass calc obv
  filter(!is.na(Abundance)) #remove data without abundance estimates

datFinal[is.na(datFinal)] <- 0 #gives age classes with 1 observation NA variance values 0 for calculations

##Let's lineup the dataframes

## Make two seperate dataframes to correspond with t adn t+1
T1 <- datFinal %>% ## Set up dataframe for sampling period 1 or t
  filter(AGE != 23, 
         AGE != 20, ## remove age class 20, age class 20 is to only be used in t+1
         AGE != 21, 
         Year !=2016) %>% 
  select(-c(8:10)) #remove all columns unecessary for remaining calculations

T2 <- datFinal %>% ## Set up dataframe for sampling period t+1
  filter(AGE != 22, #remove 20+ group
         AGE != 1, #for calculation purposes age 1's cannot be used in T2
         AGE != 21,
         Year !=1986) %>% 
  select(-c(8:10)) 

##Bring t and t+1 dataframes together to perform remaining calculations
T3 <- bind_cols(T1, T2) %>%
  mutate(varBiomass = (biomass...13 + biomass...26)/4, 
         Growth = log(meanWeight...16)-log(meanWeight...3), 
         varGrowth = (varMeanWeight...4/meanWeight...3^2) + (varMeanWeight...17/meanWeight...16^2), 
         production = ((Growth*((biomass...26 + biomass...13)/2))/27.3)/1000, #divided by the hectare area of the lake (23.2) and by 1000 to get the kg/ha
         varProd = (((varBiomass * (log(meanWeight...16) - log(meanWeight...3)^2) + (((biomass...26+biomass...13)/2)^2) * varGrowth)/27.3^2)/1000^2)) #divided by the hectare area of the lake (23.2) and by 1000 to get the kg/ha

T3[T3 == -Inf] <- 0 ##turn all +/-Inf values to 0 so you can sum estimates
T3[T3 == Inf] <- 0 
T3[is.na(T3)] <- 0 #change NaNs to 0 to get estimates. 

##Remove negative estimates

T3zero <- T3
T3zero$production[T3zero$production<0] <- 0 ##This code will turn all negative values or values less than 0 to 0 in the table so you can then sum cohort production. 

ProductionEsts <- T3zero %>% 
  group_by(Year...1) %>%
  summarize(ProdEst = sum(production, na.rm = TRUE), 
            varProdEst = sum(varProd, na.rm = TRUE)) %>%
  mutate(SDProd = sqrt(varProdEst), 
         Method = "StandardIGR") %>%
  rename(Year = Year...1) %>%
  mutate(Year = Year +1) ##Shift year to ensure production corresponds to the correct growth period for Lake Trout 

#write.csv(ProductionEsts, file = "data/L373LakeTroutIGRProductionEsts.csv", row.names = F)
