# Clear the workspace
rm(list = ls())

# Load necessary libraries
library(tidyverse) # for data manipulation
library(knitr)

# Load data from previous analysis
load("./DATA/level_15.Rdata")

# Data for Food, Consumables, and Durable good expenses
load("./DATA/level_14.Rdata")

# Subset data for level 14
L14subset <- level_14 %>% select(HH_ID, Questionnaire.No., Item.Code, Value..in.Rs., Weights, StateName, FSU.Serial.No., Sector)


# Separate data into categories: Food, Consumables, Durables 
# Note: Each of the dataframes below represent Sections A1, B1, and C1 in the Questionnaire respectively
FoodSummary <- L14subset %>% filter(Questionnaire.No. == "F")
ConsumablesSummary <- L14subset %>% filter(Questionnaire.No. == "C")
DurablesSummary <- L14subset %>% filter(Questionnaire.No. == "D")


##########

# Calculate expenses for different categories
# Expenses of certain items are provided for a 7 day period, others for a 30 day period, and some others for a 365 day period
# We need to normalise all of this to expenses for a 30 day period

# Food expenses

# Items with 30 day period 
# Item codes can be deduced from Section A1 in the questionnaire
Food1 <- FoodSummary %>% 
  filter(Item.Code %in% c(129, 139, 159, 179)) %>% 
  group_by(HH_ID) %>% 
  summarise(
    Sum1 = sum(Value..in.Rs.)
  )

# Items with 7 day period
# Item codes can be deduced from Section A1 in the questionnaire
# Convert from 7 day to 30 day, by multiplying the sum with 30/7
Food2 <- FoodSummary %>% 
  filter(Item.Code %in% c(169, 219, 239, 249, 199, 189, 269, 279, 289, 299)) %>% 
  group_by(HH_ID) %>% 
  summarise(
    Sum2 = sum(Value..in.Rs.)*(30/7) 
  )

# Merge all together, Replace NA with 0 and calculate total Food expenses in a 30 day period
Food <- full_join(Food1, Food2)
Food[is.na(Food)] <- 0
Food <- Food %>% mutate(FoodExpense = Sum1 + Sum2) %>% select(HH_ID, FoodExpense)

##########

# Consumables Expenses

# Items with 30 day period 
# Item codes can be deduced from Section B1 in the questionnaire (NOTE: make sure to not take 539, as it is only required in calculations that take into account imputed values)
Consumables1 <- ConsumablesSummary %>% 
  filter(Item.Code %in% c(349, 459, 479, 429, 519, 499, 439, 529)) %>% 
  group_by(HH_ID) %>% 
  summarise(
    Sum1 = sum(Value..in.Rs.) 
  )

# Items with 7 day period
# Item codes can be deduced from Section B1 in the questionnaire
# Convert from 7 day to 30 day, by multiplying the sum with 30/7
Consumables2 <- ConsumablesSummary %>% 
  filter(Item.Code %in% c(309,319,329)) %>% 
  group_by(HH_ID) %>% 
  summarise(
    Sum2 = sum(Value..in.Rs.)*(30/7)
  )

# Items with 365 day period
# Item codes can be deduced from Section B1 in the questionnaire
# Convert from 365 day to 30 day, by multiplying the sum with 30/365
Consumables3 <- ConsumablesSummary %>% 
  filter(Item.Code %in% c(409,419,899)) %>% 
  group_by(HH_ID) %>% 
  summarise(
    Sum3 = sum(Value..in.Rs.)*(30/365)
  )

# Merge all together, Replace NA with 0 and calculate total Consumables expenses in a 30 day period
Consumables <- Consumables1 %>% 
  full_join(Consumables2) %>% 
  full_join(Consumables3)

Consumables[is.na(Consumables)] <- 0

Consumables <- Consumables %>% mutate(ConsumablesExpense = Sum1 + Sum2 + Sum3) %>% select(HH_ID, ConsumablesExpense)

##########

# Durables Expenses


# All Items here have a 365 day period
# Item codes can be deduced from Section C1 in the questionnaire
# Convert from 365 day to 30 day, by multiplying the sum with 30/365
Durables <- DurablesSummary %>% 
  group_by(HH_ID) %>% 
  summarise(
    DurablesExpense = sum(Value..in.Rs.)*(30/365)
  )

##########

# Merge all together, Replace NA with 0

AllExpenses <- Food %>% full_join(Consumables) %>% full_join(Durables)

AllExpenses[is.na(AllExpenses)] <- 0

##########

# Household members during the canvassing of each visit differ and are recorded seperately.
# This is available in Level 15
# Let us extract this information, along with some other additional information that can be useful

# Extract household size data for each category
FoodHH <- level_15 %>% 
  filter(Questionnaire.No. == "F") %>% 
  select(HH_ID, Household.size)

ConsumablesHH <- level_15 %>% 
  filter(Questionnaire.No. == "C") %>%
  select(HH_ID, Household.size)

DurablesHH <- level_15 %>% 
  filter(Questionnaire.No. == "D") %>% 
  select(HH_ID, Household.size)

# Extract additional information
Additional <- level_15 %>% 
  filter(Questionnaire.No. == "F") %>% 
  select(HH_ID, Weights, Sector, State, StateName, FSU.Serial.No.)

# Join household size data with additional information
AdditionalInfo <- Additional %>% full_join(FoodHH, by = "HH_ID") %>% rename(Household.size.F = Household.size)
AdditionalInfo <- AdditionalInfo %>% full_join(ConsumablesHH, by = "HH_ID") %>% rename(Household.size.C = Household.size)
AdditionalInfo <- AdditionalInfo %>% full_join(DurablesHH, by = "HH_ID") %>% rename(Household.size.D = Household.size)


##########

# Combine additional information with all expenses to get the final data frame required to calculate the results
Results <- full_join(AdditionalInfo, AllExpenses)

# check if any NA. Should be FALSE
any(is.na(Results)) 


# Calculate Total Expenditure (TE) and Monthly Per Capita Consumption Expenditure (MPCE) as per formula given in Report Section 2.2.1 (page 14 [32 off 288])
# Note that this MPCE is for the household, not the population
Results <- Results %>% 
  mutate(
    TE = FoodExpense + (ConsumablesExpense * (Household.size.F/Household.size.C)) + (DurablesExpense * (Household.size.F/Household.size.D)),
    MPCE = TE/Household.size.F
  )

MPCE <- Results 

# Save data in Rdata format
save(MPCE, file = "./DATA/MPCE.RData")

# Load the saved data for MPCE
load("./DATA/Results.RData")


# Household characterstics
load("./DATA/level_3.Rdata")

# Subset data for level 3 - household characterstics
L03subset <- level_3 %>% select(HH_ID, Weights, StateName, FSU.Serial.No., Sector,
                                Does.the.household.have.a.dwelling.unit.at.present.place.of.enumeration., # 1 - yes, 2 - no
                                Type.of.Dwelling.Unit, # 1 - owned, 2 - hired, 3 - others
                                Basic.building.Material.used.for.major.portion.of.the.wall.of.the.dwelling.Unit,
                                Basic.building.Material.used.for.construction.of.the.major.portion.of.the.outer.exposed.part.of.the.roof.of.the.dwelling.unit,
                                Basic.Building.Material.used.for.construction.of.the.major.portion.of.the.floor.of.the.dwelling.Unit.,
                                Primary.source.of.energy.of.the.household.for.cooking,
                                Primary.source.of.energy.of.the.household.for.Lighting,
                                Source.of.Drinking.Water..Last.365.days.,
                                Prevailing.rate.of.rent.in.the.locality.is.available..FOR.Rural.only.
                                )

Household <- L03subset
# Save data in Rdata format
save(Household, file = "./DATA/Household.RData")


# Consumer goods ownership
load("./DATA/level_11.Rdata")

# subset data for level 11
L11subset <- level_11 %>% select(HH_ID, Weights, StateName, FSU.Serial.No., Sector,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey..Television,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey..Radio,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey...Laptop.PC,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey..Mobile.handset,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey..Bicycle,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey..Motorcycle..scooter,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey..Motor.car.jeep.van,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey..Trucks,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey...Animal.cart,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey..Refrigerator,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey..Washing.machine,
                                 Whether.household.possessed.one.or.more.item.as.on.the.date.of.the.survey..Air.conditioner.air.cooler
                                 )


Consumable_ownership <- L11subset
# Save data in Rdata format
save(Consumable_ownership, file = "./DATA/Consumable_ownership.RData")


# Data for appliance and transport equipment purchases
load("./DATA/level_13.Rdata")

# subset data for level 13 
L13subset <- level_13 %>% select(HH_ID, StateName, FSU.Serial.No., Sector, Weights,
                                 Item.Code, First.hand.purchase.number, Whether.purchased.on.hire,
                                 First.hand.purchase.Value.Rs.., Cost.of.repair...maintenance..cost.of.raw.material.and.services.for.construction.and.repair.Rs..,
                                 X2nd.hand.purchase.Number, X2nd.hand.purchase.Value.Rs.., Total.expenditure.Rs..)

L13subset <- L13subset %>% filter(Item.Code %in% c("600", "601","602","603","604", "609",
                                                   "580","043","581","044","045","588","585",
                                                   "582","046","586","590","047","048","587","591","584","583","592","599"))

Transport_appliance_purchase <- L13subset
# Save data in Rdata format
save(Transport_appliance_purchase, file = "./DATA/Transport_appliance_purchase.RData")
