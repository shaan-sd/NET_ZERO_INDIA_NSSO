# Clear the workspace
rm(list = ls())

# Load necessary libraries
library(tidyverse) # for data manipulation
library(survey)
library(knitr)

# Load data from previous analysis
load("./DATA/level_14.Rdata")
load("./DATA/level_15.Rdata")

# Subset data for level 14
L14subset <- level_14 %>% select(HH_ID, Questionnaire.No., Item.Code, Value..in.Rs., Weights, StateName, FSU.Serial.No., Sector)

# subset for  Karnataka state
L14subset <- L14subset %>%
  filter(StateName == "Karnataka") 

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
  filter(StateName == "Karnataka") %>%
  select(HH_ID, Household.size)

ConsumablesHH <- level_15 %>% 
  filter(Questionnaire.No. == "C") %>%
  filter(StateName == "Karnataka") %>%
  select(HH_ID, Household.size)

DurablesHH <- level_15 %>% 
  filter(Questionnaire.No. == "D") %>% 
  filter(StateName == "Karnataka") %>%
  select(HH_ID, Household.size)

# Extract additional information
Additional <- level_15 %>% 
  filter(Questionnaire.No. == "F") %>% 
  select(HH_ID, Weights, Sector, State, StateName, FSU.Serial.No.) %>%
  filter(StateName == "Karnataka")

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


# Save the dataframe for future analysis
#write.csv(Results, file = "./DATA/KARNATAKA/Results_Kar_MPCE.csv", row.names = FALSE)


# READ SAVED DATA FOR KARNATAKA MPCE
Results <- read.csv("./DATA/KARNATAKA/Results_Kar_MPCE.csv")

# MPCE CALCULATION ONLY FOR RURAL/URBAN

Results <- Results %>%
  mutate(Sector = if_else(Sector == 2, "Urban", "Rural"))

## SAMPLE DISTRIBUTION
print(length(Results$HH_ID))

household_counts <- Results %>%
  group_by(Sector) %>%
  summarise(unique_household_count = n_distinct(HH_ID))

# Print the household counts
print(household_counts)


# MPCE only by rural / urban 
# Subset data for Rural areas
RuralResults <- Results %>% filter(Sector == "Rural")
UrbanResults <- Results %>% filter(Sector == "Urban")
Rural <- sum(RuralResults$TE * RuralResults$Weights)/sum(RuralResults$Household.size.F * RuralResults$Weights)
Urban <- sum(UrbanResults$TE * UrbanResults$Weights)/sum(UrbanResults$Household.size.F * UrbanResults$Weights)

# Create a survey design object
options(survey.lonely.psu= "adjust")

sdesg = svydesign(id= ~HH_ID,
                  strata = ~State,
                  weight= ~Weights, data= Results,
                  nest= TRUE)

# quintiles based on MPCE
quintiles <- svyby(
  ~MPCE,
  ~Sector,
  sdesg,
  svyquantile,
  quantiles = c(0.2, 0.4, 0.6, 0.8),
  keep.var = TRUE
)

# Extract quintile values for each sector and assign them back to the Results dataframe
Results$quintile <- NA

# Define the correct quantile columns
quantile_columns <- c("MPCE.0.2", "MPCE.0.4", "MPCE.0.6", "MPCE.0.8")

for (sector in unique(Results$Sector)) {
  sector_quintiles <- quintiles[quintiles$Sector == sector, quantile_columns]
  sector_quintiles <- as.numeric(unlist(sector_quintiles))
  
  Results$quintile[Results$Sector == sector] <- cut(Results$MPCE[Results$Sector == sector],
                                                    breaks = c(-Inf, sector_quintiles, Inf),
                                                    labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                                                    include.lowest = TRUE)
}

# Print the dataframe with quintiles
#print(Results)


## Summing up Total Expenditure
# Create a survey design object
options(survey.lonely.psu= "adjust")

sdesg1 = svydesign(id= ~HH_ID,
                   strata = ~State,
                   weight= ~Weights, data= Results,
                   nest= TRUE)

# Sum total of TE by quintiles by rural / urban
sum_TE <- svyby(
  ~TE,
  ~Sector + quintile,
  sdesg1,
  FUN = svytotal
)


# merge with original data
Kar_1 <- merge(Results,sum_TE[,c('Sector', 'quintile' ,'TE')],by=c('Sector', 'quintile'),all.x=TRUE)

Kar_2 <- Kar_1 %>%
  rename("TE" = "TE.x",
         "Total_TE" = "TE.y")

# create a survey design object
sdesg2 = svydesign(id= ~HH_ID,
                   strata = ~State,
                   weight= ~Weights, data= Kar_2,
                   nest= TRUE)


# Sum total of TE by quintiles by rural / urban
sum_Household_size.F <- svyby(
  ~Household.size.F,
  ~Sector + quintile,
  sdesg2,
  FUN = svytotal
)

# merge with original data
Kar_3 <- merge(Kar_2,sum_Household_size.F[,c('Sector','quintile', 'Household.size.F')],by=c('Sector', 'quintile'),all.x=TRUE)

Kar_3 <- Kar_3 %>%
  rename("Household.size.F" = "Household.size.F.x",
         "Total_Household.size.F" = "Household.size.F.y")

# ratio calculation for MPCE

# Create a survey design object
options(survey.lonely.psu= "adjust")

sdesg3 = svydesign(id= ~HH_ID,
                   strata = ~State,
                   weight= ~Weights, data= Kar_3,
                   nest= TRUE)

# MPCE by expenditure quintiles in Rural / Urban
Final_MPCE <- svyby(
  ~Total_TE,
  ~Sector + quintile,
  denominator = ~Total_Household.size.F,
  sdesg3,
  FUN = svyratio
)

Final_MPCE1 <- Final_MPCE %>%
  select(Sector, quintile, `Total_TE/Total_Household.size.F`) %>%
  rename("MPCE" = "Total_TE/Total_Household.size.F") %>%
  arrange(Sector, quintile) 

row.names(Final_MPCE1) <- NULL

#print(Final_MPCE1)

p <- ggplot(Final_MPCE1, aes(x = quintile, y = MPCE, fill = Sector)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "MPCE by Quintile for Rural and Urban Sectors",
       x = "Quintile",
       y = "MPCE (in Rs)") +
  theme_minimal() +
  scale_fill_manual(values = c("Rural" = "blue", "Urban" = "red")) +
  theme(legend.title = element_blank()) +
  geom_hline(aes(yintercept = Rural), color = "blue", linetype = "dashed", size = 1) +
  geom_hline(aes(yintercept = Urban), color = "red", linetype = "dashed", size = 1)

# Print the plot
print(p)

# merge with original data
Kar_4 <- merge(Kar_3, Final_MPCE[,c('Sector','quintile', 'Total_TE/Total_Household.size.F')],by=c('Sector', 'quintile'),all.x=TRUE)

Kar_4 <- Kar_4 %>%
  rename("Total_MPCE" = "Total_TE/Total_Household.size.F")

#write.csv(Kar_4, file = "./DATA/KARNATAKA/Karnataka_Quintile_MPCE.csv", row.names = FALSE)
#read.csv("./DATA/KARNATAKA/Karnataka_Quintile_MPCE.csv")

final_mpce1_rural <- Final_MPCE1[Final_MPCE1$Sector == "Rural", ]
final_mpce1_urban <- Final_MPCE1[Final_MPCE1$Sector == "Urban", ]

# Present in table format with rural and urban separated
kable(final_mpce1_rural, caption = "Rural Sector Quintiles")

kable(final_mpce1_urban, caption = "Urban Sector Quintiles", row.names = FALSE)


# MIN, MAX of MPCE and TOTAL HHs
Kar_q <- Kar_4 %>%
  select(Sector, quintile, MPCE, HH_ID) %>%
  group_by(Sector, quintile) %>%
  summarize(Min_MPCE = min(MPCE), 
            Max_MPCE = max(MPCE),
            Total_Household = n_distinct(HH_ID)
            )

#print(Kar_q)

Kar_q_rural <- Kar_q[Kar_q$Sector == "Rural", ]
Kar_q_urban <- Kar_q[Kar_q$Sector == "Urban", ]

# Present in table format with rural and urban separated
kable(Kar_q_rural, caption = "Rural Sector Quintiles")

kable(Kar_q_urban, caption = "Urban Sector Quintiles")




### AVERAGE HOUSEHOLD SIZE
options(survey.lonely.psu= "adjust")

sdesg_hh = svydesign(id= ~HH_ID,
                   strata = ~State,
                   weight= ~Weights, data= Results,
                   nest= TRUE)

# Avg HH size by rural / urban
avg_HH <- svyby(
  ~Household.size.F,
  ~Sector,
  sdesg_hh,
  FUN = svymean
)

kar_hh_avg <- avg_HH %>%
  select(Sector, Household.size.F)

kable(kar_hh_avg, caption = "Avg Household size in Rural/Urban")

# Avg HH size by quintiles in Rural/Urban
avg_HH_q <- svyby(
  ~Household.size.F,
  ~Sector + quintile,
  sdesg_hh,
  FUN = svymean
)

kar_hh_avg_q <- avg_HH_q %>%
  select(Sector, quintile, Household.size.F)

Kar_hh_avg_q_rural <- kar_hh_avg_q[kar_hh_avg_q$Sector == "Rural", ]
Kar_hh_avg_q_urban <- kar_hh_avg_q[kar_hh_avg_q$Sector == "Urban", ]

kable(Kar_hh_avg_q_rural, caption = "Avg Household size in Rural quintiles")
kable(Kar_hh_avg_q_urban, caption = "Avg Household size in Urban quintiles")


#WEIGHTED POPULATION 
Total_Population <- svyby(
  ~Household.size.F,
  ~Sector + quintile,
  sdesg_hh,
  FUN = svytotal
)

Total_Population <- as.data.frame(Total_Population) %>% arrange(Sector)
Total_Population <- Total_Population %>% select(-se)
row.names(Total_Population) <- NULL

# Add a row for the total value
total_row <- data.frame(
  Sector = "Total",
  Household.size.F = sum(Total_Population$Household.size.F)
)
pop <- rbind(Total_Population, total_row)
pop <- pop %>% rename("Weighted_population" = "Household.size.F")

# Print the table using kable
kable(pop)
