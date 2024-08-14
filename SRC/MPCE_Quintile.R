#clear global environment
rm(list = ls())

# Load necessary libraries
library(tidyverse) # for data manipulation

# load data for MPCE
load("./DATA/MPCE.RData")

# MPCE CALCULATION ONLY FOR RURAL/URBAN
MPCE <- MPCE %>%
  mutate(Sector = if_else(Sector == 2, "Urban", "Rural"))

# filter for Karnataka
MPCE_Karnataka <- MPCE %>% filter(StateName == "Karnataka")

# filter for rural
MPCE_Karnataka_Rural <- MPCE_Karnataka %>% filter(Sector == "Rural")

# ordering by mpce
MPCE_Karnataka_Rural <- MPCE_Karnataka_Rural[order(MPCE_Karnataka_Rural$MPCE), ]


# Step 1: Ensure the data is sorted by MPCE
MPCE_Karnataka_Rural1 <- MPCE_Karnataka_Rural %>%
  arrange(MPCE)

# Step 2: Calculate cumulative estimated population
MPCE_Karnataka_Rural1 <- MPCE_Karnataka_Rural1 %>%
  mutate(cumulative_population = cumsum(Household.size.F * Weights))


# Step 3: Determine total population and quintile breakpoints
total_population_rural <- sum(MPCE_Karnataka_Rural1$Household.size.F * MPCE_Karnataka_Rural1$Weights)
quintile_breaks_rural <- seq(0, total_population_rural, length.out = 6)  # 5 quintiles = 6 breakpoints

# Step 4: Assign quintiles based on cumulative population
MPCE_Karnataka_Rural1 <- MPCE_Karnataka_Rural1 %>%
  mutate(quintile = cut(cumulative_population, breaks = quintile_breaks_rural, labels = 1:5, include.lowest = TRUE))


# Step 5: Verify the population distribution across quintiles
# population_by_quintile <- MPCE_Karnataka_Rural1 %>%
#   group_by(quintile) %>%
#   summarise(total_population = sum(Household.size.F * Weights))
# 
# Print the results to verify
# print(population_by_quintile)

# summary stats by quintile
mpce_by_quintile_rural <- MPCE_Karnataka_Rural1 %>%
  group_by(quintile) %>%
  summarise(
    min_MPCE = quantile(MPCE, probs = 0, weights = Weights),  # Minimum MPCE using weighted quantile
    avg_MPCE = sum(MPCE * Weights * Household.size.F)/sum(Weights * Household.size.F),
    max_MPCE = quantile(MPCE, probs = 1, weights = Weights),    # Maximum MPCE using weighted quantile
    avg_HH_size = sum(Household.size.F * Weights) / sum(Weights),  # Calculate average household size in each percentile
    total_population = sum(Household.size.F * Weights),  # Total population in each percentile
    weighted_num_households = sum(Weights)
  )

# Print the results
print(mpce_by_quintile_rural)


# URBAN
# filter for urban
MPCE_Karnataka_Urban <- MPCE_Karnataka %>% filter(Sector == "Urban")

#ordering by mpce
MPCE_Karnataka_Urban <- MPCE_Karnataka_Urban[order(MPCE_Karnataka_Urban$MPCE), ]


# Step 1: Ensure the data is sorted by MPCE
MPCE_Karnataka_Urban1 <- MPCE_Karnataka_Urban %>%
  arrange(MPCE)

# Step 2: Calculate cumulative estimated population
MPCE_Karnataka_Urban1 <- MPCE_Karnataka_Urban1 %>%
  mutate(cumulative_population = cumsum(Household.size.F * Weights))

# Step 3: Determine total population and quintile breakpoints
total_population_urban <- sum(MPCE_Karnataka_Urban1$Household.size.F * MPCE_Karnataka_Urban1$Weights)
quintile_breaks_urban <- seq(0, total_population_urban, length.out = 6)  # 5 quintiles = 6 breakpoints

# Step 4: Assign quintiles based on cumulative population
MPCE_Karnataka_Urban1 <- MPCE_Karnataka_Urban1 %>%
  mutate(quintile = cut(cumulative_population, breaks = quintile_breaks_urban, labels = 1:5, include.lowest = TRUE))


# Step 5: Verify the population distribution across quintiles
# population_by_quintile <- MPCE_Karnataka_Urban1 %>%
#   group_by(quintile) %>%
#   summarise(total_population = sum(Household.size.F * Weights))
# 
# Print the results to verify
# print(population_by_quintile)

# summary stats by quintile
mpce_by_quintile_urban <- MPCE_Karnataka_Urban1 %>%
  group_by(quintile) %>%
  summarise(
    min_MPCE = quantile(MPCE, probs = 0, weights = Weights),  # Minimum MPCE using weighted quantile
    avg_MPCE = sum(MPCE * Weights * Household.size.F)/sum(Weights * Household.size.F),
    max_MPCE = quantile(MPCE, probs = 1, weights = Weights),    # Maximum MPCE using weighted quantile
    avg_HH_size = sum(Household.size.F * Weights) / sum(Weights),  # Calculate average household size in each percentile
    total_population = sum(Household.size.F * Weights),  # Total population in each percentile
    weighted_num_households = sum(Weights)
  )

# Print the results
print(mpce_by_quintile_urban)

# Average Urban MPCE
Urban <- MPCE_Karnataka_Urban %>%
  summarise(avg_MPCE = sum(MPCE * Weights * Household.size.F)/sum(Weights * Household.size.F))


Rural <- MPCE_Karnataka_Rural %>%
  summarise(avg_MPCE = sum(MPCE * Weights * Household.size.F)/sum(Weights * Household.size.F))
