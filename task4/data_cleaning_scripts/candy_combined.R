library(tidyverse)
library(here)

# Read in the CSV files
candy_2015 <- read.csv(here("clean_data/candy_2015.csv"))
candy_2016 <- read.csv(here("clean_data/candy_2016.csv"))
candy_2017 <- read.csv(here("clean_data/candy_2017.csv"))

# Add "country" and "gender" column to candy_2015 data frame and fill with NA values
candy_2015 <- candy_2015 %>%
  mutate(country = NA, gender = NA)

# Combine data frames vertically using bind_rows
candy_2015_to_2017 <- bind_rows(candy_2015, candy_2016, candy_2017)

candy_2015_to_2017 <- candy_2015_to_2017 %>% 
  select(year, age, gender, trick_or_treating, country, everything())

write.csv(candy_2015_to_2017, "clean_data/candy_2015_to_2017.csv")