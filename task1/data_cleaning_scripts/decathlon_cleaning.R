library(janitor)
library(tidyverse)
library(readr)
library(tibble)



decathlon <- readRDS("raw_data/decathlon.rds") %>% 
  janitor::clean_names()

## Convert athlete names row names into " athlete_name" column 
decathlon <- rownames_to_column(decathlon, var = "athlete_name")




#Further cleaning to names 

decathlon <- decathlon %>% 
  mutate(athlete_name = str_to_lower(athlete_name))



write_csv(decathlon, "clean_data/decathlon.csv")
