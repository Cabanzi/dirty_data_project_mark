library(janitor)
library(tidyverse)
library(readr)
library(tibble)



decathlon <- readRDS("raw_data/decathlon.rds") %>% 
  janitor::clean_names()

decathlon <- rownames_to_column(decathlon, var = "athlete_name")




view(decathlon)
glimpse(decathlon)

decathlon <- decathlon %>% 
  mutate(athlete_name = str_to_lower(athlete_name))



write_csv(decathlon, "clean_data/decathlon.csv")
