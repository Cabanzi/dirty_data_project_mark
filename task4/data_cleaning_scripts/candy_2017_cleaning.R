library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(here)

candy_2017 <- read_excel(here("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx"))


candy_2017 <- candy_2017 %>%
  rename_all(~ gsub("^Q[1-6]:|^Q[1-6]\\s\\|", "", .))

#------------------------ Creating a "year" column -----------------------------
candy_2017 <- candy_2017 %>%
  mutate(year = as.double(2017))
#------------------------ Creating an "age" column -----------------------------
candy_2017 <- candy_2017 %>%
  rename("age" = ` AGE`) %>%
  mutate(age = ifelse(grepl("^\\d+$", age), as.numeric(age), NA)) %>%
  mutate(age = ifelse(age > 116, NA, round(age, 0))) 

#----------------- Standardising "country" column -----------------------------
# Define the mapping of country names to their standardized form
country_mapping <- c("Canada" = "Canada",
                     "usa" = "United States of America",
                     "US" = "United States of America",
                     "USA" = "United States of America",
                     "UK" = "United Kingdom",
                     "United States of America" = "United States of America",
                     "uSA" = "United States of America",
                     "Japan" = "Japan",
                     "united states" = "United States of America",
                     "canada" = "Canada",
                     "United States" = "United States of America",
                     "us" = "United States of America",
                     "france" = "France",
                     "USSA" = "United States of America",
                     "U.S.A." = "United States of America",
                     "A tropical island south of the equator" = NA,
                     "england" = "United Kingdom",
                     "uk" = "United Kingdom",
                     "Switzerland" = "Switzerland",
                     "Murica" = "United States of America",
                     "United Kingdom" = "United Kingdom",
                     "Neverland" = NA,
                     "USA!" = "United States of America",
                     "this one" = NA,
                     "USA (I think but it's an election year so who can really tell)" = "United States of America",
                     "Korea" = "South Korea",
                     "51.0" = NA,
                     "Usa" = "United States of America",
                     "NA" = NA,
                     "U.S." = "United States of America",
                     "Us" = "United States of America",
                     "America" = "United States of America",
                     "Units States" = "United States of America",
                     "belgium" = "Belgium",
                     "croatia" = "Croatia",
                     "United states" = "United States of America",
                     "Portugal" = "Portugal",
                     "England" = "United Kingdom",
                     "USA USA USA" = "United States of America",
                     "the best one - usa" = "United States of America",
                     "USA!" = "United States of America",
                     "47.0" = NA,
                     "Cascadia" = NA,
                     "españa" = "Spain",
                     "u.s." = "United States of America",
                     "there isn't one for old men" = NA,
                     "Panama" = "Panama",
                     "one of the best ones" = NA,
                     "The Yoo Ess of Aaayyyyyy" = "United States of America",
                     "United Kindom" = "United Kingdom",
                     "France" = "France",
                     "Australia" = "Australia",
                     "hungary" = "Hungary",
                     "united states of america" = "United States of America",
                     "Austria" = "Austria",
                     "Somewhere" = NA,
                     "New Zealand" = "New Zealand",
                     "54.0" = NA,
                     "Germany" = "Germany")

# Update the "country" column in candy_2016 data frame based on the mapping
candy_2017 <- candy_2017 %>%
  mutate(` COUNTRY` = country_mapping[` COUNTRY`])



#------------- Cleaning columns/ removing anything that's `NOT` candy ----------
candy_2017 <- candy_2017 %>%
  select(
    -c(
      "Internal ID",
      " STATE, PROVINCE, COUNTY, ETC",
      " Anonymous brown globs that come in black and orange wrappers\t(a.k.a. Mary Janes)",
      " Any full-sized candy bar",
      " Black Jacks",
      " Bonkers (the board game)",
      " Broken glow stick",
      " Creepy Religious comics/Chick Tracts",
      " Dental paraphenalia",
      " Generic Brand Acetaminophen",
      " Glow sticks",
      " Hugs (actual physical hugs)",
      " JoyJoy (Mit Iodine!)",
      " Kale smoothie",
      " Abstained from M&M'ing.",
      " Real Housewives of Orange County Season 9 Blue-Ray",
      " Sandwich-sized bags filled with BooBerry Crunch",
      " Spotted Dick",
      " Those odd marshmallow circus peanut things",
      " Vials of pure high fructose corn syrup, for main-lining into your vein",
      " Vicodin",
      " Whatchamacallit Bars",
      " White Bread",
      " Whole Wheat anything",
      " York Peppermint Patties",
      "Q7: JOY OTHER",
      "Q8: DESPAIR OTHER",
      "Q9: OTHER COMMENTS",
      "Q10: DRESS",
      "...114",
      "Q11: DAY",
      "Q12: MEDIA [Daily Dish]",
      "Q12: MEDIA [Science]",
      "Q12: MEDIA [ESPN]",
      "Q12: MEDIA [Yahoo]",
      "Click Coordinates (x, y)"
    ))


candy_2017 <- candy_2017 %>%
  rename( 
    "Bonkers" = " Bonkers (the candy)",
    "Chick-o-Sticks" = " Chick-o-Sticks (we don’t know what that is)",
    "Fruit" =  " Healthy Fruit", 
    "Licorice" = " Licorice (yes black)",
    "Sourpatch kids" = " Sourpatch Kids (i.e. abominations of nature)",
    "Sweetums" = " Sweetums (a friend to diabetes)",
    "Tolberone" = " Tolberone something or other", 
    "Gummy Bears" = " Gummy Bears straight up", 
    "trick or treating?" = " GOING OUT?",
    "country" = " COUNTRY",
    "gender" = " GENDER"
  )

candy_2017 <- candy_2017 %>% 
  select(year, age, gender, `trick or treating?`, country, everything()) 
  

candy_2017 <- janitor::clean_names(candy_2017)


write.csv(candy_2017, "clean_data/candy_2017.csv")