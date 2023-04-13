library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(here)


candy_2016 <- read_excel(here("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx"))  


#------------------------ Creating a "year" column -----------------------------
candy_2016 <- candy_2016 %>%
  mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%S"))

# Extract only the year from "Timestamp" column/ create "year" column 
candy_2016 <- candy_2016 %>%
  mutate(year = year(Timestamp)) %>%
  select(-Timestamp)  # Remove the original "Timestamp" column

# Move the "year" column before the "How old are you?" column
candy_2016 <- candy_2016 %>%
  relocate(year, .before = "Are you going actually going trick or treating yourself?")


#------------------------ Creating an "age" column -----------------------------
candy_2016 <- candy_2016 %>%
  rename("age" = "How old are you?") %>%
  mutate(age = ifelse(grepl("^\\d+\\.\\d+$", age), as.numeric(age), NA)) %>%
  mutate(age = ifelse(age > 116, NA, round(age, 0))) 

#-------------------------------------------------------------------------------
  candy_2016 <- candy_2016 %>% 
  rename("country" = "Which country do you live in?",
         "trick or treating?" = "Are you going actually going trick or treating yourself?",
         "gender" = "Your gender:")

#----------------- Standardising "country" column -----------------------------
# Define a mapping of country names to their standardized form
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
                     "A tropical island south of the equator" = NA, # Remove non-country entries
                     "england" = "United Kingdom",
                     "uk" = "United Kingdom",
                     "Switzerland" = "Switzerland",
                     "Murica" = "United States of America",
                     "United Kingdom" = "United Kingdom",
                     "Neverland" = NA, # Remove non-country entries
                     "USA!" = "United States of America",
                     "this one" = NA, # Remove non-country entries
                     "USA (I think but it's an election year so who can really tell)" = "United States of America",
                     "Korea" = "South Korea",
                     "51.0" = NA, # Remove non-country entries
                     "Usa" = "United States of America",
                     "NA" = NA, # Remove non-country entries
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
                     "47.0" = NA, # Remove non-country entries
                     "Cascadia" = NA, # Remove non-country entries
                     "españa" = "Spain",
                     "u.s." = "United States of America",
                     "there isn't one for old men" = NA, # Remove non-country entries
                     "Panama" = "Panama",
                     "one of the best ones" = NA, # Remove non-country entries
                     "The Yoo Ess of Aaayyyyyy" = "United States of America",
                     "United Kindom" = "United Kingdom",
                     "France" = "France",
                     "Australia" = "Australia",
                     "hungary" = "Hungary",
                     "united states of america" = "United States of America",
                     "Austria" = "Austria",
                     "Somewhere" = NA, # Remove non-country entries
                     "New Zealand" = "New Zealand",
                     "54.0" = NA, # Remove non-country entries
                     "Germany"
)

# Update the "country" column in candy_2016 data frame based on the mapping
candy_2016 <- candy_2016 %>%
  mutate(country = country_mapping[country])


#------------- Cleaning columns/ removing anything that's `NOT` candy ----------
candy_2016 <- candy_2016 %>%
  rename_all(~str_remove_all(., "\\[|\\]"))


candy_2016 <- candy_2016 %>% 
  select( 
    -c( 
      "Which state, province, county do you live in?",
        "Any full-sized candy bar", 
        "Anonymous brown globs that come in black and orange wrappers",
        "Broken glow stick",
        "Candy that is clearly just the stuff given out for free at restaurants",
        "Chardonnay",
        "Creepy Religious comics/Chick Tracts",
        "Glow sticks",
        "Kale smoothie",
        "Pencils",
        "Cash, or other forms of legal tender",
        "Generic Brand Acetaminophen",
        "Glow sticks",
        "Broken glow stick",
        "Hugs (actual physical hugs)",
        "Kale smoothie",
        "Creepy Religious comics/Chick Tracts",
        "Please list any items not included above that give you JOY.",
        "Please list any items not included above that give you DESPAIR.",
        "Guess the number of mints in my hand.",
        "Betty or Veronica?",
        "Please estimate the degree(s) of separation you have from the following celebrities JK Rowling",
        "Please estimate the degree(s) of separation you have from the following celebrities JJ Abrams",
        "Please estimate the degree(s) of separation you have from the following celebrities Beyoncé",
        "Please estimate the degree(s) of separation you have from the following celebrities Bieber",
        "Please estimate the degree(s) of separation you have from the following celebrities Kevin Bacon",
        "Please estimate the degree(s) of separation you have from the following celebrities Francis Bacon (1561 - 1626)",
        "Which day do you prefer, Friday or Sunday?",
        "Do you eat apples the correct way, East to West (side to side) or do you eat them like a freak of nature, South to North (bottom to top)?",
        "When you see the above image of the 4 different websites, which one would you most likely check out (please be honest).",
        "York Peppermint Patties",
        "Person of Interest Season 3 DVD Box Set (not including Disc 4 with hilarious outtakes)",
        "Those odd marshmallow circus peanut things",
        "Vials of pure high fructose corn syrup, for main-lining into your vein",
        "Vicodin",
        "Whatchamacallit Bars",
        "White Bread" ,
        "Whole Wheat anything" ,                                                                   
        "Please leave any witty, snarky or thoughtful remarks or comments regarding your choices." ,
        "\"That dress* that went viral a few years back - when I first saw it, it was ________\"" ,
        "What is your favourite font?" ,                                             
        "York Peppermint Patties Ignore" ,
        "Bonkers (the board game)",
        "Dental paraphenalia", 
        "JoyJoy (Mit Iodine!)",
        "Spotted Dick" 
    ))


candy_2016 <- candy_2016 %>%
  rename( 
    "Bonkers" = "Bonkers (the candy)",
    "Chick-o-Sticks" = "Chick-o-Sticks (we don’t know what that is)",
    "Fruit" =  "Healthy Fruit", 
    "Licorice" = "Licorice (yes black)",
    "Sourpatch kids" = "So urpatch Kids (i.e. abominations of nature)",
    "Sweetums" = "Sweetums (a friend to diabetes)",
    "Tolberone" = "Tolberone something or other",
    "Gummy Bears" = "Gummy Bears straight up"
  )

candy_2016 <- candy_2016 %>% 
  select(year, age, gender, `trick or treating?`, country, everything())

candy_2016 <- janitor::clean_names(candy_2016)

write.csv(candy_2016, "clean_data/candy_2016.csv")