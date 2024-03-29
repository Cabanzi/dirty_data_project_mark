library(tidyverse)
library(janitor)
library(readxl)
library(lubridate)
library(here)

candy_2015 <- read_excel(here("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx"))


#------------------------ Creating a "year" column -----------------------------
candy_2015 <- candy_2015 %>%
  mutate(Timestamp = as.POSIXct(Timestamp, format = "%Y-%m-%d %H:%M:%S"))

# Extract only the year from "Timestamp" column/ create "year" column 
candy_2015 <- candy_2015 %>%
  mutate(year = year(Timestamp)) %>%
  select(-Timestamp)  # Remove the original "Timestamp" column

# Move the "year" column before the "How old are you?" column
candy_2015 <- candy_2015 %>%
  relocate(year, .before = "How old are you?")


#------------------------ Creating an "age" column -----------------------------

candy_2015 <- candy_2015 %>%
  rename("age" = "How old are you?")


candy_2015 <- candy_2015 %>%
  mutate(`age` = ifelse(grepl("^\\d+\\.\\d+$", `age`), as.numeric(`age`), NA))


candy_2015 <- candy_2015 %>%
  mutate(age = ifelse(age > 116, NA, round(age, 0)))


#------------- Cleaning columns/ removing anything that's `NOT` candy ----------
candy_2015 <- candy_2015 %>% 
  rename(
    "trick or treating?" = 
     "Are you going actually going trick or treating yourself?"
    )


candy_2015 <- candy_2015 %>%
  rename_all(~str_remove_all(., "\\[|\\]"))


candy_2015 <- candy_2015 %>%
  select(
    -c("Anonymous brown globs that come in black and orange wrappers",
        "Any full-sized candy bar",
        "Brach products (not including candy corn)",
        "Vials of pure high fructose corn syrup, for main-lining into your vein",
        "Candy that is clearly just the stuff given out for free at restaurants",
        "Cash, or other forms of legal tender",
        "Dental paraphenalia",
        "Generic Brand Acetaminophen",
        "Glow sticks",
        "Broken glow stick",
        "Creepy Religious comics/Chick Tracts",
        "Hugs (actual physical hugs)",
        "Kale smoothie",
        "Lapel Pins",
        "Those odd marshmallow circus peanut things",
        "Peterson Brand Sidewalk Chalk",
        "Vicodin",
        "White Bread",
        "Whole Wheat anything",
        "Please leave any remarks or comments regarding your choices.",
        "Please list any items not included above that give you JOY.",
        "Please list any items not included above that give you DESPAIR.",
        "Guess the number of mints in my hand.",
        "Betty or Veronica?",
        "Check all that apply: \"I cried tears of sadness at the end of  ____________\"",
        "\"That dress* that went viral early this year - when I first saw it, it was ________\"",
        "Fill in the blank: \"Taylor Swift is a force for ___________\"",
        "What is your favourite font?",
        "If you squint really hard, the words \"Intelligent Design\" would look like.",
        "Fill in the blank: \"Imitation is a form of ____________\"",
        "Please estimate the degree(s) of separation you have from the following celebrities JK Rowling",
        "Please estimate the degree(s) of separation you have from the following celebrities JJ Abrams",
        "Please estimate the degree(s) of separation you have from the following celebrities Beyoncé",
        "Please estimate the degree(s) of separation you have from the following celebrities Bieber",
        "Please estimate the degree(s) of separation you have from the following celebrities Kevin Bacon",
        "Please estimate the degree(s) of separation you have from the following celebrities Francis Bacon (1561 - 1626)",
        "Please estimate the degrees of separation you have from the following folks Bruce Lee",     
        "Please estimate the degrees of separation you have from the following folks JK Rowling",      
        "Please estimate the degrees of separation you have from the following folks Malala Yousafzai", 
        "Please estimate the degrees of separation you have from the following folks Thom Yorke",      
        "Please estimate the degrees of separation you have from the following folks JJ Abrams",      
        "Please estimate the degrees of separation you have from the following folks Hillary Clinton", 
        "Please estimate the degrees of separation you have from the following folks Donald Trump",   
        "Please estimate the degrees of separation you have from the following folks Beyoncé Knowles",
        "Which day do you prefer, Friday or Sunday?",
        "JoyJoy (Mit Iodine)", 
        "Spotted Dick"
            ))


candy_2015 <- candy_2015 %>% 
  rename(
    "Fruit" = "Healthy Fruit", 
    "Gummy Bears" = "Gummy Bears straight up",
    "Chick-o-Sticks" = "Chick-o-Sticks (we don’t know what that is)",
    `Sea-salt Chocolate` = "Sea-salt flavored stuff, probably chocolate, since this is the \"it\" flavor of the year"
  )


candy_2015 <- janitor::clean_names(candy_2015)


write.csv(candy_2015, "clean_data/candy_2015.csv")