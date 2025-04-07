library(tidyverse)
library(gtsummary)

# All US ------------------------------------------------------------------

# monument lab- monuments set only---------------
mon <- read_csv(here::here('raw-data',
                           "monumentlab_national_monuments_audit_final_object_groups_monument.csv"))

mon <- mon %>% 
  janitor::clean_names() %>%
  mutate(name_lower = tolower(name))

# More complete monument lab data -------
all <- read_csv(here::here('raw-data',
                           "monumentlab_national_monuments_audit_final.csv"))

all <- all %>%
  janitor::clean_names()

dict <- names(all) %>% enframe() %>%
  select(-name)


all <- all %>%
  select(name, location_description, entities_people, entities_events, 
         everything())

# Clean Year Dedicated/Constructed 
all <- all %>%
  mutate(year_dedicated_or_constructed2 = case_when(
    !is.na(year_dedicated_or_constructed) ~ year_dedicated_or_constructed, 
    !is.na(year_constructed) ~ year_constructed, 
    TRUE~ year_dedicated) )%>%
  mutate(year_dedicated_or_constructed3 = 
           as.Date(as.character(year_dedicated_or_constructed2), format = "%Y")) %>%
  mutate(year_dedicated_or_constructed3 = year(year_dedicated_or_constructed3))

# Clean NYC Location 
all <- all %>%
  mutate(city = case_when(
    (city == "New York City" | city == "New York" | 
       city == "Brooklyn" | city == "Bronx" | 
       city == "Queens" | city == "Staten Island" | 
       city == "Manhattan") ~ "New York City", 
    TRUE ~ city))


# Subset only Monument type
all_mon <- all %>%
  filter(str_detect(object_groups, "Monument"))

# Filter out oldest ones
all_mon <- all_mon %>%
  filter(year_dedicated_or_constructed2 > 1750) 


# Create New cleaned statue removal field
all_mon <- all_mon %>%
  mutate(removed = case_when(
    status %in% c("Confirmed Missing", "Live (Statue Removed; Pedestal Remains)", 
                  "Relocated", "Removed", "Reported Missing") ~ "Yes", 
    TRUE ~ "No"))


# Save Data ---------------------------------------------------------------


