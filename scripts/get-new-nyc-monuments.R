library(tidyverse)

new_nyc <- read_csv(here::here("raw-data", "NYC_Parks_Monuments_20231210.csv"))

new_nyc <- new_nyc %>%
  mutate(name_lower = tolower(name)) %>%
  mutate(dedicated_clean = parsedate::parse_date(dedicated)) %>%
  mutate(dedicated_clean2 = str_extract(dedicated, "\\d{4}"))



check <- new_nyc %>%
  filter(is.na(dedicated_clean2) & ! is.na(dedicated_clean))

new_nyc <- new_nyc %>% 
  mutate(dedicated_clean2 = case_when(
    is.na(dedicated_clean2) & ! is.na(dedicated_clean) ~ as.character(year(dedicated_clean)), 
    TRUE ~ dedicated_clean2
  ))


new_after_15 <- new_nyc %>%
  filter(dedicated_clean2 > 2015)


roos <- new_nyc %>% filter(str_detect(name_lower, "roosevelt"))

sims <- new_nyc %>% filter(str_detect(name_lower, "sims"))
new_nyc %>% select(extant) %>% 
  tbl_summary()


new_nyc %>%
  filter()
