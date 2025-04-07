
all_mon %>% 
  select(city, status) %>% 
  tbl_summary(by = status,
              sort = everything() ~ "frequency") %>% add_overall() %>%
  bold_labels()



z <- all_mon_rem %>%
  group_by(city) %>%
  filter(!is.na(city)) %>%
  count() %>%
  filter(n > 30)

all_mon_rem %>%
  filter(city %in% z$city) %>%
  select(city, removed) %>%
  tbl_summary(by = removed, 
              sort = everything() ~ "frequency", 
              percent = "row") %>%
  add_overall()
  

# Columbus ----------------------------------------------------------------


col <- all_mon %>%
  filter((str_detect(source, "Whose Heritage"))) %>%
  group_by(year_dedicated_or_constructed3) %>%
  count()


col %>%
  ggplot(aes(x = as.factor(year_dedicated_or_constructed3), y = n)) + 
  geom_col(aes(fill = n)) + theme_bw()  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#  geom_vline(xintercept = 1892, color = "red")+ 
#  geom_vline(xintercept = 1992, color = "red")

ggsave(here::here("outputs", "nyc-mon-hist.png"), 
       height = 4, width = 6)


# All  --------------------------------------------------------------------

all_mon %>%
  group_by(year_dedicated_or_constructed2) %>%
  count() %>%
  ggplot(aes(x = year_dedicated_or_constructed2, y = n)) + 
  geom_col(aes(fill = n)) + theme_bw() + 
  geom_vline(xintercept = 1918, color = "red")+ 
  geom_vline(xintercept = 1945, color = "red")

ggsave(here::here("outputs", "all-mon-hist.png"), 
       height = 4, width = 6)



# Richmond  --------------------------------------------------------------------


rich_all_mon <- all_mon %>%
  filter(city == "Philadelphia")



rich_all_mon %>%
  group_by(year_dedicated_or_constructed3) %>%
  count() %>%
  ggplot(aes(x = year_dedicated_or_constructed3,
             y = as.factor(n))) + 
  geom_col(aes(fill = n)) + theme_bw() + 
  geom_vline(xintercept = 1918, color = "red")+ 
  geom_vline(xintercept = 1945, color = "red")

ggsave(here::here("outputs", "rich-mon-hist.png"), 
       height = 4, width = 6)



# Removed -----------------------------------------------------------------




removed <- nyc_all_mon %>%
  filter(str_detect(location_description, "removed"))

removed_status <- nyc_all_mon %>%
  filter(!is.na(status))

nyc$year_removed %>% table()
nyc_all$year_removed %>% table()
nyc_all$status %>% table()

removed <- nyc_all %>%
  filter(!is.na(status))

nyc %>% filter(!is.na(year_removed)) %>% View()

table(mon$year_removed)

nyc <- nyc %>%
  mutate(name_lower = tolower(name))

nyc_post_2013 <- nyc %>%
  filter(year_constructed > 2013) 



nyc_all_post_2015 <- nyc_all %>%
  filter(year_constructed > 2015) 



nyc_post_2015 <- nyc_all %>%
  filter(year_constructed > 2015) 




# Removed -----------------------------------------------------------------

rem <- mon %>%
  filter(!is.na(year_removed))

rem %>% select(city) %>% 
  tbl_summary(sort = everything() ~ "frequency")

# Misc --------------------------------------------------------------------


roos <- nyc %>%
  filter(str_detect(name_lower, "roosevelt"))

sims %>% 

# New Haven ---------------------------------------------------------------

new_haven <- mon %>% 
  filter(city == "New Haven")


