library(tidyverse); library(tidycensus); library(stringr)

all_fips <- tidycensus::fips_codes %>%
  mutate(locality = tolower(county)) %>% 
  as_tibble()

# We have a codebook for these -- will ask Delaney to add others
NPIs_to_keep <- c("SDO", "SD", paste0("GS_", c(10, 100, 1000, 25, 250, 50, 500)), "CPV", "CPV_50", "PC", "NESC", "LD")

intervention_data <- read_csv("npis_raw_03-24-2020.csv") %>%
  mutate(date_start = case_when(grepl("[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2}", data_start) ~ as.Date(data_start, format = "%d-%b-%y"),
                                grepl("[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", data_start) ~ as.Date(data_start, format = "%m/%d/%Y"), 
                                grepl("[0-9]{1,2}-[A-Za-z]{3}", data_start) ~ as.Date(paste0(data_start, "-2020"), format = "%d-%b-%Y"),
                                TRUE ~ as.Date("1901-01-01")),
         date_end_date = case_when(grepl("[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2}", date_end) ~ as.Date(date_end, format = "%d-%b-%y"),
                                   grepl("[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", date_end) ~ as.Date(date_end, format = "%m/%d/%Y"), 
                                   grepl("[0-9]{1,2}-[A-Za-z]{3}", date_end) ~ as.Date(paste0(date_end, "-2020"), format = "%d-%b-%Y"),
                                   TRUE ~ as.Date(NA_character_))
  ) %>% 
  filter(!date_start == as.Date("1901-01-01")) %>% 
  select(locality, type_of_intervention, state, date_start, date_end_date) %>% 
  rename(locality_original = locality) %>% 
  filter(!type_of_intervention %in% c("CI") & !duplicated(.)) %>% 
  arrange(state, locality_original, date_start) %>% 
  group_by(state, locality_original, type_of_intervention) %>% 
  mutate(intervention = ifelse((1:n())>1, paste0(type_of_intervention,"_", 1:n()), type_of_intervention)) %>%
  ungroup %>% 
  filter(type_of_intervention %in% NPIs_to_keep)
  
intervention_starts <- intervention_data %>% select(-date_end_date, -type_of_intervention) %>% 
  spread(intervention, date_start)

county_data <- read_csv("us-counties.csv") %>% 
  mutate(county_code = substr(fips, 3, 5)) %>% 
  rename(state_name = state) %>% 
  left_join(all_fips %>% select(state, state_name) %>% group_by(state) %>% summarise(state_name = first(state_name)) %>% ungroup) %>% 
  mutate(county_code = ifelse(county == "New York City", "-1", county_code))


# Counties_in_nyt_data <- county_data %>% 
#   group_by(fips) %>% 
#   select(county) %>% 
#   summarize(county = first(county)) %>%
#   ungroup %>%
#   mutate(locality = tolower(county))
  
localities <- intervention_starts %>% 
  mutate(locality = locality_original) %>%
  group_by(locality) %>% 
  summarize(state = first(state)) %>% 
  ungroup %>% 
  mutate(locality_original = locality, 
         locality = gsub("_", " ", locality)) %>%
  mutate(locality = case_when(locality=="bexar county, san antonio" ~ "bexar county",
                              locality=="busan" ~ NA_character_, 
                              locality=="miami dade county" ~ "miami-dade county",
                              locality == "london" ~ NA_character_,
                              locality == "las vegas county" & state == "NV"~ "clark county",
                              TRUE ~ locality)) %>% 
  left_join(all_fips %>% select(locality, county_code, state), by = c("locality","state") ) %>% 
  filter(state %in% all_fips$state) %>% 
  filter(locality != "nan") %>% 
  mutate(county_code = case_when(locality == "new york city" ~ "-1", 
                                 locality == "washington dc" ~ "001", 
                                 TRUE ~ county_code))


joined_data <- county_data %>% left_join(localities, by = c("county_code", "state")) %>% 
  filter(!is.na(locality)) %>% 
  left_join(intervention_starts, by = c("locality_original", "state")) 

panel_attempt <- joined_data %>% 
  gather(intervention, date_of_intervention, -date:-locality_original) %>% 
  arrange(state, locality_original, intervention, date) %>% 
  mutate(intervention_on = as.numeric(date >= date_of_intervention),
         intervention_on = if_else(is.na(intervention_on), 0, intervention_on)) %>% 
  select(-date_of_intervention) %>%
  spread(intervention, intervention_on)

write_csv(panel_attempt, "interventions_and_deaths.csv")
