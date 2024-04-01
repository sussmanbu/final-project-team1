
library(tidyverse)
library(readr)
library(dplyr)

#notes:
#there are 1928 rows by default
#should we remove the unknown and ""s?

#had to move the file outside of "scripts" in order to get "here" to work
shooting_data <- read.csv(here::here("dataset", "BostonShootingData.csv"))

#view for debug
#View(shooting_data)

## CLEAN the data

#make sure to handle NAs or Unknown as well
unknown <- sapply(shooting_data, function(x) {
  sum(x == "Unknown" | x == "")
})

#show count debug
#print(unknown)

#reformat victim_ethnicity_NIBRS into v_hispanic_or_latinx
shooting_data <- shooting_data %>%
  mutate("v_hispanic_or_latinx" = ifelse(victim_ethnicity_NIBRS == "Hispanic or Latinx", TRUE, FALSE))

#remove victim_ethnicity_NIBRS
shooting_data <- shooting_data %>%
  select(-victim_ethnicity_NIBRS)

#reformat shooting_type_v2 into fatal
shooting_data <- shooting_data %>%
  mutate("fatal" = ifelse(shooting_type_v2 == "Fatal", TRUE, FALSE))

#remove shooting_type_v2
shooting_data <- shooting_data %>%
  select(-shooting_type_v2)

#list of districts
district_mapping <- c("A1"="Downtown", "A15"="Charlestown", "A7"="East Boston", "B2"="Roxbury", "B3"="Mattapan", "C6"="South Boston", "C11"="Dorchester", "D4"="South End", "D14"="Brighton", "E5"="West Roxbury", "E13"="Jamaica Plain", "E18"="Hyde Park")

#district mapping
shooting_data <- shooting_data %>%
  mutate("district_name" = case_when(district %in% names(district_mapping) ~ district_mapping[district]))

#save to clean
shooting_data_clean <- shooting_data

#view for debug
#View(shooting_data)

#SOME CAN HAVE SAME INCIDENT NUMBER IF MULTI VICTIM

write_csv(shooting_data_clean, file = here::here("dataset", "BostonShootingDataClean.csv"))

save(shooting_data_clean, file = here::here("dataset/BostonShootingDataClean.RData"))

