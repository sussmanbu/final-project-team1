
library(tidyverse)
library(readr)
library(dplyr)
library(here)

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

#clean column data types and create date/time column
date_time<-separate(shooting_data,shooting_date, into = c('Date','Time'), sep = " ")

date_time$Time <- strptime(date_time$Time, format = "%H:%M:%S")
date_time$Date = as.Date(date_time$Date)
date_time$multi_victim <- ifelse(date_time$multi_victim == "t", 1, 0)


#merge demographic data 
demo<-read.csv(here::here("dataset", "redistricting_data_tract20_nbhd_hhpopsize_ab-1.csv"))

district_names <-data.frame(district = c('A1','A15','A7','B2','B3','C6','C11','D4','D14','E5','E13','E18'),
                            district_name = c('Downtown & Charlestown','Downtown & Charlestown','East Boston', 'Roxbury',
                                              'Mattapan','South Boston','Dorchester','South End','Brighton','West Roxbury','Jamaica Plain','Hyde Park'))

colnames(demo) <- as.character(unlist(demo[1,]))
demo <- demo[-1,]
colnames(demo)[1] <- "district_name"

filtered_demo <- subset(demo, district_name %in% district_names$district_name)

cleaned_data<-merge(date_time,filtered_demo,by = 'district_name')
nrow(cleaned_data)
sum(is.na(cleaned_data))

#save to clean
shooting_data_clean <- cleaned_data

#view for debug
#View(shooting_data)

#SOME CAN HAVE SAME INCIDENT NUMBER IF MULTI VICTIM

write_csv(shooting_data_clean, file = here::here("dataset", "BostonShootingDataClean.csv"))

save(shooting_data_clean, file = here::here("dataset/BostonShootingDataClean.RData"))

