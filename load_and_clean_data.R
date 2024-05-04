
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

#Add census data 
library(tidyverse)
library(readr)

shooting_data <- read.csv(here::here("dataset", "BostonShootingDataClean.csv"))
shooting_data <- mutate(shooting_data, year = year(Date))

census_api_key('')

#filter for variables 
vars<-load_variables(2022, "acs1", cache = TRUE)
search<-vars %>%
  filter(str_detect(label,'age'))


#Loop over years
fetch_acs_data <- function(year, zips) {
  dat <- get_acs(geography = 'zcta', 
                 variables = c(total_house_units = 'B25001_001',
                               non_married_household = 'B11001A_004',
                               bachelors_25 = 'B15003_022',
                               one_parent = 'B23008_021',
                               married_households = 'B11001A_003',
                               income_deficit = 'B17011_001',
                               household_medincome = 'B19013_001',
                               not_enrolled_school = 'B14001_010',
                               not_in_labor_force = 'B23025_007',
                               pct_below_poverty_level = 'B06012_002',
                               vacancy_status = 'B25004_001',
                               total_pop = 'B01003_001',
                               White_alone = 'B02001_002',
                               Black_or_African_American_alone = 'B02001_003',
                               American_Indian_and_Alaska_Native_alone = 'B02001_004',
                               Asian_alone = 'B02001_005',
                               Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006',
                               aggregate_time_to_work = 'B08131_001'
                 ),
                 zip = "MA", 
                 year = year)
  
  df <- dat %>%
    mutate(GEOID = as.integer(GEOID)) %>%
    filter(GEOID %in% zips) %>%
    select(-moe) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(year = year)
  
  return(df)
}

years <- c(2022,2021,2020,2019,2018,2017, 2016, 2015)
dfs <- list()

for (year in years) {
  df <- fetch_acs_data(year, zips)
  dfs[[length(dfs) + 1]] <- df
}

combined_df <- bind_rows(dfs)
nrow(combined_df)

#get original data set columns
selected_columns <- shooting_data[, c(
  "district_name",
  "incident_num",
  "Date",
  "Time",
  "district",
  "victim_gender",
  "victim_race",
  "multi_victim",
  "v_hispanic_or_latinx",
  "fatal",
  "Total."
)]

#add year column to shooting dataset 
selected_columns$year <- year(as.Date(selected_columns$Date))

zips<-c(02135,02121, 02122, 02124, 02125,02128,02136,02130,02126,02118,02119,02120,02132,02127,02111, 02116, 02118, 02119, 02120, 02127,02132)

#add district names corresponding to zipcodes/Geoid for final merge with original dataset
with_districts <- combined_df %>%
  mutate(district_name = case_when(
    GEOID == 2135 ~ "Brighton",
    GEOID %in% c(2121, 2122, 2124, 2125) ~ "Dorchester",
    GEOID == 2128 ~ "East Boston",
    GEOID == c(2136,2137) ~ "Hyde Park",
    GEOID == c(2130,2135) ~ "Jamaica Plain",
    GEOID == 2126 ~ "Mattapan",
    GEOID %in% c(2119,2120,2132) ~ "Roxbury",
    GEOID == 2127 ~ "South Boston",
    GEOID %in% c(2111, 2116, 2118, 2127) ~ "South End",
    GEOID == 2132 ~ "West Roxbury",
    TRUE ~ NA_character_
  ))

final_df<-merge(selected_columns,with_districts, by = c('district_name','year'))

#get tract geometry
data <- read.table("tab20_zcta520_tract20_natl.txt", header = TRUE, sep = "|") #ZCTA to tract conversion file 
head(data)

tract_data<-data%>%
  select(GEOID_TRACT_20,GEOID_ZCTA5_20)
head(tract_data)

final_df<-final_df%>%
  rename(GEOID_ZCTA5_20 = GEOID)
nrow(final_df)

#get tract geom for MA 
with_geometry <- get_acs(
  geography = "tract",
  variables = c(total_house_units = 'B25001_001'),
  state = "MA",
  geometry = TRUE,
  year = 2021
)

#select necessary columns for geometry 
with_geometry_select<-with_geometry%>%
  select(GEOID, NAME, geometry)%>%
  rename(GEOID_TRACT_20=GEOID)

with_geometry_select$GEOID_TRACT_20 <- as.double(with_geometry_select$GEOID_TRACT_20)

merge_tract_zcta<- left_join(final_df, tract_data, by = 'GEOID_ZCTA5_20') #merge ZCTA conversion sheet with our dataframe by ZCTA

merge_geometry<-left_join(merge_tract_zcta, with_geometry_select, by='GEOID_TRACT_20')#merge the new tract data with geometry GEOID 

saveRDS(merge_geometry, "all_census_dat.rdr")




