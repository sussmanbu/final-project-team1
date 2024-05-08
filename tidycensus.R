library(tidycensus)
library(tidyverse)
library(readr)
library(tidycensus)
library(tidyverse)
library(sf)

shooting_data <- read.csv(here::here("dataset", "BostonShootingDataClean.csv"))
shooting_data <- mutate(shooting_data, year = year(Date))

census_api_key('')

#zip codes for our districts collected from the internet 
zips<-c(2135,2121, 2122, 2124, 2125,2128,2136,2130,2126,2118,2119,2120,2132,2127,2111, 2116, 2118, 2119, 2120, 2127,2132)

#Websites to help query: https://censusreporter.org/topics/table-codes/
#https://data.census.gov/table/ACSST5Y2022.S1501?q=high%20school&g=1400000US48201553300

vars<-load_variables(2022, "acs1", cache = TRUE)
search<-vars %>%
  filter(str_detect(label,'age'))
print(search, n=100)

vars<-load_variables(2022, "acs1", cache = TRUE)
search<-vars %>%
  filter(str_detect(name,'B17002_001'))
print(search, n=700)

#variables considered: 
# B25001_001 Estimate!!Total Housing Units
# B11001A_004 Estimate!!Total:!!Family households:!!Other family: 
# B08131_001 Estimate!!Aggregate travel time to work (in minutes):  
# B17002_001 Estimate!!Total:                Ratio of Income to Poverty Level in the Past 12 Months
# B19013_001 Estimate!!Median household income in the past 12 months (in 2022 inflation-adjusted dollars) Median Household I…
# B01003_001 Estimate!!Total Total Population
# B23025_007 Estimate!!Total:!!Not in labor force Employment Status for the Population 16 Years and Over
# B25004_001 Estimate!!Total: Vacancy Status
# B23008_021 Estimate!!Total!!6 to 17 years!!Living with one parent    
# B14001_010 Estimate!!Total:!!Not enrolled in school School Enrollment by Level of School for the Population 3 Years and Over
# B15003_022 Estimate!!Total:!!Bachelor's degree                        Educational Attainment for the Population 25 Years and Over
# B17011_001 Estimate!!Aggregate income deficit in the past 12 months:                                               
# B11001A_003 Estimate!!Total:!!Family households:!!Married-couple family Household Type (Including Living Alone) (White Alo…
# B02001_002 Estimate!!Total:!!White alone Race 
# B02001_003 Estimate!!Total:!!Black or African American alone Race   
#Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006'
# American_Indian_and_Alaska_Native_alone = 'B02001_004'
#Asian_alone = 'B02001_005'
#pct_25_and_up_bachelors_degree = 'DP02_0068P'
#B08131	Aggregate Travel Time to Work (In Minutes) of Workers by
#B23020	Mean Usual Hours Worked for Workers 16 to 64 Years


#chat GPT, create function to loop over set of years and make an api call 
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
  
  # pivot the returned table 
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

final_df<-readRDS('all_census_dat.rdr')









