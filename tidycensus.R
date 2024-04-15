library(tidycensus)
library(tidyverse)
shooting_data <- read.csv(here::here("dataset", "BostonShootingDataClean.csv"))
shooting_data <- mutate(shooting_data, year = year(Date))
unique(shooting_data$year)
names(shooting_data)
head(shooting_data)
unique(shooting_data$district_name)

census_api_key('edc88cbdb20f0ecb1fde3abf4e45a732dd998e96')

brighton_zip<-02135
dorchestor_zip<-c(02121, 02122, 02124, 02125)
east_boston_zip< - 02128
hyde_park <- 02136
jamaica_plain <-02130
Mattapan<-02126
Roxbury<-c(02118,02119,02120,02132)
south_boston<-02127
south_end<-c(02111, 02116, 02118, 02119, 02120, 02127)
West_Roxbury<-c(02132)

vars<-load_variables(2017, "acs5", cache = TRUE)

filter<-vars %>%
  filter(str_detect(label, " one person"))

filter$label


dat_2021<-get_acs(geography = 'zcta', 
                 variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                               has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                               pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE'),
                 zip = "MA", 
                 year = 2021)


dat_2020<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',single_parent_household_under_18 = 'B11005_003E'),
                  
                  zip = "MA", 
                  year = 2020)

df_2020 <- dat_2020 %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(year = 2020)

dat_2021<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2021)
df_2021 <- dat_2021 %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(year = 2021)

dat_2018<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2018)

df_2018 <- dat_2018 %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(year = 2018)

dat_2019<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2019)

df_2019 <- dat_2019 %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(year = 2019)

dat_2017<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2017)

df_2020 <- dat_2020 %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(year = 2017)

dat_2016<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2016)

df_2020 <- dat_2016%>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(year = 2016)

dat_2015<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2015)

df_2015 <- dat_2015 %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(year = 2015)

merged_df<- merge(dat_2015,dat_2016, by = )

dat_2023<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2023)


df_2023<- dat_2023 %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(year = 2023)

dat_2024<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2024)


df_2024 <- dat_2024 %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(year = 2024)


#2021 2018 2022 2020 2015 2019 2016 2023 2017 2024


little_english = 'B16004_004E',
has_highschool_diploma ='B15003_017E',occoupation ='C24010_',employment_status = 'B23025_001E',
pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE'
zip_dat

'B16004_005E'#percent of individuals who do no speak english at all 
'B16004_004E'# percent of individuals who do not speak english well 
'B15003_017E'#high school graduate population 
'C24010_'# Occupation by industry 
'B23025_001E'#Employment status 
'B17001_002E'# percent of individuals below poverty level 
'B17001_003E'#percent above poverty level 
'DP04_0046PE' # percent of homes that are owner occupied 
'DP02_0068P' # percent 25 and up with bachelors degree

df_subset <- shooting_data[, c("district_name","year", "incident_num", "Date", "Time", "district", "victim_gender", "victim_race", "multi_victim", "v_hispanic_or_latinx", "fatal", "Total.")]

