library(tidycensus)
library(tidyverse)
shooting_data <- read.csv(here::here("dataset", "BostonShootingDataClean.csv"))
shooting_data <- mutate(shooting_data, year = year(Date))

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

filter_vars<-vars %>%
  filter(str_detect(label, " one person"))




zips<-c(2135,2121, 2122, 2124, 2125,2128,2136,2130,2126,2118,2119,2120,2132,2127,2111, 2116, 2118, 2119, 2120, 2127,2132)



dat_2020<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",noenglish = 'B16004_005E',littleenglish = 'B16004_004E',
                                hashighschooldiploma ='B15003_017E',employmentstatus = 'B23025_001E',
                                pctbelowpoverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',single_parent_household_under_18 = 'B11005_003E'),
                  
                  zip = "MA", 
                  year = 2020)


df_2020<-dat_2020%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)
  
df_2020 <- df_2020 %>% mutate(year = 2020)


dat_2021<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2021)


df_2021<-dat_2021%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2021 <- df_2021 %>% mutate(year = 2021)


dat_2018<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2018)
df_2018<-dat_2018%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2018 <- df_2018 %>% mutate(year = 2018)


dat_2019<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2019)

df_2019<-dat_2019%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2019 <- df_2019 %>% mutate(year = 2019)


dat_2017<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2017)

df_2017<-dat_2017%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2017 <- df_2017 %>% mutate(year = 2017)


dat_2016<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B15003_017E',employment_status = 'B23025_001E',
                                pct_below_poverty_level = 'B17001_002E',pct_homes_owner_occupied = 'DP04_0046PE',
                                total_pop = 'B02001_002E', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P'),
                  
                  zip = "MA", 
                  year = 2016)

df_2016<-dat_2016%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2016 <- df_2016 %>% mutate(year = 2016)

dat_2015<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = "B16004_005E",little_english = "B16004_004E",
                                has_highschool_diploma ="B15003_017E",employment_status = "B23025_001E",
                                pct_below_poverty_level = "B17001_002E",pct_homes_owner_occupied = "DP04_0046PE",
                                total_pop = "B02001_002E", White_alone = "B02001_002E", Black_or_African_American_alone = "B02001_003E",
                                American_Indian_and_Alaska_Native_alone = "B02001_004E",Asian_alone = "B02001_005E", 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = "B02001_006E", pct_25_and_up_bachelors_degree = "DP02_0068P"),
                  
                  zip = "MA", 
                  year = 2015)

df_2015<-dat_2015%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2015 <- df_2015 %>% mutate(year = 2015)

combined_table <- bind_rows(df_2015, df_2016, df_2017,df_2018, df_2019, df_2020,df_2021)

new_column_names<-c(medincome = "B19013_001",no_english = "B16004_005E",little_english = "B16004_004E",
  has_highschool_diploma ="B15003_017E",employment_status = "B23025_001E",
  pct_below_poverty_level = "B17001_002E",pct_homes_owner_occupied = "DP04_0046PE",
  total_pop = "B02001_002E", White_alone = "B02001_002E", Black_or_African_American_alone = "B02001_003E",
  American_Indian_and_Alaska_Native_alone = "B02001_004E",Asian_alone = "B02001_005E", 
  Native_Hawaiian_and_Other_Pacific_Islander_alone = "B02001_006E", pct_25_and_up_bachelors_degree = "DP02_0068P")

df <- combined_table %>%
  rename(
    medincome = medincome,
    no_english = B16004_005,
    little_english = B16004_004,
    has_highschool_diploma = B15003_017,
    employment_status = B23025_001,
    pct_below_poverty_level = B17001_002,
    pct_homes_owner_occupied = DP04_0046P,
    total_pop = B02001_002,
    White_alone = B02001_003,
    Black_or_African_American_alone = B02001_004,
    American_Indian_and_Alaska_Native_alone = B02001_005,
    Asian_alone = B02001_006, 
    Native_Hawaiian_and_Other_Pacific_Islander_alone = DP04_0046P,
    pct_25_and_up_bachelors_degree = pct_25_and_up_bachelors_degree
  )

colnames(df)

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

unique(selected_columns$district_name)

selected_columns$year <- year(as.Date(selected_columns$Date))

combined_table$year

nrow(combined_table)
nrow(selected_columns)


print(names(df))
df$NAME
combined_table$GEOID

zips<-c(02135,02121, 02122, 02124, 02125,02128,02136,02130,02126,02118,02119,02120,02132,02127,02111, 02116, 02118, 02119, 02120, 02127,02132)

with_districts <- df %>%
  mutate(district_name = case_when(
    GEOID == 2135 ~ "Brighton",
    GEOID %in% c(2121, 2122, 2124, 2125) ~ "Dorchestor",
    GEOID == 2128 ~ "East Boston",
    GEOID == 2136 ~ "Hyde Park",
    GEOID == 2130 ~ "Jamaica Plain",
    GEOID == 2126 ~ "Mattapan",
    GEOID %in% c(2118, 2119, 2120, 2132) ~ "Roxbury",
    GEOID == 2127 ~ "South Boston",
    GEOID %in% c(2111, 2116, 2118, 2119, 2120, 2127) ~ "South End",
    GEOID == 2132 ~ "West Roxbury",
    TRUE ~ NA_character_
  ))


colnames(with_districts)
colnames(selected_columns)

nrow(selected_columns)

final_df<-merge(selected_columns,with_districts, by = c('district_name','year'))

write.csv(final_df, file = "census_dat.csv", row.names = FALSE)

colnames(final_df)
unique(final_df$year)
unique(selected_columns$year)
unique(with_districts$year)

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


dat_2023<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = "B16004_005E",little_english = "B16004_004E",
                                has_highschool_diploma ="B15003_017E",employment_status = "B23025_001E",
                                pct_below_poverty_level = "B17001_002E",pct_homes_owner_occupied = "DP04_0046PE",
                                total_pop = "B02001_002E", White_alone = "B02001_002E", Black_or_African_American_alone = "B02001_003E",
                                American_Indian_and_Alaska_Native_alone = "B02001_004E",Asian_alone = "B02001_005E", 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = "B02001_006E", pct_25_and_up_bachelors_degree = "DP02_0068P"),
                  
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
                  variables = c(medincome = "B19013_001", no_english = 'B16004_005E',little_english = 'B16004_004E',
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

\
