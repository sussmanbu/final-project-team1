library(tidycensus)
library(tidyverse)
shooting_data <- read.csv(here::here("dataset", "BostonShootingDataClean.csv"))
shooting_data <- mutate(shooting_data, year = year(Date))

census_api_key('edc88cbdb20f0ecb1fde3abf4e45a732dd998e96')



zips<-c(2135,2121, 2122, 2124, 2125,2128,2136,2130,2126,2118,2119,2120,2132,2127,2111, 2116, 2118, 2119, 2120, 2127,2132)

#Websites to help query: https://censusreporter.org/topics/table-codes/
#https://data.census.gov/table/ACSST5Y2022.S1501?q=high%20school&g=1400000US48201553300

#tables: Employment status: B23025

#variables: 
# B06012_002 below 100% poverty level 
# B01003_001 total population
# B23025_007 Estimate!!Total:!!Not in labor force
# B25004_001 total vacancy 
# B23008_021 Estimate!!Total!!6 to 17 years!!Living with one parent    
# B14001_010 Estimate!!Total:!!Not enrolled in school School Enrollment by Level of School for the Population 3 Years and Over


vars<-load_variables(2022, "acs1", cache = TRUE)
search<-vars %>%
  filter(str_detect(label,'high school'))
print(search, n=100)

vars<-load_variables(2022, "acs1", cache = TRUE)
search<-vars %>%
  filter(str_detect(name,'B25004'))
print(search, n=100)

dat_2022<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B16010_002',not_in_labor_force = 'B23025_007',
                                pct_below_poverty_level = 'B06012_002',pct_vacant = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',one_parent = 'B23008_021'),
                  zip = "MA", 
                  year = 2022)

df_2022<-dat_2022%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)
df_2022 <- df_2022 %>% mutate(year = 2022)


dat_2021<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B16010_002',employment_status = 'B23025_001',
                                pct_below_poverty_level = 'B06012_002',pct_vacant = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',one_parent = 'B23008_021'),
                  
                  zip = "MA", 
                  year = 2021)

df_2021<-dat_2021%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2021 <- df_2021 %>% mutate(year = 2021)

dat_2020<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B16010_002',employment_status = 'B23025_001',
                                pct_below_poverty_level = 'B06012_002',pct_vacant = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',one_parent = 'B23008_021'),
                  
                  zip = "MA", 
                  year = 2020)


#filter for zipcodes in collected list of zipcodes. Pivot so each variable has its own column
df_2020<-dat_2020%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)
  
#add year column
df_2020 <- df_2020 %>% mutate(year = 2020)

dat_2019<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B16010_002',employment_status = 'B23025_001',
                                pct_below_poverty_level = 'B06012_002',pct_vacant = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',one_parent = 'B23008_021'),
                  
                  zip = "MA", 
                  year = 2019)

df_2019<-dat_2019%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2019 <- df_2019 %>% mutate(year = 2019)


dat_2018<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B16010_002',employment_status = 'B23025_001',
                                pct_below_poverty_level = 'B06012_002',pct_vacant = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',one_parent = 'B23008_021'),
                  
                  zip = "MA", 
                  year = 2018)

df_2018<-dat_2018%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2018 <- df_2018 %>% mutate(year = 2018)


dat_2017<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B16010_002',employment_status = 'B23025_001',
                                pct_below_poverty_level = 'B06012_002',pct_vacant = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',one_parent = 'B23008_021'),
                  
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
                                has_highschool_diploma ='B16010_002',employment_status = 'B23025_001',
                                pct_below_poverty_level = 'B06012_002',pct_vacant = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',one_parent = 'B23008_021'),
                  
                  zip = "MA", 
                  year = 2016)

df_2016<-dat_2016%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2016 <- df_2016 %>% mutate(year = 2016)

dat_2015<-get_acs(geography = 'zcta', 
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B16010_002',employment_status = 'B23025_001',
                                pct_below_poverty_level = 'B06012_002',pct_vacant = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',one_parent = 'B23008_021'),
                  
                  zip = "MA", 
                  year = 2015)

df_2015<-dat_2015%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2015 <- df_2015 %>% mutate(year = 2015)

#combine all the acs data into one table 
combined_table <- bind_rows(df_2015, df_2016, df_2017,df_2018, df_2019, df_2020,df_2021)


new_column_names<-c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                    has_highschool_diploma ='B16010_002',employment_status = 'B23025_001',
                    pct_below_poverty_level = 'B06012_002',pct_vacant = 'B25004_001',
                    total_pop = 'B01003_001', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                    American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                    Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',one_parent = 'B23008_021')

#rename the census return variables B02001_002
df <- combined_table %>%
  rename(
    White_alone = B02001_002,
    little_english = B16004_004,
    no_english = B16004_005,
    Black_or_African_American_alone = B02001_004,
    American_Indian_and_Alaska_Native_alone = B02001_005,
    Asian_alone = B02001_006, 
    below_highschool =has_highschool_diploma
  )

colnames(df)

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
with_districts <- df %>%
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

write.csv(final_df, file = "census_dat.csv", row.names = FALSE)
colnames(final_df)


library(tidyverse)
library(sandwich)
library(lmtest)
library(car)
final_df<-read.csv('census_dat.csv')

#shows correlation between income and total cases for each district. Lower income => more cases 

#income plot
correlation_coef <- final_df%>%
  group_by(district_name)%>%
  summarise(medincome_mean= mean(medincome), total_cases = sum(!duplicated(incident_num))) %>%
  summarise(correlation_coef = round(cor(total_cases, medincome_mean),2))

final_df%>%
  group_by(district_name)%>%
  select(year, district_name, medincome, incident_num)%>%
  summarise(medincome_mean= mean(medincome), total_cases = sum(!duplicated(incident_num))) %>%
  distinct()%>%
  ggplot(aes(district_name, medincome_mean,fill = total_cases))+
  geom_bar(stat= 'identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = 'Avg median income',title = 'Avg Median Income and Total Cases Over All Years')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

#scatterplot with line of best fit 
final_df%>%
  group_by(district_name)%>%
  select(year, district_name, medincome, incident_num)%>%
  summarise(medincome_mean= mean(medincome), total_cases = sum(!duplicated(incident_num))) %>%
  distinct()%>%
  ggplot(aes( medincome_mean,total_cases))+
  geom_point()+
  geom_smooth(method ='lm')+
  labs(y = 'Avg median income',title = 'Avg Median Income and Total Cases Over All Years')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

#Highschool plot
final_df%>%
  group_by(district_name)%>%
  select(year, district_name, below_highschool, Total.,incident_num)%>%
  summarise(high_school_pct= mean(below_highschool/Total.), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name, high_school_pct,fill = total_cases))+
  geom_bar(stat= 'identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = 'Pct of population with high school diploma',title = 'Avg Populaion with HS Diploma and Total Cases Over All Years')

#employment plot

correlation_coef <- final_df%>%
  group_by(district_name)%>%
  summarise(employment_pct= mean(employment_status/Total.), total_cases = sum(!duplicated(incident_num)))%>%
  summarise(correlation_coef = round(cor(total_cases, employment_pct),2))

final_df%>%
  group_by(district_name)%>%
  summarise(employment_pct= mean(employment_status/Total.), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name, employment_pct,fill = total_cases))+
  geom_bar(stat= 'identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = 'Pct of population employed',title = 'Avg Employment pct and Total Cases Over All Years')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

#total cases for districts plot

final_df%>%
  group_by(district_name)%>%
  summarise(total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name,total_cases))+
  geom_bar(stat= 'identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = 'Total Cases by District')

#single parent household plot
correlation_coef <- final_df%>%
  group_by(district_name)%>%
  summarise(single_household = mean(one_parent/ Total.),total_cases = sum(!duplicated(incident_num))) %>%
  summarise(correlation_coef = round(cor(single_household, total_cases),2))
final_df%>%
  group_by(district_name)%>%
  summarise(single_household = mean(one_parent/ Total.),total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name,single_household, fill = total_cases))+
  geom_bar(stat= 'identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = 'Pct of Households with Single Parent')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)




#poverty level plot
correlation_coef <- final_df%>%
  group_by(district_name)%>%
  summarise(poverty_pct = mean(pct_below_poverty_level/ Total.),total_cases = sum(!duplicated(incident_num))) %>%
  summarise(correlation_coef = round(cor(poverty_pct, total_cases),2))
final_df%>%
  group_by(district_name)%>%
  summarise(pct_below_poverty_level= mean(pct_below_poverty_level/Total.), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name, pct_below_poverty_level,fill = total_cases))+
  geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = 'Avg pct below poverty level ',title = 'Avg pct below poverty level over All Years')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)


lm_dat<-final_df%>%
  group_by(district_name, year)%>%
  summarise(
            pct_below_poverty_level=mean(pct_below_poverty_level/Total.),medincome_mean= mean(medincome),
            employment_pct = mean(employment_status/Total.),
            total_cases = sum(!duplicated(incident_num)),single_household = mean(one_parent/ Total.))

ols<-lm(total_cases~ district_name+ medincome_mean+ pct_below_poverty_level+employment_pct, data = lm_dat)
coeftest(ols, vcov. = vcovHC)
summary(ols) #92 adjusted Rsquared good for forecasting
vif(ols)
#interpretation: where you live is the best predictor of cases as shown by total cases by locations graph, p-values are high for other variables indicates multicollinearity
#vif, anything above 5 high collinearity, below 1 no collinearity, above 1 medium collinearity 

ols<-lm(total_cases~medincome_mean+single_household+pct_below_poverty_level, data = lm_dat)
coeftest(ols, vcov. = vcovHC)
summary(ols)
vif(ols)
#interpretation these variables are highly collinear, signs of the coefficients do not make sense 
#however as single households increases, cases increas,
#as employment pct increases, cases decrease
#check signs of estimates

#check these plots for residuals, QQ, outliers
plot(ols)

logit_dat<-final_df%>%
  group_by(district_name, year,victim_gender)%>%
  summarise(victim_gender =victim_gender, no_english_pct = mean(no_english/Total.), black_pct =mean(Black_or_African_American_alone/Total.), 
            pct_below_poverty_level=mean(pct_below_poverty_level/Total.),medincome_mean= mean(medincome),
            mean_high_school_pct =mean(below_highschool/Total.), employment_pct = mean(employment_status/Total.),
            total_cases = sum(!duplicated(incident_num)),single_household = mean(one_parent/ Total.))%>%
  distinct()

#logistic regression to predict victims gender, residuals highly heteroskedastic, 
logit <- glm(factor(victim_gender) ~ employment_pct+ pct_below_poverty_level+mean_high_school_pct+ medincome_mean, data = logit_dat, family = binomial)
coeftest(logit, vcov.=vcovHC)#heteroskedastic standard errors
summary(logit)#homoskedasitic standard erros formula, pvalues much higher 





#update variables for these years 

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
                  variables = c(medincome = "B19013_001",no_english = 'B16004_005E',little_english = 'B16004_004E',
                                has_highschool_diploma ='B16010_002',employment_status = 'B23025_001',
                                pct_below_poverty_level = 'B06012_002',pct_vacant = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002E', Black_or_African_American_alone = 'B02001_003E',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004E',Asian_alone = 'B02001_005E', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006E', pct_25_and_up_bachelors_degree = 'DP02_0068P',one_parent = 'B23008_021'),
                
                  zip = "MA", 
                  year = 2024)


df_2024 <- dat_2024 %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(year = 2024)




#List of variables and years for acs data 

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


