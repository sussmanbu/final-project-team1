library(tidycensus)
library(tidyverse)
shooting_data <- read.csv(here::here("dataset", "BostonShootingDataClean.csv"))
shooting_data <- mutate(shooting_data, year = year(Date))

census_api_key('edc88cbdb20f0ecb1fde3abf4e45a732dd998e96')



zips<-c(2135,2121, 2122, 2124, 2125,2128,2136,2130,2126,2118,2119,2120,2132,2127,2111, 2116, 2118, 2119, 2120, 2127,2132)

#Websites to help query: https://censusreporter.org/topics/table-codes/
#https://data.census.gov/table/ACSST5Y2022.S1501?q=high%20school&g=1400000US48201553300

vars<-load_variables(2022, "acs1", cache = TRUE)
search<-vars %>%
  filter(str_detect(label,'age'))
print(search, n=100)

vars<-load_variables(2022, "acs1", cache = TRUE)
search<-vars %>%
  filter(str_detect(name,'B25001'))
print(search, n=700)

#variables: 
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

dat_2022<-get_acs(geography = 'zcta', 
                  variables = c(total_house_units = 'B25001_001',non_married_household = 'B11001A_004', bacehelors_25 = 'B15003_022', one_parent = 'B23008_021', married_households='B11001A_003', income_deficit = 'B17011_001', medincome = "B19013_001",
                                not_enrolled_school ='B14001_010',not_in_labor_force = 'B23025_007',
                                pct_below_poverty_level = 'B06012_002',vacancy_status = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002', Black_or_African_American_alone = 'B02001_003',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004',Asian_alone = 'B02001_005', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006'),
                  zip = "MA", 
                  year = 2022)

df_2022<-dat_2022%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)
df_2022 <- df_2022 %>% mutate(year = 2022)


dat_2021<-get_acs(geography = 'zcta', 
                  variables = c(total_house_units = 'B25001_001',non_married_household = 'B11001A_004',bacehelors_25 = 'B15003_022', one_parent = 'B23008_021', married_households='B11001A_003', income_deficit = 'B17011_001', medincome = "B19013_001",
                                not_enrolled_school ='B14001_010',not_in_labor_force = 'B23025_007',
                                pct_below_poverty_level = 'B06012_002',vacancy_status = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002', Black_or_African_American_alone = 'B02001_003',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004',Asian_alone = 'B02001_005', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006'),
                  
                  zip = "MA", 
                  year = 2021)

df_2021<-dat_2021%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2021 <- df_2021 %>% mutate(year = 2021)

dat_2020<-get_acs(geography = 'zcta', 
                  variables = c(total_house_units = 'B25001_001',non_married_household = 'B11001A_004',bacehelors_25 = 'B15003_022', one_parent = 'B23008_021', married_households='B11001A_003', income_deficit = 'B17011_001', medincome = "B19013_001",
                                not_enrolled_school ='B14001_010',not_in_labor_force = 'B23025_007',
                                pct_below_poverty_level = 'B06012_002',vacancy_status = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002', Black_or_African_American_alone = 'B02001_003',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004',Asian_alone = 'B02001_005', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006'),
                  
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
                  variables = c(total_house_units = 'B25001_001',non_married_household = 'B11001A_004',bacehelors_25 = 'B15003_022',one_parent = 'B23008_021', married_households='B11001A_003', income_deficit = 'B17011_001', medincome = "B19013_001",
                                not_enrolled_school ='B14001_010',not_in_labor_force = 'B23025_007',
                                pct_below_poverty_level = 'B06012_002',vacancy_status = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002', Black_or_African_American_alone = 'B02001_003',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004',Asian_alone = 'B02001_005', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006'),
                  
                  zip = "MA", 
                  year = 2019)

df_2019<-dat_2019%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2019 <- df_2019 %>% mutate(year = 2019)


dat_2018<-get_acs(geography = 'zcta', 
                  variables = c(total_house_units = 'B25001_001',non_married_household = 'B11001A_004',bacehelors_25 = 'B15003_022', one_parent = 'B23008_021', married_households='B11001A_003', income_deficit = 'B17011_001', medincome = "B19013_001",
                                not_enrolled_school ='B14001_010',not_in_labor_force = 'B23025_007',
                                pct_below_poverty_level = 'B06012_002',vacancy_status = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002', Black_or_African_American_alone = 'B02001_003',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004',Asian_alone = 'B02001_005', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006'),
                  
                  zip = "MA", 
                  year = 2018)

df_2018<-dat_2018%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2018 <- df_2018 %>% mutate(year = 2018)


dat_2017<-get_acs(geography = 'zcta', 
                  variables = c(total_house_units = 'B25001_001',non_married_household = 'B11001A_004',bacehelors_25 = 'B15003_022', one_parent = 'B23008_021', married_households='B11001A_003', income_deficit = 'B17011_001', medincome = "B19013_001",
                                not_enrolled_school ='B14001_010',not_in_labor_force = 'B23025_007',
                                pct_below_poverty_level = 'B06012_002',vacancy_status = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002', Black_or_African_American_alone = 'B02001_003',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004',Asian_alone = 'B02001_005', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006'),
                  
                  zip = "MA", 
                  year = 2017)

df_2017<-dat_2017%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2017 <- df_2017 %>% mutate(year = 2017)

dat_2016<-get_acs(geography = 'zcta', 
                  variables = c(total_house_units = 'B25001_001',non_married_household = 'B11001A_004',bacehelors_25 = 'B15003_022', one_parent = 'B23008_021', married_households='B11001A_003', income_deficit = 'B17011_001', medincome = "B19013_001",
                                not_enrolled_school ='B14001_010',not_in_labor_force = 'B23025_007',
                                pct_below_poverty_level = 'B06012_002',vacancy_status = 'B25004_001',
                                total_pop = 'B01003_001', White_alone = 'B02001_002', Black_or_African_American_alone = 'B02001_003',
                                American_Indian_and_Alaska_Native_alone = 'B02001_004',Asian_alone = 'B02001_005', 
                                Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006'),
                  
                  zip = "MA", 
                  year = 2016)

df_2016<-dat_2016%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2016 <- df_2016 %>% mutate(year = 2016)

dat_2015<-get_acs(geography = 'zcta', 
                  variables =c(total_house_units = 'B25001_001',non_married_household = 'B11001A_004',bacehelors_25 = 'B15003_022', one_parent = 'B23008_021', married_households='B11001A_003', income_deficit = 'B17011_001', medincome = "B19013_001",
                               not_enrolled_school ='B14001_010',not_in_labor_force = 'B23025_007',
                               pct_below_poverty_level = 'B06012_002',vacancy_status = 'B25004_001',
                               total_pop = 'B01003_001', White_alone = 'B02001_002', Black_or_African_American_alone = 'B02001_003',
                               American_Indian_and_Alaska_Native_alone = 'B02001_004',Asian_alone = 'B02001_005', 
                               Native_Hawaiian_and_Other_Pacific_Islander_alone = 'B02001_006'),
                  
                  zip = "MA", 
                  year = 2015)

df_2015<-dat_2015%>%
  mutate(GEOID = as.integer(GEOID))%>%
  filter(GEOID %in% zips)%>%
  select(-moe)%>%
  pivot_wider(names_from = variable, values_from = estimate)

df_2015 <- df_2015 %>% mutate(year = 2015)


#combine all the acs data into one table 
combined_table <- rbind(df_2015, df_2016, df_2017,df_2018, df_2019, df_2020,df_2021,df_2022)

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
with_districts <- combined_table %>%
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
library(leaps)
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
  summarise(medincome_mean= mean(medincome), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes( medincome_mean,total_cases))+
  geom_point()+
  geom_smooth(method ='lm')+
  labs(y = 'Avg median income',title = 'Avg Median Income and Total Cases Over All Years')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

#Bachelors degree plot
correlation_coef <- final_df%>%
  group_by(district_name)%>%
  summarise(pct_not_enrolled= mean(bacehelors_25/total_pop), total_cases = sum(!duplicated(incident_num)))%>%
  summarise(correlation_coef = round(cor(total_cases, pct_not_enrolled),2))

final_df%>%
  group_by(district_name)%>%
  summarise(pct_not_enrolled= mean(bacehelors_25/total_pop), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name, pct_not_enrolled,fill = total_cases))+
  geom_bar(stat= 'identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = 'Pct of population with high school diploma',title = 'Avg Populaion with HS Diploma and Total Cases Over All Years')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

final_df%>%
  group_by(district_name)%>%
  summarise(pct_not_enrolled= mean(bacehelors_25/total_pop), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(pct_not_enrolled, total_cases))+
  geom_point()+
  geom_smooth(method ='lm')


#not in labor force plot

correlation_coef <- final_df%>%
  group_by(district_name)%>%
  summarise(unemployment_pct= mean(not_in_labor_force/total_pop), total_cases = sum(!duplicated(incident_num)))%>%
  summarise(correlation_coef = round(cor(total_cases, unemployment_pct),2))

final_df%>%
  group_by(district_name)%>%
  summarise(unemployment_pct= mean(not_in_labor_force/total_pop), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name, unemployment_pct,fill = total_cases))+
  geom_bar(stat= 'identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = 'Pct of population employed',title = 'Avg Employment pct and Total Cases Over All Years')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

final_df%>%
  group_by(district_name)%>%
  summarise(unemployment_pct= mean(not_in_labor_force/total_pop), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(unemployment_pct, total_cases))+
  geom_point()+
  geom_smooth(method ='lm')

final_df%>%
  group_by(district_name)%>%
  summarise(unemployment_pct= mean(not_in_labor_force/total_pop), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(unemployment_pct))+
  geom_histogram()

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
  summarise(single_household = mean(one_parent/ total_pop),total_cases = sum(!duplicated(incident_num))) %>%
  summarise(correlation_coef = round(cor(single_household, total_cases),2))

final_df%>%
  group_by(district_name)%>%
  summarise(single_household = mean(one_parent/ total_pop),total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name,single_household, fill = total_cases))+
  geom_bar(stat= 'identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(title = 'Pct of Households with Single Parent')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

final_df%>%
  group_by(district_name)%>%
  summarise(single_household = mean(one_parent/ total_pop),total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(single_household, total_cases))+
  geom_point()+
  geom_smooth(method ='lm')


#married househomes
correlation_coef <- final_df%>%
  group_by(district_name)%>%
  summarise(married_house = mean(married_households/total_house_units),total_cases = sum(!duplicated(incident_num))) %>%
  summarise(correlation_coef = round(cor(married_house, total_cases),2))

final_df%>%
  group_by(district_name)%>%
  summarise(married_house_pct= mean(married_households/total_house_units), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name, married_house_pct,fill = total_cases))+
  geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = 'Avg pct below poverty level ',title = 'Avg pct below poverty level over All Years')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

final_df%>%
  group_by(district_name)%>%
  summarise(married_house_pct= mean(married_households/total_house_units), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(married_house_pct, total_cases))+
  geom_point()+
  geom_smooth(method ='lm')

#vacant househomes

correlation_coef <- final_df%>%
  group_by(district_name)%>%
  summarise(vaccant_house = mean(vacancy_status/total_house_units),total_cases = sum(!duplicated(incident_num))) %>%
  summarise(correlation_coef = round(cor(vaccant_house, total_cases),2))

final_df%>%
  group_by(district_name)%>%
  summarise(vaccant_house_pct= mean(vacancy_status/total_house_units), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name, vaccant_house_pct,fill = total_cases))+
  geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = 'Avg pct below poverty level ',title = 'Avg pct below poverty level over All Years')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

final_df%>%
  group_by(district_name)%>%
  summarise(vaccant_house_pct= mean(vacancy_status/total_house_units), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(vaccant_house_pct, total_cases))+
  geom_point()+
  geom_smooth(method ='lm')




library(olsrr)
lm_dat<-final_df%>%
  group_by(district_name, year)%>%
  summarise(unemployment=mean(not_in_labor_force/total_pop),medincome_mean= mean(medincome),bachelors = mean(bacehelors_25/total_pop),
            one_parent= mean(one_parent/total_house_units),married_house= mean(married_households/total_house_units),vacant_houses = mean(vacancy_status/total_house_units),
            total_cases = sum(!duplicated(incident_num)))

ols<-lm(total_cases~ district_name+ unemployment+ medincome_mean+bachelors+one_parent+ vacant_houses+married_house, data = lm_dat)
coeftest(ols, vcov. = vcovHC)
summary(ols) #92 adjusted Rsquared good for forecasting
vif(ols)
ols_step_all_possible(ols)
ols_step_forward_p(ols)
ols_step_backward_p(ols)
#interpretation: where you live is the best predictor of cases as shown by total cases by locations graph, p-values are high for other variables indicates multicollinearity
#vif, anything above 5 high collinearity, below 1 no collinearity, above 1 medium collinearity 

I(age^2)
ols<-lm(total_cases~+ unemployment +medincome_mean+bachelors+one_parent+married_house, data = lm_dat)
coeftest(ols, vcov. = vcovHC)
summary(ols)
vif(ols)
ols_step_forward_p(ols)
ols_step_backward_p(ols)


plot(ols)


#time series forecast:
library(dlym)

final_df%>%
  group_by(year)%>%
  summarise(total_cases = sum(!duplicated(incident_num)))%>%
  ggplot(aes(year, total_cases))+
  geom_line()




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







