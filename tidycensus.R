library(tidycensus)
library(tidyverse)
shooting_data <- read.csv(here::here("dataset", "BostonShootingDataClean.csv"))
shooting_data <- mutate(shooting_data, year = year(Date))

census_api_key('edc88cbdb20f0ecb1fde3abf4e45a732dd998e96')

library(readr)

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

final_df<-readRDS('all_census_dat.rdr')

#write.csv(merge_geometry, file = "census_dat.csv",row.names = FALSE)

library(tidycensus)
library(tidyverse)
library(sandwich)
library(lmtest)
library(car)
library(leaps)
library(segregation)
library(tigris)
library(sf)
library(olsrr)

#shows correlation between income and total cases for each district. Lower income => more cases 

#time to work plot 


final_df%>%
  group_by(district_name,year)%>%
  na.omit(aggregate_time_to_work)%>%
  summarise(work_time= mean(aggregate_time_to_work), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes( work_time,total_cases))+
  geom_point()+
  geom_smooth(method ='lm')+
  labs(y = 'Avg travel time to work',title = 'Avg Travel Time to Work and Total Cases')

#income plot
correlation_coef <- final_df%>%
  group_by(district_name)%>%
  summarise(medincome_mean= mean(household_medincome), total_cases = sum(!duplicated(incident_num))) %>%
  summarise(correlation_coef = round(cor(total_cases, medincome_mean),2))

final_df%>%
  group_by(district_name)%>%
  summarise(medincome_mean= mean(household_medincome), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name, medincome_mean,fill = total_cases))+
  geom_bar(stat= 'identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = 'Avg median income',title = 'Avg Median Income and Total Cases Over All Years')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

#scatterplot with line of best fit 
final_df%>%
  group_by(district_name)%>%
  summarise(medincome_mean= mean(household_medincome), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes( medincome_mean,total_cases))+
  geom_point()+
  geom_smooth(method ='lm')+
  labs(y = 'Avg median income',title = 'Avg Median Income and Total Cases Over All Years')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

#Bachelors degree plot
correlation_coef <- final_df%>%
  group_by(district_name)%>%
  summarise(pct_not_enrolled= mean(bachelors_25/total_pop), total_cases = sum(!duplicated(incident_num)))%>%
  summarise(correlation_coef = round(cor(total_cases, pct_not_enrolled),2))

final_df%>%
  group_by(district_name)%>%
  summarise(pct_bachelors= mean(bachelors_25/total_pop), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(district_name, pct_bachelors,fill = total_cases))+
  geom_bar(stat= 'identity')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  labs(y = 'Pct of Population with Bachelors ',title = 'Bachelors Degree and Total Cases')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

final_df%>%
  group_by(district_name)%>%
  summarise(pct_bachelors= mean(bachelors_25/total_pop), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(pct_bachelors, total_cases))+
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
  labs(y = 'Pct Unemployed',title = 'Avg Unemployment and Total Cases')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

final_df%>%
  group_by(district_name)%>%
  summarise(unemployment_pct= mean(not_in_labor_force/total_pop), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(unemployment_pct, total_cases))+
  geom_point()+
  geom_smooth(method ='lm')


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
  labs(y = 'Avg Pct Married Households',title = 'Avg Married Households and Total Cases ')+
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
  labs(y = 'Vacant Households ',title = 'Avg Vacant Households and Total Cases')+
  annotate("text", x = Inf, y = Inf, label = paste("Correlation coefficient: ", correlation_coef), hjust = 1.5, vjust = 1)

final_df%>%
  group_by(district_name)%>%
  summarise(vaccant_house_pct= mean(vacancy_status/total_house_units), total_cases = sum(!duplicated(incident_num))) %>%
  ggplot(aes(vaccant_house_pct, total_cases))+
  geom_point()+
  geom_smooth(method ='lm')


lm_dat<-final_df%>%
  group_by(district_name, year)%>%
  summarise(aian_pct = mean(American_Indian_and_Alaska_Native_alone/total_pop),
            nhpt_pct = mean(Native_Hawaiian_and_Other_Pacific_Islander_alone/total_pop), 
            asian_pct = mean(Asian_alone/total_pop),
            pct_black = mean(Black_or_African_American_alone/total_pop),
            pct_white = mean(White_alone/total_pop),
            unemployment=mean(not_in_labor_force/total_pop),
            medincome_mean= mean(household_medincome),
            bachelors = mean(bachelors_25/total_pop),
            one_parent= mean(one_parent/total_house_units),
            married_house= mean(married_households/total_house_units),
            vacant_houses = mean(vacancy_status/total_house_units),
            travel_work = mean(aggregate_time_to_work),
            total_cases = sum(!duplicated(incident_num)))

#OLS with all variables 
ols<-lm(total_cases~travel_work+ pct_white+pct_black+asian_pct+nhpt_pct+aian_pct+district_name+ unemployment+ medincome_mean+bachelors+one_parent+ vacant_houses+married_house, data = lm_dat)
coeftest(ols, vcov. = vcovHC)
summary(ols) #92 adjusted Rsquared good for forecasting
vif(ols) # vif, look for correlated variables,anything above 5 high collinearity, below 1 no collinearity, above 1 medium collinearity 

ols_step_forward_p(ols) #forward and backwards variable selection 
ols_step_backward_p(ols)

#OLS socieoeconomic variables 
ols<-lm(total_cases~ travel_work+unemployment +medincome_mean+bachelors+one_parent+married_house, data = lm_dat)
coeftest(ols, vcov. = vcovHC)
summary(ols)
vif(ols)
ols_step_forward_p(ols)
ols_step_backward_p(ols)
plot(ols)

#OLS race and socieoeconomic variables
ols<-lm(total_cases~ asian_pct+pct_white+pct_black+nhpt_pct+aian_pct+travel_work+unemployment +medincome_mean+bachelors+one_parent+married_house, data = lm_dat)
coeftest(ols, vcov. = vcovHC)
summary(ols)
vif(ols)
ols_step_forward_p(ols)
ols_step_backward_p(ols)
plot(ols)

#OLS socieoeconomic and location
ols<-lm(total_cases~travel_work+district_name+ unemployment+ medincome_mean+bachelors+one_parent+vacant_houses+married_house, data = lm_dat)
coeftest(ols, vcov. = vcovHC)
summary(ols)
vif(ols)
ols_step_forward_p(ols)
ols_step_backward_p(ols)

#OLS race and location
ols<-lm(total_cases~ pct_white+pct_black+asian_pct+nhpt_pct+aian_pct+district_name, data = lm_dat)
coeftest(ols, vcov. = vcovHC)
summary(ols)
vif(ols)
ols_step_forward_p(ols)
ols_step_backward_p(ols)


#Spatial analysis 


#map
race_data<-c("White_alone", "Black_or_African_American_alone",
  "American_Indian_and_Alaska_Native_alone",
  "Asian_alone", "Native_Hawaiian_and_Other_Pacific_Islander_alone")


pivot_race<-final_df%>%
  group_by(year, GEOID_TRACT_20)%>%
  pivot_longer(race_data,names_to ='ethnicity',values_to = 'population_count')%>%
  ungroup()%>%
  group_by(ethnicity, GEOID_TRACT_20)%>%
  summarise(mean_population_count = mean(population_count))

print(pivot_race, n =100)

boston_local_seg <- pivot_race %>%
  mutual_local(
    group = "ethnicity",
    unit = "GEOID_TRACT_20",
    weight = "mean_population_count", 
    wide = TRUE
  )%>%
  rename(GEOID = GEOID_TRACT_20)

boston_local_seg$GEOID<-as.character(boston_local_seg$GEOID)



MA_tracts_seg <- tracts("MA", cb = TRUE, year = 2020) %>%
  inner_join(boston_local_seg, by = "GEOID") 

MA_tracts_seg%>%
  ggplot(aes(fill = ls)) + 
  geom_sf(color = NA) + 
  coord_sf(crs = st_crs(with_geometry))+
  scale_fill_viridis_c(option = "inferno") + 
  theme_void() + 
  labs(fill = "Local\nsegregation index")
  
#Geo weighted Regression 


library(GWmodel)

lm_dat<-final_df%>%
  group_by(district_name, year)%>%
  summarise(unemployment=mean(not_in_labor_force/total_pop),medincome_mean= mean(medincome),bachelors = mean(bacehelors_25/total_pop),
            one_parent= mean(one_parent/total_house_units),married_house= mean(married_households/total_house_units),vacant_houses = mean(vacancy_status/total_house_units),
            total_cases = sum(!duplicated(incident_num)))

formula2 <- "lm(total_cases~ unemployment +medincome_mean+bachelors+one_parent+married_house, data = roxbury_lm_dat)"

dfw_data_sp <- lm_dat %>%
  as_Spatial()

gw_model <- gwr.basic(
  formula = formula2, 
  data = lm_dat, 
  bw = bw,
  kernel = "bisquare",
  adaptive = TRUE
)

#time series forecast:
library(dynlm)
unloadNamespace('dyplr')

final_df%>%
  group_by(year)%>%
  summarise(total_cases = sum(!duplicated(incident_num)))%>%
  ggplot(aes(year, total_cases))+
  geom_line()

lm_dat<-final_df%>%
  group_by(year)%>%
  summarise(unemployment=mean(not_in_labor_force/total_pop),medincome_mean= mean(medincome),bachelors = mean(bacehelors_25/total_pop),
            one_parent= mean(one_parent/total_house_units),married_house= mean(married_households/total_house_units),vacant_houses = mean(vacancy_status/total_house_units),
            total_cases = sum(!duplicated(incident_num)))

unempl_zoo = as.zoo(lm_dat$unemployment)
total_zoo = as.zoo(lm_dat$total_cases)
parent_zoo = as.zoo(lm_dat$one_parent)

reg1 <- dynlm(log(total_zoo) ~ L(log(total_zoo),1:2)+L(log(unempl_zoo),1), data = lm_dat )

test1 <- coeftest(reg1, vcov = vcovHAC)
print(test1)

out1 = data.frame( t = 0:18, Impact = reg1$coef[2:4], UB = reg1$coef[2:4] + 1.96*test1[2:4,2], LB = reg1$coef[2:4] - 1.96*test1[2:4,2] )

FOJC_pctc = as.zoo(FOJC_pctc)
FDD = as.zoo(FDD) 

selected_columns$Date<-as.Date(selected_columns$Date)

count_dates<-selected_columns%>%
  mutate(month_year =format(Date, "%Y-%m"))%>%
  group_by(month_year)%>%
  summarize(unique_dates = rev(unique(month_year)))

print(count_dates,n=200)




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







