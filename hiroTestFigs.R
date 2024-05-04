library(tidyverse)
library(readr)
library(dplyr)
library(here)

shooting_data <- read.csv(here::here("dataset", "BostonShootingDataClean.csv"))
shooting_data <- mutate(shooting_data, year = year(Date))

df_shooting <- as.data.frame(shooting_data) %>%
  filter(victim_race != "")

df_shooting <- df_shooting %>%
  mutate(victim_race = ifelse(v_hispanic_or_latinx == TRUE, "Hispanic or Latinx", victim_race))
#View(df_shooting)  

#Fatal Shootings by Race over Years faceted by District
props <- df_shooting %>%
  filter(fatal == "TRUE") %>%
  group_by(victim_race, year, district_name) %>%
  summarize(count = n())
props$yearSolid <- as.integer(props$year)

#View(props)

ggplot(props, aes(x = yearSolid, y = count, color = victim_race)) +
  geom_line() +
  labs(title = "Fatal Shootings by Race over Years by District", x = "Year", y = "Fatalities") + 
  scale_color_discrete(name="Race") +
  scale_x_continuous(breaks = unique(props$yearSolid))+ 
  facet_wrap(~district_name)


#Non Fatal Shootings by Race over Years faceted by District
props2 <- df_shooting %>%
  filter(fatal == "FALSE") %>%
  group_by(victim_race, year, district_name) %>%
  summarize(count = n())
props2$yearSolid <- as.integer(props2$year)

#View(props2)

ggplot(props2, aes(x = yearSolid, y = count, color = victim_race)) +
  geom_line() +
  labs(title = "Non Fatal Shootings by Race over Years by District", x = "Year", y = "Non Fatal Incidents") + 
  scale_color_discrete(name="Race") +
  scale_x_continuous(breaks = unique(props2$yearSolid))+
  facet_wrap(~district_name)



#Fatal Shootings by Race over Years Unfaceted
props3 <- df_shooting %>%
  filter(fatal == "TRUE") %>%
  group_by(victim_race, year) %>%
  summarize(count = n())
props3$yearSolid <- as.integer(props3$year)

#View(props)

ggplot(props3, aes(x = yearSolid, y = count, color = victim_race)) +
  geom_line() +
  labs(title = "Fatal Shootings by Race over Years", x = "Year", y = "Fatalities") + 
  scale_color_discrete(name="Race") +
  scale_x_continuous(breaks = unique(props3$yearSolid))



#Non Fatal Shootings by Race over Years Unfaceted
props4 <- df_shooting %>%
  filter(fatal == "FALSE") %>%
  group_by(victim_race, year) %>%
  summarize(count = n())
props4$yearSolid <- as.integer(props4$year)

#View(props4)

ggplot(props4, aes(x = yearSolid, y = count, color = victim_race)) +
  geom_line() +
  labs(title = "Non Fatal Shootings by Race over Years", x = "Year", y = "Non Fatal Incidents") + 
  scale_color_discrete(name="Race") +
  scale_x_continuous(breaks = unique(props4$yearSolid))


