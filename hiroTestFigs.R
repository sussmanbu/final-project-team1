library(tidyverse)
library(readr)
library(dplyr)
library(here)

shooting_data <- read.csv(here::here("dataset", "BostonShootingDataClean.csv"))
shooting_data <- mutate(shooting_data, year = year(Date))
#maybe change every year or so

#View(shooting_data)

#View(df_fbr)
#createa a fatality chart for each year
#make it a proportion value instead
#view how proportions change over the years.
#find out what the range of years actually is


#  test <- subset(shooting_data, year == 2016)
#  fatal_by_race <- table(test$victim_race, test$fatal)
  
#  df_fbr <- as.data.frame(fatal_by_race)
#  colnames(df_fbr) <- c("Race", "Fatal", "Freq")
#  ggplot(df_fbr, aes(x = Race, y = Freq, fill = Fatal)) + geom_bar(stat = "Identity") + labs(title = paste("Fatal Shootings Across Races ",as.character(num)), x="Race",y="Frequency", fill = "Fatal")+ theme_minimal() + theme(axis.text.x= element_text(angle=45, hjust = 1))

#facet wrap

ggplot(df_fbr)

