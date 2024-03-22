
library(tidyverse)

shooting_data <- read_csv(here::here("dataset", "BostonShootingData.csv"))

## CLEAN the data
shooting_data_clean <- shooting_data

write_csv(shooting_data_clean, file = here::here("dataset", "BostonShootingDataClean.csv"))

save(shooting_data_clean, file = here::here("dataset/BostonShootingDataClean.RData"))
