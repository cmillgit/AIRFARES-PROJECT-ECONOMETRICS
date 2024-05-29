library(tidyverse)
library(magrittr)
library(modelsummary)
library(tsibble)
library(estimatr)
library(readr)
library(broom)
library(readr)
library(plm)
library(clubSandwich)
library(lubridate)

df <- read_csv("Project Data/Consumer_Airfare_Report__Table_1_-_Top_1_000_Contiguous_State_City-Pair_Markets_20231018.csv")

df %<>% mutate(quarter = as.factor(quarter),
               Year = as.factor(Year),
               farediff = (fare_lg - fare_low))

df1 <- df %>% select(-Geocoded_City1, -Geocoded_City2, -table_1_flag, -farediff, -city1, -city2, -carrier_lg, -carrier_low, -citymarketid_1, -citymarketid_2)
df1 %<>% rename(distance = nsmiles, price = fare)

df1 %>% datasummary_skim(histogram = FALSE)
df1 %>% datasummary_skim(histogram = FALSE,
                         output = "RoughDraftData2.docx")

est <- lm_robust(log(price) ~ log(passengers) + log(distance) + large_ms + lf_ms + fare_lg + fare_low + quarter, data = df1)
modelsummary(est)




modelsummary(est, stars = TRUE,
             output = "RoughRegression2.docx")

