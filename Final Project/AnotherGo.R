library(tidyverse)
library(tidyr)
library(magrittr)
library(modelsummary)
library(scales)
library(estimatr)
library(readr)
library(broom)
library(readr)
library(plm)
library(clubSandwich)
library(lubridate)
library(ggplot2)
library(lmtest)
library(sandwich)
library(stargazer)

#Load the data 
airfares_large <- read_csv("C:/Users/cmill/Downloads/airfares_large.csv")
airfares_large <- as_tibble(airfares_large)
View(airfares_large)

fuel_cost <- read_csv("Project Data/Fuel_cost.csv")
fuel_cost <- as_tibble(fuel_cost)
fuel_cost <- fuel_cost %>%
  separate(Month, into = c("Month", "Year"), sep = "-")

fuel_cost$Month <- match(fuel_cost$Month, c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

fuel_cost$Year <- ifelse(as.numeric(fuel_cost$Year) > 22,
                         as.numeric(fuel_cost$Year) + 1900, 
                         as.numeric(fuel_cost$Year) + 2000)

fuel_cost %<>% rename(fuel = `U.S. Kerosene-Type Jet Fuel Retail Sales by Refiners Dollars per Gallon`) 
fuel_cost %<>% mutate(lfuel = log(fuel))

fuel_cost <- fuel_cost %>%
  mutate(quarter = case_when(
    Month %in% 1:3  ~ "1",
    Month %in% 4:6  ~ "2",
    Month %in% 7:9  ~ "3",
    Month %in% 10:12 ~ "4"
  ))

fuel_cost <- fuel_cost %>%
  group_by(Year, quarter) %>%
  summarize(avgfuel = mean(fuel, na.rm = TRUE),
            lavgfuel = mean(lfuel, na.rm = TRUE))
fuel_cost %<>% rename(year = Year)
View(fuel_cost)

#Drop unimportant variables
airfares_large <- airfares_large %>% select(-tbl, -tbl6pk, -Geocoded_City1, -Geocoded_City2)

#Rename columns for clarity
airfares_large %<>% rename(top1000 = table_1_flag)

#Create a panel identifier that combines cross-sectional unit (citymarketIDs)
airfares_large %<>% 
  mutate(panel_id = interaction(citymarketid_1, citymarketid_2))

#Change data types
airfares_large %<>% mutate(quarter = as.factor(quarter),
            year = as.factor(Year),
            citymarketid_1 = as.factor(citymarketid_1),
            citymarketid_2 = as.factor(citymarketid_2),
            panel_id = as.factor(panel_id)
)
airfares_large %<>% mutate(top1000 = as.factor(top1000))



#Sort the data by the panel identifier and then by the time variables
airfares_large %<>% 
  arrange(panel_id, year, quarter)
airfares_large %>% group_by(panel_id) %>%
  summarize(start_year = first(year), end_year = last(year))
head(airfares_large)
airfares_large %<>% select(-year)
airfares_large %<>% rename(year = Year)

#Merge Fuel and Airfare Datasets
airfares_merged <- airfares_large %>%
  left_join(fuel_cost, by = c("year", "quarter"))

airfares_merged %<>% 
  mutate(lfare = log(fare),
         lpassengers = log(passengers))

airfares_merged %<>%
  mutate(covid = as.factor(dummy_covid),
         d911 = as.factor(dummy_911),
         grec = as.factor(grecession))

view(airfares_merged)

#Create Dummies for Significant Events
airfares_merged <- airfares_merged %>%
  mutate(dummy_covid = ifelse((year == 2020 | (year == 2021 & quarter == "1")), 1, 0))
airfares_merged <- airfares_merged %>%
  mutate(dummy_911 = ifelse((year == 2001 & quarter %in% c("3", "4")) |
                              (year == 2002) |
                              (year == 2003 & quarter %in% c("1", "2", "3")), 1, 0))
airfares_merged <- airfares_merged %>%
  mutate(grecession = ifelse((year == 2007 & quarter %in% "4") |
                              (year == 2008) |
                              (year == 2009 & quarter %in% c("1", "2")), 1, 0))



#Summary Statistics
summary(airfares_large)
airfares_large %>% datasummary_skim(histogram = FALSE)

airfares_merged %>% datasummary_skim(histogram = FALSE)

selected_vars <- airfares_merged[, c("fare", "nsmiles", "passengers", "lf_ms", "avgfuel")]
selected_vars %>% datasummary_skim(histogram = FALSE)

overview_data <- airfares_merged %>%
  distinct(citymarketid_1, citymarketid_2) %>%
  nrow()
total_observations <- nrow(airfares_merged)
overview.2 <- tibble(
  "Unique City-Pair Markets" = overview_data,
  "Total Observations"= total_observations
)
avgobs <- total_observations/overview_data
overview.2 <- overview.2 %>%
  add_row("Average Observations per City-Pair Market" = avgobs)

#Create Visualizations 

#Average Fares Over Time
avg_fares <- airfares_large %>%
  group_by(year) %>%
  summarize(avg_fare = mean(fare, na.rm = TRUE))
view(avg_fares)


ggplot(avg_fares, aes(x = year, y = avg_fare)) +
  geom_line() +
  scale_x_continuous(limits = c(1996, NA), 
                     breaks = seq(1996, 2022, by = 4)) +
  scale_y_continuous(limits = c(100, 300), breaks = seq(100, 300, by = 50)) +
  labs(title = "Average Fares Over Time",
       x = "Year",
       y = "Average Fare ($)") 

#Passengers Over Time
time_passengers <- airfares_large %>%
  group_by(year) %>%
  filter(year < 2023) %>%
  summarize(sum_passengers = sum(passengers, na.rm = TRUE))
view(time_passengers)

ggplot(time_passengers, aes(x = year, y = sum_passengers)) +
  geom_line() +
  scale_x_continuous(limits = c(1996, NA), 
                     breaks = seq(1996, 2022, by = 4)) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"),
                     limits = c(0, 6000000), breaks = seq(0, 6000000, by = 1000000)) +
  labs(title = "Passengers by Year",
       x = "Year",
       y = "Total Passengers (Millions)") 
#Quarterly Passengers Between 2019 and Q3 2023
quarterly_passengers <- airfares_large %>%
  group_by(year, quarter) %>%
  filter(year >= 2019) %>%
  summarize(passengers = sum(passengers, na.rm = T))

quarterly_passengers %<>% 
  mutate(quarter = as.numeric(quarter))
quarterly_passengers %<>%
  mutate(timecont = year + (quarter - 1) / 4)

view(quarterly_passengers)

ggplot(quarterly_passengers, aes(x = timecont, y = passengers)) +
  geom_line() +
 scale_x_continuous(breaks = floor(min(quarterly_passengers$timecont)):ceiling(max(quarterly_passengers$timecont)),
                    labels = floor(min(quarterly_passengers$timecont)):ceiling(max(quarterly_passengers$timecont))) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(title = "Passengers (2019 Q1 - 2023 Q3)",
       x = "Year", 
       y = "Total Passengers (Millions)") +
  theme(axis.text.x = element_text(hjust = 0.5))

#Fuel Costs Over Time
fuel_year <- fuel_cost %>% 
  summarize(fuel = mean(avgfuel))
ggplot(fuel_year, aes(x = year, y = fuel)) +
  geom_line() +
  scale_x_continuous(limits = c(1996, NA), 
                     breaks = seq(1996, 2022, by = 4)) +
  labs(title = "Average Jet Fuel Costs Since 1996", 
       x = "Year",
       y = "Price (Dollars per Gallon)")


#Model Time

df.within <- airfares_merged %>% select(panel_id, year, quarter, fare) %>%
  group_by(panel_id) %>%
  summarize(farebar = mean(fare))
min(df.within$farebar)
max(df.within$farebar)


est.fe <- plm(lfare ~ d911 + grec + covid + lpassengers + lf_ms + lavgfuel,
              data = airfares_merged,
              index = c("panel_id", "year", "quarter"),
              model = "within")

clustered_se <- vcovHC(est.fe, method = "arellano", type = "HC1", cluster = "group")
coeftest(est.fe, vcov = clustered_se)

modelsummary(est.fe, stars = T)

tidy(robust_se)

est.re <- plm(lfare ~ d911 + grec + covid + lpassengers + lf_ms + lavgfuel + covid*lpassengers,
              data = airfares_merged,
              index = c("panel_id", "year", "quarter"),
              model = "random")
modelsummary(est.re, stars = T)

hausman_test <- phtest(est.fe, est.re)
print(hausman_test)











