#Code to create datasets to work with in shiny app
library(lubridate)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)

#Import basic accident and population files
a24 <- read.csv("FARS2024NationalCSV/FARS2024NationalCSV/accident.csv")
a23 <- read.csv("Traffic_accidents_FARS2023NationalCSV/FARS2023NationalCSV/accident.csv")
pop <- read.csv("NST-EST2025-ALLDATA.csv")

#View data
View(a24)
View(a23)
View(pop)

#Clean and view population data
clean_pop <- pop |>
  filter(STATE != "00", !REGION %in% c("0", "X")) |>
  select(STATE, NAME, REGION, starts_with("POPESTIMATE")) |>
  pivot_longer(cols = starts_with("POPESTIMATE"), names_to = "Year", values_to = "Population") |>
  mutate(Year = as.numeric(str_remove(Year, "POPESTIMATE")), 
         REGION = case_match(as.character(REGION), "1" ~ "Northeast", "2" ~ "Midwest", "3" ~ "South", "4" ~ "West"))
clean_pop <- clean_pop |>
  mutate(STATE = as.integer(STATE))
View(clean_pop)

#Import different datasets
distract24 <- read.csv("FARS2024NationalCSV/FARS2024NationalCSV/distract.csv")
distract23 <- read.csv("Traffic_accidents_FARS2023NationalCSV/FARS2023NationalCSV/distract.csv")
impair24 <- read.csv("FARS2024NationalCSV/FARS2024NationalCSV/drimpair.csv")
impair23 <- read.csv("Traffic_accidents_FARS2023NationalCSV/FARS2023NationalCSV/drimpair.csv")

#Clean accident data
clean_a24 <- a24 |>
  filter(HOUR != 99) |>
  mutate(time_of_day = case_when(
    HOUR >= 6 & HOUR <= 11 ~ "Morning",
    HOUR >= 12 & HOUR <= 18 ~ "Afternoon",
    TRUE ~ "Night"
  )) |>
  select(STATE, STATENAME, ST_CASE, time_of_day, FATALS, RUR_URBNAME, WEATHERNAME, DAY_WEEKNAME, MONTHNAME)

clean_a23 <- a23 |>
  filter(HOUR != 99) |>
  mutate(time_of_day = case_when(
    HOUR >= 6 & HOUR <= 11 ~ "Morning",
    HOUR >= 12 & HOUR <= 18 ~ "Afternoon",
    TRUE ~ "Night"
  )) |>
  select(STATE, STATENAME, ST_CASE, time_of_day, FATALS, RUR_URBNAME, WEATHERNAME, DAY_WEEKNAME, MONTHNAME)

View(clean_a24)
#Accidents and fatalities by state
total_by_state24 <- clean_a24 |>
  group_by(STATE, STATENAME, time_of_day) |>
  summarise(
    total_crashes = n(),
    total_fatalities = sum(FATALS, na.rm = TRUE),
    .groups = "drop"
  )

total_by_state23 <- clean_a23 |>
  group_by(STATE, STATENAME, time_of_day) |>
  summarise(
    total_crashes = n(),
    total_fatalities = sum(FATALS, na.rm = TRUE),
    .groups = "drop"
  )

#Clean Distracted data
clean_d24 <- distract24 |>
  filter(!DRDISTRACTNAME %in% c(
    "Not Distracted",
    "Not Reported",
    "Reported as Unknown if Distracted",
    "No Driver Present/Unknown if Driver present"
  )) |>
  select(STATE, STATENAME, ST_CASE) |>
  distinct()

clean_d23 <- distract23 |>
  filter(!DRDISTRACTNAME %in% c(
    "Not Distracted",
    "Not Reported",
    "Reported as Unknown if Distracted",
    "No Driver Present/Unknown if Driver present"
  )) |>
  select(STATE, STATENAME, ST_CASE) |>
  distinct()

#Distracted accidents and fatalities by state
d_state24 <- clean_a24 |>
  inner_join(clean_d24, by = c("STATE", "STATENAME", "ST_CASE")) |>
  group_by(STATE, STATENAME, time_of_day) |>
  summarise(
    distracted_crashes = n(), 
    distracted_fatalities = sum(FATALS, na.rm = TRUE),
    .groups = "drop"
    )

d_state23 <- clean_a23 |>
  inner_join(clean_d24, by = c("STATE", "STATENAME", "ST_CASE")) |>
  group_by(STATE, STATENAME, time_of_day) |>
  summarise(
    distracted_crashes = n(), 
    distracted_fatalities = sum(FATALS, na.rm = TRUE),
    .groups = "drop"
  )

View(impair24)
#Clean Impaired data
clean_i24 <- impair24 |>
  filter(DRIMPAIR == "9") |>
  select(STATE, STATENAME, ST_CASE) |>
  distinct()

clean_i23 <- impair23 |>
  filter(DRIMPAIR == "9") |>
  select(STATE, STATENAME, ST_CASE) |>
  distinct()

#Impaired accidents and fatalities by state
i_state24 <- clean_a24 |>
  inner_join(clean_i24, by = c("STATE", "STATENAME", "ST_CASE")) |>
  group_by(STATE, STATENAME, time_of_day) |>
  summarise(
    impaired_crashes = n(),
    impaired_fatalities = sum(FATALS, na.rm = TRUE),
    .groups = "drop"
  )

i_state23 <- clean_a23 |>
  inner_join(clean_i23, by = c("STATE", "STATENAME", "ST_CASE")) |>
  group_by(STATE, STATENAME, time_of_day) |>
  summarise(
    impaired_crashes = n(),
    impaired_fatalities = sum(FATALS, na.rm = TRUE),
    .groups = "drop"
  )

#Combine to one dataset
combined_state24 <- total_by_state24 |> 
  left_join(d_state24, by = c("STATE", "STATENAME", "time_of_day")) |>
  left_join(i_state24, by = c("STATE", "STATENAME", "time_of_day")) |>
  mutate(
    distracted_crashes = ifelse(is.na(distracted_crashes), 0, distracted_crashes),
    distracted_fatalities = ifelse(is.na(distracted_fatalities), 0, distracted_fatalities),
    impaired_crashes = ifelse(is.na(impaired_crashes), 0, impaired_crashes),
    impaired_fatalities = ifelse(is.na(impaired_fatalities), 0, impaired_fatalities),
    pct_distracted = distracted_crashes / total_crashes * 100,
    pct_impaired = impaired_crashes / total_crashes * 100
  )

combined_state23 <- total_by_state23 |> 
  left_join(d_state23, by = c("STATE", "STATENAME", "time_of_day")) |>
  left_join(i_state23, by = c("STATE", "STATENAME", "time_of_day")) |>
  mutate(
    distracted_crashes = ifelse(is.na(distracted_crashes), 0, distracted_crashes),
    distracted_fatalities = ifelse(is.na(distracted_fatalities), 0, distracted_fatalities),
    impaired_crashes = ifelse(is.na(impaired_crashes), 0, impaired_crashes),
    impaired_fatalities = ifelse(is.na(impaired_fatalities), 0, impaired_fatalities),
    pct_distracted = distracted_crashes / total_crashes * 100,
    pct_impaired = impaired_crashes / total_crashes * 100
  )
View(combined_state23)  

#Incorporate Population
pop_24 <- clean_pop |>
  filter(Year == 2024) |>
  select(STATE, NAME, REGION, Population)

pop_23 <- clean_pop |>
  filter(Year == 2023) |>
  select(STATE, NAME, REGION, Population)

combined_state24 <- combined_state24 |>
  left_join(pop_24, by = "STATE") |>
  mutate(
    total_rate = total_crashes / Population * 100000,
    distracted_rate = distracted_crashes / Population * 100000,
    impaired_rate = impaired_crashes / Population * 100000,
    fatality_rate = total_fatalities / Population * 100000
  )

combined_state23 <- combined_state23 |>
  left_join(pop_24, by = "STATE") |>
  mutate(
    total_rate = total_crashes / Population * 100000,
    distracted_rate = distracted_crashes / Population * 100000,
    impaired_rate = impaired_crashes / Population * 100000,
    fatality_rate = total_fatalities / Population * 100000
  )


#Save files
saveRDS(combined_state24, "accidents_24.rds")
saveRDS(combined_state23, "accidents_23.rds")
