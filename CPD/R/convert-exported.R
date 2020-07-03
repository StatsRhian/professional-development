library(tidyverse)
library(lubridate)
library(stringr)
library(yaml)
library(glue)

# Functions

create_years = function(path = ".", years = 2017:2020){
  for (y in seq_along(years)) {
    if (!dir.exists(glue("{path}/{years[y]}"))) {
      dir.create(glue("{path}/{years[y]}"))
    }
  }
}


activities = readr::read_csv("data/raw/review_activities_report.csv")
benefit = readr::read_csv("data/raw/benefit_report.csv")

# Replace NA
benefit$`Activity Description`[(is.na(benefit$`Activity Description`))] = "ASDF123"

# Remove accidental dups
activities =
  activities %>%
  distinct() %>%
  filter(`Start Date` != "09/01/2020 00:00") %>%
  filter(`Start Date` != "26/04/2019 12:30")

benefit =
  benefit %>%
  distinct()

# Duplicate fixes:

# Lancs Data Sci
activities$Title[activities$`Start Date` == "06/06/2017 18:00"] = "Lancaster Data Science Meetup MB"
benefit$Title[str_sub(benefit$`Benefit to Practice`, 1,4) == "Matt"] = "Lancaster Data Science Meetup MB"

# R Ladies
activities$Title[activities$`Start Date` == "25/01/2018 18:00"] = "R Ladies Jan"
benefit$Title[str_sub(benefit$`Activity Description`, 1, 26) == "Using a real-life data set"] = "R Ladies Jan"

# MancML
activities$Title[activities$`Start Date` == "24/05/2018 18:00"] = "Manc ML May"
benefit$Title[str_sub(benefit$`Activity Description`, 1, 13) == "@DreamAgility"] = "Manc ML May"

# Manchester R
activities$Title[activities$`Start Date` == "01/08/2017 19:00"] = "Manchester R August"
benefit$Title[str_sub(benefit$`Activity Description`, 1, 13) == "19:05 - 19:35"] = "Manchester R August"

activities$Title[activities$`Start Date` == "09/11/2017 19:00"] = "Manchester R November"
benefit$Title[str_sub(benefit$`Benefit to Practice`, 1, 23) == "Martin gave an in-depth"] = "Manchester R November"

activities$Title[activities$`Start Date` == "15/05/2018 18:00"] = "Manchester R May"
benefit$Title[str_sub(benefit$`Activity Description`, 1, 12) == "6pm Tutorial"] = "Manchester R May"


# Join data

df =
  dplyr::full_join(activities, benefit) %>%
  janitor::clean_names() %>%
  mutate(start_date = as_date(dmy_hm(start_date))) %>%
  mutate(end_date = as_date(dmy_hm(end_date))) %>%
  mutate(activity_url = "") %>%
  mutate(tags = list("")) %>%
  relocate(title, activity_type, start_date, end_date, activity_url, learning_hours, tags) %>%
  mutate(across(everything(), ~str_replace_all(., ":", ""))) %>%
  mutate(across(everything(), ~str_replace_all(., '"', ""))) %>%
  mutate(across(everything(), ~str_replace_all(., "#", ""))) %>%
  mutate(across(everything(), ~str_replace_all(., ">", ""))) %>%
  mutate(across(everything(), ~str_replace_all(., "<", ""))) %>%
  mutate(across(everything(), ~str_replace_all(., "\\r", ""))) %>%
  mutate(across(everything(), ~str_replace_all(., "\\n", ""))) %>%
  mutate(across(everything(), ~str_replace_all(., "\\*", ""))) %>%
  mutate(across(-c(start_date, end_date), ~str_replace_all(., "-", ""))) %>%
  mutate(path = glue::glue("{year(start_date)}/{year(start_date)}-{str_pad(month(start_date), width = 2, pad = 0)}-{str_pad(day(start_date), width = 2, pad = 0)}.md")) %>%
  nest(data = -path)

#str-remove
create_years(path = "data")

map2(.x = df$path, .y = df$data,
       ~write_yaml(.y, file = glue("data/{.x}")))

