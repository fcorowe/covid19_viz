##########
# title: Heat map of COVID19 United Kingdom
# Author: Francisco Rowe
# Date: 30 Dec 2020
# Data source: https://coronavirus.data.gov.uk/developers-guide
##########

# clean workspace
rm(list = ls())

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(magrittr)
library(httr)
library(jsonlite)
library(rgdal)
library(tmap)
library(sf)
library(gifski)
library(viridis)
library(ggthemes)
library(showtext)

# Government API ----------------------------------------------------------

# Coronavirus (COVID-19) in the UK
# API endpoint
endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"

# Data structure to request
structure <- list(
  date       = "date", 
  name       = "areaName", 
  code       = "areaCode", 
  cumCasesBySpecimenDate = "cumCasesBySpecimenDate",
  cumCasesBySpecimenDateRate = "cumCasesBySpecimenDateRate",
  newCasesBySpecimenDate = "newCasesBySpecimenDate"
)

# Load spatial data -------------------------------------------------------

# Read la boundaries 
lower_tier_la <- st_read("boundaries/Local_Authority_Districts__December_2019__Boundaries_UK_BFC.shp")

# Main --------------------------------------------------------------------
start_date <- "2020-03-01"
end_date <- "2020-12-29"

start_date <- start_date %>% lubridate::ymd() %>% lubridate::floor_date(unit = "week")
end_date <- end_date %>% lubridate::ymd() %>% lubridate::floor_date(unit = "week") - lubridate::days(1)
this_date <- start_date

this_interval_data <- NA
this_interval_srt <- 
  paste(
    start_date %>% format('%Y%m%d'),
    end_date %>% format('%Y%m%d'),
    sep = "_"
  )

# Obtaining COVID19 data via API ----------------------------------------------------------
while (this_date <= end_date) {
  
  # date to string format
  date_to_request_str <- this_date %>% 
    format('%Y-%m-%d')
  cat("Requesting:", date_to_request_str, "\n")
  
  # Set filters
  #   - type of area
  #   - date
  filters <- c(
    sprintf("areaType=%s", "ltla"),
    sprintf("date=%s", date_to_request_str)
  )
  
  # Request
  httr::GET(
    url   = endpoint,
    query = list(
      filters   = paste(filters, collapse = ";"),
      structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
      latestBy  = "newCasesByPublishDate",
      format    = "json"
    ),
    timeout(10)
  ) -> response
  
  # Check response status
  if (response$status_code >= 400) {
    err_msg = httr::http_status(response)
    stop(err_msg)
  }
  
  # Extract response from json
  # and append to csv
  this_date_data <-
    response %>%
    httr::content("text", encoding="UTF-8") %>%
    jsonlite::fromJSON() %$%
    data
  
  # Instantiate the dataset if it doesn't exist
  # or append the data
  if (is.na(this_interval_data)) {
    this_interval_data <- this_date_data
  } else {
    this_interval_data <- 
      rbind(
        this_interval_data,
        this_date_data
      )
  }
  
  # Next date
  this_date <- this_date + lubridate::days(1)

}



# Seven–day rolling rate of new cases by specimen -------------------------
coronavirus_cases <- 
  this_interval_data  %>%
  rename(
    covid19_date = date,
    covid19_area_name = name,
    covid19_area_code = code
  ) %>%
  mutate(
    covid19_week_ending = covid19_date %>% 
      lubridate::ymd() %>% 
      lubridate::ceiling_date(unit = "week") %>%
      `-` (lubridate::days(1)) %>%
      format('%Y-%m-%d')
  ) %>%
  group_by(covid19_area_name, covid19_area_code, covid19_week_ending) %>%
  summarise(
    max_cumCasesBySpecimenDate = max(cumCasesBySpecimenDate),
    max_cumCasesBySpecimenDateRate = max(cumCasesBySpecimenDateRate),
    sum_newCasesBySpecimenDate = sum(newCasesBySpecimenDate)
  ) %>%
  mutate(
    area_population = (max_cumCasesBySpecimenDate / max_cumCasesBySpecimenDateRate) * 100000
  ) %>%
  mutate(
    seven_day_rate_newCasesBySpecimenDate = (sum_newCasesBySpecimenDate / area_population) * 100000
  )

# Plotting -----------------------------------------------------------------

# load font
font_add_google("Roboto Condensed", "robotocondensed")
# automatically use showtext to render text
showtext_auto()

coronavirus_cases$date <- as.Date(coronavirus_cases$covid19_week_ending)

hm <- coronavirus_cases %>% dplyr::filter(area_population > 250000) %>% 
  ggplot(., mapping = aes(x= date, y= reorder(covid19_area_name, sum_newCasesBySpecimenDate, .fun='median'), fill= seven_day_rate_newCasesBySpecimenDate)) +
  geom_tile() +
  scale_fill_viridis(name="New Cases per 100,000", option ="plasma", begin = 0, end = 1, direction = 1) +
  theme_tufte() + 
  theme(text = element_text(family="robotocondensed")) +
  labs(title= paste(" "), x="Date", y="Lower Tier Authority Area") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text(size=15)) +
  theme(axis.text.y = element_text(size=10)) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.title=element_text(size=20, face="plain")) +
  theme(legend.key.width = unit(4, "cm"), legend.key.height = unit(.9, "cm"))

png("/Users/franciscorowe/Dropbox/Francisco/Research/github_projects/covid_fr/covid19_viz/maps/output/covid_heatmap_lta.png", units="in", width=16, height=12, res=300)
hm
dev.off()
