##########
# title: Time Plots of COVID19 England
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
library(ggrepel)
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
    sprintf("areaType=%s", "region"),
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
covid <- 
  this_interval_data  %>%
  mutate(
    covid19_week_ending = date %>% 
      lubridate::ymd() %>% 
      lubridate::ceiling_date(unit = "week") %>%
      `-` (lubridate::days(1)) %>%
      format('%Y-%m-%d')
  ) %>%
  group_by(name, code, covid19_week_ending) %>%
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

covid$date <- as.Date(covid$covid19_week_ending)
covid_sel <- covid %>% dplyr::filter( date >= as.Date("2020-09-01"))

tsp <- ggplot(data = covid_sel, mapping = aes(x = date, y = seven_day_rate_newCasesBySpecimenDate,
                            group = name, colour = name))
tsp1 <- tsp + geom_smooth(method = "loess", se = F, size=2, span = 0.3) +
  geom_text(data = covid_sel %>% 
            dplyr::filter(date == last(date)),
            fontface = "bold", 
            size = 8,
            check_overlap = TRUE,
    aes(label = name, 
        x = date + 1, 
        y = seven_day_rate_newCasesBySpecimenDate, 
        colour = name)) +
  scale_colour_viridis_d(option = "plasma") +
  labs(title= paste(" "), x="Date", y="New Cases per 100,000") +
  theme_tufte() +
  theme(text = element_text(family="robotocondensed",
        size = 22)) +
  theme(legend.position = "none")
#  theme(plot.title=element_text(size = 20)) +
#  theme(axis.text=element_text(size=16)) +
#  theme(axis.title.y = element_text(size = 18)) +
#  theme(axis.title.x = element_text(size = 18)) +
#  theme(plot.subtitle=element_text(size = 16)) +
#  theme(axis.title=element_text(size=20, face="plain"))


png("./output/covid_reg.png", units="in", width=16, height=12, res=300)
tsp1
dev.off()


tsp2 <- tsp + geom_smooth(method = "loess", se = F, size=2, span = 0.3) +
  scale_colour_viridis_d(option = "viridis") +
  geom_text(x = as.Date("2020-10-05"), y = 400, label="North West", fontface = "bold", size = 8, colour = "#21908CFF") +
  geom_text(x = as.Date("2020-10-15"), y = 350, label="North East", fontface = "bold", size = 8, colour = "#2C728EFF") +
  geom_text(x = as.Date("2020-12-10"), y = 350, label="London", fontface = "bold", size = 8, colour = "#3B528BFF") +
  geom_text(x = as.Date("2020-10-20"), y = 270, label="East Midlands", fontface = "bold", size = 8, colour = "#440154FF") +
  geom_text(x = as.Date("2020-11-01"), y = 220, label=" West Midlands", fontface = "bold", size = 8, colour = "#AADC32FF") +
  geom_text(x = as.Date("2020-12-10"), y = 60, label="South West", fontface = "bold", size = 8, colour = "#5DC863FF") +
  geom_text(x = as.Date("2020-12-10"), y = 256, label="South East", fontface = "bold", size = 8, colour = "#27AD81FF") +
  geom_text(x = as.Date("2020-12-10"), y = 240, label="East England", fontface = "bold", size = 8, colour = "#472D7BFF") +
  labs(title= paste(" "), x="Date", y="New Cases per 100,000") +
  theme_tufte() +
  theme(text = element_text(family="robotocondensed",
                            size = 30)) +
  theme(legend.position = "none")


png("./output/covid_reg2.png", units="in", width=16, height=12, res=300)
tsp2
dev.off()
