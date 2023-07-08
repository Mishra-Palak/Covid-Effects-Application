library(tidyverse)
library(rvest)
library(tibble)
library(ggplot2)

html <- read_html("https://www.worldometers.info/coronavirus/")
html1 <- html %>% html_table()
df <- data.frame(html1)

continents <- df[1:6,]
countries <- df[9:238, ]

world <- df[8, ]
world[,3] <- as.numeric(gsub(",", "", world[,3]))
world[,5] <- as.numeric(gsub(",", "", world[,5]))
world[,7] <- as.numeric(gsub(",", "", world[,7]))
world[,9] <- as.numeric(gsub(",", "", world[,9]))

## country wise
country_names <- countries[,2]
total_cases_of_countries <- countries[,3]
total_cases_of_countries <- as.numeric(gsub(",", "", total_cases_of_countries))
total_deaths1 <- countries[,5]
total_deaths1 <- as.numeric(gsub(",", "", total_deaths1))
total_recovered1 <- countries[,7]
total_recovered1 <- as.numeric(gsub(",", "", total_recovered1))
active_cases1 <- countries[,9]
active_cases1 <- as.numeric(gsub(",", "", active_cases1))
total_population1<- countries[,15]
total_population1 <- as.numeric(gsub(",", "", total_population1))

covid_effects_across_countries <- data.frame(country_names, total_cases_of_countries, total_deaths1, total_recovered1, active_cases1, total_population1)
save(covid_effects_across_countries, file = "countries_data.Rdata")

## continent wise
continent_names <- continents[,2]
total_cases_of_continents <- continents[,3]
total_cases_of_continents <- as.numeric(gsub(",", "", total_cases_of_continents))
total_deaths2 <- continents[,5]
total_deaths2 <- as.numeric(gsub(",", "", total_deaths2))
total_recovered2 <-continents[,7]
total_recovered2 <- as.numeric(gsub(",", "", total_recovered2))
active_cases2 <- continents[,9]
active_cases2 <- as.numeric(gsub(",", "", active_cases2))

covid_effects_across_continents <- data.frame(continent_names = c("World", continent_names), total_cases_of_continents = c(world[,3], total_cases_of_continents), total_deaths2 = c(world[,5], total_deaths2), total_recovered2 = c(world[,7], total_recovered2), active_cases2 = c(world[,9], active_cases2))
save(covid_effects_across_continents, file = "continents_data.Rdata")

