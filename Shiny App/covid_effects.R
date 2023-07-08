library(readxl)
library(dplyr)
library(tidyverse)
library(rvest)
library(ggplot2)
library(curl) 
library(tidyr)
library(scales)
library(gganimate)
library(zoo)
library(plotly)
library(reshape2)


## covid cases

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

# COUNTRY WISE
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

# CONTINENT WISE
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

#INDIA DATA
##This the web-scraping code:
html <- read_html("https://www.mygov.in/corona-data/covid19-statewise-status/")
covid <- html %>% html_elements(".field-item .even") %>% html_text()
all_data <- vector(mode = "character", length = 108)
name <- vector(mode = "character", length = 36)
total <- numeric(length = 36)
cured <- numeric(length = 36)
deaths <- numeric(length = 36)
idx <- 1
for (i in 1:length(covid)) {
  s <- substr(covid[i], 1, 1)
  if (s != 'h') {               ##Not including the non-required data (URL links) in the dataset
    all_data[idx] <- covid[i]
    idx <- idx + 1
  }
}
##all_data


for (i in 1:length(all_data)) { ##Assigning values to the elements of the parameter vectors depending on the pattern of their indices  
  if ((i-1) %% 8 == 0) {
    name[((i-1) / 8) + 1] <- all_data[i]
  }
  if ((i-2) %% 8 == 0) {
    total[((i-2) / 8) + 1] <- as.numeric(all_data[i])
  }
  if ((i-3) %% 8 == 0) {
    cured[((i-3) / 8) + 1] <- as.numeric(all_data[i])
  }
  if ((i-4) %% 8 == 0) {
    deaths[((i-4) / 8) + 1] <- as.numeric(all_data[i])
  }
}
##name
##total
##cured
##deaths

##Organizing and resizing the parameter vectors
name <- name[1:36]
total <- total[1:36]
cured <- cured[1:36]
deaths <- deaths[1:36]

html <- read_html("https://www.indiacensus.net/density.php")
population <- html %>% html_elements(".txt-right") %>% html_text()
population <- population[3:74]
final <- numeric(length = 36)
for (i in 1:length(final)) {
  final[i] <- population[2*i-1]
  final[i] <- as.numeric(gsub(",", "", final[i]))
}
final <- as.numeric(final)

india.data <- data.frame(StateName = name, TotalCases = total, TotalDeaths = deaths, TotalRecovered = cured, ActiveCases = (total - deaths - cured), Population = final)   ##Creating a dataframe


# US DATA

html =read_html("https://www.worldometers.info/coronavirus/country/us/")
tables = html %>% html_table
tables = tables[[1]]
tables = tables[-1,]
tables = tables[,-4]
tables = tables[,-5]
tables = tables[-c(53:64),]
tables = tables[,-c(12,13)]
for (i in 3:11) {
  tables[[i]] <- as.numeric(gsub(",", "", tables[[i]]))
}
us_data = data.frame(tables[2:6], tables[11])

## GDP
gdp <- read.csv("data/gdp_data.csv")
colnames(gdp)[1]<- "Country"
gdp_1 <- gdp %>% 
  pivot_longer(!Country,
               names_to = "year",
               values_to = "gdp") 
gdp_1[,2] <-  as.numeric(substring(gdp_1$year, 2,5))





## UNEMPLOYMENT 
unemployment <- read.csv("data/unemployment_data.csv")
colnames(unemployment)[1]<- "Country"
unemployment_1 <- unemployment %>%
  pivot_longer(!Country, names_to = "year",
               values_to = "unemployment")
unemployment_1[,2] <-as.numeric(substring(unemployment_1$year , 2, 5))
which(unemployment[,4] > unemployment[,5]) 


## TOURISM
tsm = read.csv("data/tsm.csv")
tsm = data.frame(tsm)
tsm = tsm[,-c(3,4)]

tsm = transform(tsm,date =zoo::as.Date(Date,frac = 0))

tsm = as_tibble(tsm)
dat <- unique(tsm$date)
tsm %>%
  ggplot(aes(x = date,y= Arrivals)) +
  geom_line(col = dat, lwd = 1.0) +
  labs(x = "Date", y = "Number of Tourist Arriavls", title = "Tourist Arrival with Month")+
  theme_classic() 



## PHARMA
ph = read_html("https://commerce.gov.in/about-us/divisions/export-products-division/export-products-pharmaceuticals/") %>% html_table()
ph = ph[[2]]
ph = ph[-1,]
ph =ph[,-5]
ph = ph[-12,]
ph = melt(ph)
ph =data.frame(ph)
ph = ph[1:10,]
ph = t(ph)
colnames(ph) <- ph[1,]
ph<- ph[2:4,]
ph <- melt(ph)
levels(ph$Var1) = c("2019-20","2020-21","2021-22")