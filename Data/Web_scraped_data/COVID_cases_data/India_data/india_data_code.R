##India Data

##Including all the required R packages
library(rvest)
library(tidyverse)

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
india.data <- data.frame(name, total, cured, deaths)   ##Creating a dataframe
##india.data

save(india.data, file = "india_data.Rdata")