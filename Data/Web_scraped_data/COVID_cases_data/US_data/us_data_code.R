##US Data

library(rvest)
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
df = data.frame(tables[2:6], tables[11])
save(df, file = "us_data.Rdata")