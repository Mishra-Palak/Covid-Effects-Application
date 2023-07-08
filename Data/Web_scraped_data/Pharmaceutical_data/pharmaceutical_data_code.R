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

save(ph, file = "pharmaceutical_data.Rdata")