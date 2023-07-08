library(shiny)

ui <- navbarPage(
  title = "COVID EFFECTS ",
  
  tabPanel("Covid Cases",
           titlePanel("COVID CASES"),
           fluidPage(
             
             navlistPanel(
               "Categories",
               tabPanel("WORLD",
                        fluidRow("TOTAL CASES vs TOTAL DEATHS", plotlyOutput("p.1")),hr(),
                        fluidRow("TOTAL CASES vs TOTAL RECOVERED", plotlyOutput("p.2")),hr(),
                        fluidRow("TOTAL CASES vs ACTIVE CASES", plotlyOutput("p.3")),hr(),
                        fluidRow("TOTAL CASES CONTINENT-WISE PERCENTAGE", plotOutput("p.4")),hr(),
                        fluidRow("TOTAL DEATHS CONTINENT-WISE PERCENTAGE", plotOutput("p.5")),hr(),
                        fluidRow("TOTAL RECOVERED CONTINENT-WISE PERCENTAGE", plotOutput("p.6")),hr(),
                        fluidRow("ACTIVE CASES CONTINENT-WISE PERCENTAGE", plotOutput("p.7"))),
               tabPanel("USA",
                        fluidRow("TOTAL CASES vs TOTAL DEATHS", plotlyOutput("p.8")),hr(),
                        fluidRow("TOTAL CASES vs TOTAL RECOVERED", plotlyOutput("p.9")),hr(),
                        fluidRow("TOTAL CASES vs ACTIVE CASES", plotlyOutput("p.10"))),
               tabPanel("INDIA",
                        fluidRow("TOTAL CASES vs TOTAL DEATHS", plotlyOutput("p.11")),hr(),
                        fluidRow("TOTAL CASES vs TOTAL RECOVERED", plotlyOutput("p.12")),hr(),
                        fluidRow("TOTAL CASES vs ACTIVE CASES", plotlyOutput("p.13")))
             )
           )
  ),
  tabPanel("GDP",
           titlePanel("COVID EFFECT ON GDP RATE"),
           
           fluidRow(
             column(6,
                    selectInput("country1", "Select Countries: ",
                                choices = gdp$Country,
                                multiple = TRUE,
                                width = "100%")),
             
             column(6,
                    selectInput("year1","Select year: ",
                                choices = unique(gdp_1$year),
                                multiple = TRUE,
                                width = "100%"))
           ),
           
           fluidRow(column(12,
                           actionButton("r", "Plot", class = "btn-block"))),
           
           fluidRow(column(8,plotOutput("distPlot1", brush = "plot_brush1")),
                    column(4,tableOutput("data1")))
  ),
  
  tabPanel("Unemployment",
           titlePanel("COVID EFFECT ON  UNEMPLOYMENT RATE "),
           
           fluidRow(
             column(6,
                    selectInput("country2", "Select Countries: ",
                                choices = unemployment$Country,
                                multiple = TRUE,
                                width = "100%")),
             
             column(6,
                    selectInput("year2","Select year: ",
                                choices = unique(unemployment_1$year),
                                multiple = TRUE,
                                width = "100%"))
           ),
           
           fluidRow(column(12,
                           actionButton("q", "Plot", class = "btn-block"))),
           
           fluidRow(column(8,plotOutput("distPlot2", brush = "plot_brush2")),
                    column(4,tableOutput("data2")))
  ),
  
  tabPanel("Tourism",
           titlePanel("COVID EFFECT ON TOURISM"),
           
           fluidPage(
             mainPanel(
               plotlyOutput("plot", width = "150%")
             )
           )),
  
  tabPanel("Pharmaceutical ",
           titlePanel("COVID EFFECT ON PHARMACEUTICAL EXPORTS(in million $)"),
           mainPanel(
             
             plotlyOutput("distplot4",  width = "150%")
           )
  )
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$p.1 <- renderPlotly({
    Countries = covid_effects_across_countries$country_names
    Population = covid_effects_across_countries$total_population1
    TotalDeaths = covid_effects_across_countries$total_deaths1
    TotalCases = covid_effects_across_countries$total_cases_of_countries
    TotalRecovered = covid_effects_across_countries$total_recovered1
    ActiveCases = covid_effects_across_countries$active_cases1
    
    p <- ggplot(covid_effects_across_countries, aes(x = TotalDeaths, y = TotalCases)) + geom_point(alpha = 0.7, aes(col = Countries, size = Population)) + xlab("TOTAL DEATHS") + ylab("TOTAL CASES")  + theme(legend.position = "none")
    p <- p + geom_smooth(method = 'lm', formula = y ~ x)
    p
  })
  
  output$p.2 <- renderPlotly({
    Countries = covid_effects_across_countries$country_names
    Population = covid_effects_across_countries$total_population1
    TotalDeaths = covid_effects_across_countries$total_deaths1
    TotalCases = covid_effects_across_countries$total_cases_of_countries
    TotalRecovered = covid_effects_across_countries$total_recovered1
    ActiveCases = covid_effects_across_countries$active_cases1
    
    p <- ggplot(covid_effects_across_countries, aes(x = TotalRecovered, y = TotalCases)) + geom_point(alpha = 0.7, aes(col = Countries, size = Population)) + xlab("TOTAL RECOVERED") + ylab("TOTAL CASES")  + theme(legend.position = "none")
    p <- p + geom_smooth(method = 'lm', formula = y ~ x)
    p
  })
  
  output$p.3 <- renderPlotly({
    Countries = covid_effects_across_countries$country_names
    Population = covid_effects_across_countries$total_population1
    TotalDeaths = covid_effects_across_countries$total_deaths1
    TotalCases = covid_effects_across_countries$total_cases_of_countries
    TotalRecovered = covid_effects_across_countries$total_recovered1
    ActiveCases = covid_effects_across_countries$active_cases1
    
    p <- ggplot(covid_effects_across_countries, aes(x = ActiveCases, y = TotalCases)) + geom_point(alpha = 0.7, aes(col = Countries, size = Population))  + xlab("ACTIVE CASES") + ylab("TOTAL CASES")  + theme(legend.position = "none")
    p <- p + geom_smooth(method = 'lm', formula = y ~ x)
    p
  })
  
  output$p.4 <- renderPlot({
    Continents <- covid_effects_across_continents[2:7,]$continent_names
    p <- ggplot(covid_effects_across_continents[2:7,], aes(x = "", y = total_cases_of_continents/covid_effects_across_continents[2][1,]*100, fill = Continents)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + ylab("")
    p
  })
  
  output$p.5 <- renderPlot({
    Continents <- covid_effects_across_continents[2:7,]$continent_names
    p <- ggplot(covid_effects_across_continents[2:7,], aes(x = "", y = total_deaths2/covid_effects_across_continents[3][1,]*100, fill = Continents)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + ylab("")
    p
  })
  
  output$p.6 <- renderPlot({
    Continents <- covid_effects_across_continents[2:7,]$continent_names
    p <- ggplot(covid_effects_across_continents[2:7,], aes(x = "", y = total_recovered2/covid_effects_across_continents[4][1,]*100, fill = Continents)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + ylab("") 
    p
  })
  
  output$p.7 <- renderPlot({
    Continents <- covid_effects_across_continents[2:7,]$continent_names
    p <- ggplot(covid_effects_across_continents[2:7,], aes(x = "", y = active_cases2/covid_effects_across_continents[5][1,]*100, fill = Continents)) + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + ylab("") 
    p
  })
  
  output$p.8 <- renderPlotly({
    p <- ggplot(us_data, aes(x = TotalDeaths, y = TotalCases)) + geom_point(alpha = 0.7, aes(col = USAState, size = Population)) + xlab("TOTAL DEATHS") + ylab("TOTAL CASES")  + theme(legend.position = "none")
    p <- p + geom_smooth(method = 'lm', formula = y ~ x)
    p
  })
  
  output$p.9 <- renderPlotly({
    p <- ggplot(us_data, aes(x = TotalRecovered, y = TotalCases)) + geom_point(alpha = 0.7, aes(col = USAState, size = Population)) + xlab("TOTAL RECOVERED") + ylab("TOTAL CASES") + theme(legend.position = "none")
    p <- p + geom_smooth(method = 'lm', formula = y ~ x)
    p
  })
  
  output$p.10 <- renderPlotly({
    p <- ggplot(us_data, aes(x = ActiveCases, y = TotalCases)) + geom_point(alpha = 0.7, aes(col = USAState, size = Population)) + xlab("ACTIVE CASES") + ylab("TOTAL CASES")  + theme(legend.position = "none")
    p <- p + geom_smooth(method = 'lm', formula = y ~ x)
    p
  })
  
  output$p.11 <- renderPlotly({
    States <- india.data$StateName
    p <- ggplot(india.data, aes(x = TotalDeaths, y = TotalCases)) + geom_point(alpha = 0.7, aes(col = States, size = Population)) + xlab("TOTAL DEATHS") + ylab("TOTAL CASES")  + theme(legend.position = "none")
    p <- p + geom_smooth(method = 'lm', formula = y ~ x)
    p
  })
  
  output$p.12 <- renderPlotly({
    States <- india.data$StateName
    p <- ggplot(india.data, aes(x = TotalRecovered, y = TotalCases)) + geom_point(alpha = 0.7, aes(col = States, size = Population)) + xlab("TOTAL RECOVERED") + ylab("TOTAL CASES")  + theme(legend.position = "none")
    p <- p + geom_smooth(method = 'lm', formula = y ~ x)
    p
  })
  
  output$p.13 <- renderPlotly({
    States <- india.data$StateName
    p <- ggplot(india.data, aes(x = ActiveCases, y = TotalCases)) + geom_point(alpha = 0.7, aes(col = States, size = Population)) + xlab("ACTIVE CASES") + ylab("TOTAL CASES") + theme(legend.position = "none")
    p <- p + geom_smooth(method = 'lm', formula = y ~ x)
    p
  })
  
  out_1 <- eventReactive(input$r,{
    req(input$country1, input$year1)
    
    df <- subset(gdp_1, Country %in% input$country1 &
                   year %in% input$year1)
    
  })
  
  out_2 <- eventReactive(input$q,{
    req(input$country2, input$year2)
    
    df <- subset(unemployment_1, Country %in% input$country2 &
                   year %in% input$year2)
    
  })
  
  output$distPlot1 <- renderPlot({
    ggplot(out_1(), aes(x = year , y = gdp,
                        group = Country,
                        color = Country)) +
      geom_line(size = 1) +
      scale_y_continuous(labels= scales::dollar,
                         breaks = pretty_breaks()) +
      scale_x_discrete(breaks = pretty_breaks()) +
      theme(legend.position = "bottom",
            legend.title = element_blank())
  },res = 96)
  
  output$distPlot2 <- renderPlot({
    ggplot(out_2(), aes(x = year , y = unemployment,
                        group = Country,
                        color = Country)) +
      geom_line(size = 1) +
      labs(x= "year", y = "unemployment(% of labour force)")+
      scale_y_continuous(
        breaks = pretty_breaks()) +
      scale_x_discrete(breaks = pretty_breaks()) +
      theme(legend.position = "bottom",
            legend.title = element_blank())
  },res = 96)
  
  output$data1 <- renderTable({
    brushedPoints(out_1(), input$plot_brush1)
  })
  
  output$data2 <- renderTable({
    brushedPoints(out_2(), input$plot_brush2)
  })
  
  
  
  output$plot <- renderPlotly({
    ggplot(tsm,aes(x = date,y= Arrivals)) +
      geom_line(color = "red", lwd = 1.0) +
      labs(x = "Date", y = "Number of Tourist Arriavls", title = "Tourist Arrival with Month")
    
    
  })
  
  output$distplot4 <- renderPlotly({
    ph$value = as.numeric(ph$value)
    ggplot(ph, aes(fill=Var1, y=value, x=Var2)) +
      geom_bar(position="dodge", stat="identity")  + labs(x = "Regions",y= "Count",fill="Year")
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)