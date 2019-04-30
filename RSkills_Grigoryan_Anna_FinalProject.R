rm(list=ls())
#install.packages("usmap")

# Application URL https://thegrigorian.shinyapps.io/HoneyProductionAnna/

#######################################################
# LOADING ALL THE REQUIRED PACKAGES
library(shiny)
library(dplyr)
library(data.table)
library(forecast)
library(reshape2)
library(ggplot2)
library(pander)
library(DT)
library(plotly)
library(usmap)

#######################################################
# UI


# Define UI 
ui <- shinyUI(fluidPage(
  titlePanel("Honey Production in USA between 1998 and 2012"),
  sidebarLayout(
    sidebarPanel(
      uiOutput('values'),
      
      uiOutput('states'),
      
      uiOutput('year')
    ),
    
    # Name the panels
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction", verbatimTextOutput("Introduction")),
        tabPanel("Overview", verbatimTextOutput("overview")),
        
        tabPanel("Data", tableOutput("table1")),
        
        tabPanel("Summary statistics", tableOutput("table2")),
        
        tabPanel("Times series plot", plotlyOutput("plot")),
        
        tabPanel("Production Forecast with ARIMA", plotOutput("forecast1")),
        
        tabPanel("Production map", plotlyOutput("map"))
        
      )
    )
  )
))



#######################################################
# server
server <- shinyServer(function(input, output) {
  data <- fread('https://raw.githubusercontent.com/thegrigorian/honeyproduction/master/honeyproduction.csv')

  data$state <- state.name[match(data$state,state.abb)]
  
  data[, ':='(
    numcol_K = numcol/1000,
    yieldpercol_kg = yieldpercol*0.45359237,
    totalprod_ton = yieldpercol*numcol*0.45359237/1000,
    stocks_ton = stocks * 0.45359237/1000,
    price_per_kg = priceperlb / 0.45359237,
    prodvalue_K = prodvalue / 1000,
    valuepercol = yieldpercol*priceperlb
  )]
  

# Drop down menu for plot values
  
  output$values <- renderUI({
    selectInput(inputId = "values", label = strong("Measure"), 
                choices = list("Total Production (ton)" = "totalprod_ton",
                               "Yield per colony (kg)" = "yieldpercol_kg",
                               "Price per kg (USD)" = "price_per_kg",
                               "Stocks (ton)" = "stocks_ton"
                               ))
  })
  
# Drop down menu for states 
  
  output$states <- renderUI({
    selectInput("states", "Select state:", unique(data$state), multiple = T, selected = 'Alabama')
  })
  
  
# Slider for the year to start from
  
  output$year <- renderUI({
    sliderInput(inputId = "year", label = strong("Year"), min = min(data$year),
                max = max(data$year), value = 2004, step = 1 )
  })
  
  
# Making the data interactive
  myreactivedata1 <- reactive({
    t_data<- data[state %in% input$states & input$year <= year &  year <= 2012, ]
    return(t_data)
  })
  
# Introduction tab
  
  output$Introduction <- renderText({
    description <- ("This Shiny application analyzes the US Honey production. 
                    
       OUTLINE

    1. Overview
    Description of the data source

    2. Data
    Querying state level data by years

    3. Summary statistics
    US Honey Production 1998-2012 summary

    4. Time series plot 
    US Honey Production for selected states from the given time up to 2012

    5. Production Forecast with ARIMA
    Forecast for 5 years using ARIMA

    6. Production map
    US map of production by states for given year and measure
                   
                    
                     ")
    print(description)  
  })
  
# Overview Tab 
  
  output$overview <- renderText({
    description <- ("UN Honey production data is collected by the National Agricultural Statistics Center. The original data contains 626 observations from 1998 to 2012 years and the following features. 

          numcol: Number of honey producing colonies. 
          yieldpercol: Honey yield per colony. (ibs)
          totalprod: Total production of honey in a year.  (ibs)
          stocks: Stocks held by producers. (ibs)
          priceperlb: Average price per pound based on expanded sales.(USD)
          prodvalue: Value of production (totalprod x priceperlb). (USD)

The following variables have been added to the dataset manually:
          numcol_K: Number of honey producing colonies in thousands. 
          yieldpercol_kg: Honey yield per colony. (kgs)
          totalprod_ton: Total production of honey in a year.  (tons)
          stocks_ton: Stocks held by producers. (tons)
          price_per_kg: Average price per kg based on expanded sales.(USD)
          prodvalue_K: Value of production  (1000 USD)


** Due to rounding, total colonies multiplied by total yield may not equal production. Also, summation of states will not equal U.S. level value of production. Metric conversion of the same variable have also been added to the work data. ")
    print(description)  
  })

  
# First plot
  
  output$plot <- renderPlotly({
    
    p <- ggplot(myreactivedata1(), 
                aes(x = year, y = get(input$values), color = state)) +
      geom_line() +
      geom_point() +
      geom_text (aes(label = as.integer(input$values)), vjust = 4) +
      labs(x = input$year, y = input$value)
    return(ggplotly(p))
  })
  
  
  cols1 <- c('year', 'state', 'totalprod_ton', 'price_per_kg', 'yieldpercol_kg', 'stocks_ton', 'numcol_K', 'prodvalue_K')
  
# Data query table
  output$table1 <- renderTable({
    
    return(myreactivedata1()[, .SD, .SDcols = cols1])
  })
  
  cols2 <- c('totalprod_ton', 'price_per_kg', 'yieldpercol_kg', 'stocks_ton')
  
  
  
# summary statistics table
  
  output$table2 <- renderTable({
    cbind("Measure"= c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."),
          data[, lapply(.SD, summary), .SDcols = cols2])
  })
  
# forecast
  output$forecast1 <- renderPlot({
    honey <- data[, .(prod = sum(totalprod_ton)), by = year]
    honey_ts <- ts(honey$prod, start = 1998)
    fit <- auto.arima(honey_ts, D = 1)
    autoplot(forecast(fit, 5))
  }) 
  
# Mapping
  output$map <- renderPlotly({
    datamap <- subset(data,year == input$year)
    datamap <- as.data.frame(datamap)
    
    m <- plot_usmap(data=datamap, values = input$values, lines = "red", labels = TRUE) + 
      scale_fill_continuous(name = "Measure") + 
      theme(legend.position = "right")
    return(ggplotly(m))
    
  
   
  })
  

  
})

shinyApp(ui = ui, server = server)