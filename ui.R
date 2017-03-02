# ui.R 
# Contributors: Kyle Simpson, Taehyun Kwon, Sojin Park, Mitesh Goyal

library(tidyverse)
library(shiny)
library(rsconnect)

# data
total.production <- read.csv("./data/clean1a-TotalProduction.csv", stringsAsFactors = FALSE)
grower.prices <- read.csv("./data/clean3a-PricesPaidToGrowers.csv", stringsAsFactors = FALSE)
retail.prices <- read.csv("./data/clean3b-RetailPrices.csv", stringsAsFactors = FALSE)
consumption <- read.csv("./data/clean4b-Consumption.csv", stringsAsFactors = FALSE)

years <- c(1990:2015)

my.ui <- fluidPage(
  titlePanel("Coffee Industry Report"),
  
  sidebarLayout(
    sidebarPanel(
      label = "Controls",
      # Drop-Down menu of countries
      selectInput("country", label = "Select Country", choices = c("USA", "usa")),
      # Slider for years
      sliderInput("year", label = "Select Year", min = min(years), max = max(years), value = median(years), step = 1),
      # Checkbutton for trend lines on the plots
      radioButtons("trendline", label = "Show Trend Line?", choices = list("Yes" = "y", "No" = "n"))
      ),
    mainPanel(
      tabsetPanel(
        # Summary panel
        tabPanel("Summary"),
        # Table panel
        tabPanel("Table", tableOutput("table")),
        # 1 of 3 plot panels
        tabPanel("Total Production v Retail Price", plotOutput("tp.v.rp")),
        # 2 of 3 plot panels
        tabPanel("Price Paid to Growers v Retail Price", plotOutput("ppg.v.rp")),
        # 3 of 3 plot panels
        tabPanel("Retail Price v Consumption", plotOutput("rp.v.con"))
      )
    )
  )
  
)