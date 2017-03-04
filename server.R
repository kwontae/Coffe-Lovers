# server.R
# Contributors: Kyle Simpson, Taehyun Kwon, Sojin Park, Mitesh Goyal

library(tidyverse)
library(shiny)
library(rsconnect)

# data
total.production <- read.csv("./data/clean1a-TotalProduction.csv", stringsAsFactors = FALSE)
grower.prices <- read.csv("./data/clean3a-PricesPaidToGrowers.csv", stringsAsFactors = FALSE)
retail.prices <- read.csv("./data/clean3b-RetailPrices.csv", stringsAsFactors = FALSE)
consumption <- read.csv("./data/clean4b-Consumption.csv", stringsAsFactors = FALSE)

my.server <- function(input, output) {
  # Temporary data frame for Table tab
  temp.retail <- filter(retail.prices, complete.cases(retail.prices[, 2:26])) %>% 
    select(Country, X1990, X1992, X1994, X1996, X1998, X2000, X2002, X2004, X2006, X2008, X2010, X2012, X2014, X2015)
  colnames(temp.retail) <- c("Country", "1990", "1992", "1994", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2015")
  output$table <- renderTable(temp.retail)
  
  # Temporary data frame for Table tab
  temp.grower <- filter(grower.prices, complete.cases(grower.prices[, 2:26])) %>% 
    select(Country, X1990, X1992, X1994, X1996, X1998, X2000, X2002, X2004, X2006, X2008, X2010, X2012, X2014, X2015)
  colnames(temp.grower) <- c("Country", "1990", "1992", "1994", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2015")
  output$table2 <- renderTable(temp.grower)
}