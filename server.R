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
  
}