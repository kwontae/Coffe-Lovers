# server.R
# Contributors: Kyle Simpson, Taehyun Kwon, Sojin Park, Mitesh Goyal
library(ggplot2)
library(dplyr)
library(tidyverse)
library(shiny)
library(rsconnect)

# data
total.production <- read.csv("./data/clean1a-TotalProduction.csv", stringsAsFactors = FALSE)
grower.prices <- read.csv("./data/clean3a-PricesPaidToGrowers.csv", stringsAsFactors = FALSE)
retail.prices <- read.csv("./data/clean3b-RetailPrices.csv", stringsAsFactors = FALSE)
consumption <- read.csv("./data/clean4b-Consumption.csv", stringsAsFactors = FALSE)

grower.country <- grower.prices[-45,]
retail.country <- retail.prices[-29,]

my.server <- function(input, output) {
  # Temporary data frame for Table tab
  ranges <- reactiveValues(x = NULL, y = NULL) #This allows the plot to adjust for more interaction.
  temp.retail <- filter(retail.prices, complete.cases(retail.prices[, 2:26])) %>% 
    select(Country, X1990, X1992, X1994, X1996, X1998, X2000, X2002, X2004, X2006, X2008, X2010, X2012, X2014, X2015)
  colnames(temp.retail) <- c("Country", "1990", "1992", "1994", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2015")
  output$table <- renderTable(temp.retail)
  
  # Temporary data frame for Table tab
  temp.grower <- filter(grower.prices, complete.cases(grower.prices[, 2:26])) %>% 
    select(Country, X1990, X1992, X1994, X1996, X1998, X2000, X2002, X2004, X2006, X2008, X2010, X2012, X2014, X2015)
  colnames(temp.grower) <- c("Country", "1990", "1992", "1994", "1996", "1998", "2000", "2002", "2004", "2006", "2008", "2010", "2012", "2014", "2015")
  output$table2 <- renderTable(temp.grower)
  
  #this is my reactive variable of a main data set.
  data.tp <- reactive({
    colnames(total.production) <- c("Country", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016");
    total.production <- total.production %>%
      filter(Country == input$tpcountry) %>%
      subset(select = c("Country", input$year[1]:input$year[2]))
    total.production <- total.production %>%
      gather_("year","production",as.character(c(input$year[1]:input$year[2]))) %>%
      select(year, production) %>%
      mutate(retail = data.rp()[[1]])
    return(total.production);
  })
  data.rp <- reactive({
    colnames(retail.prices) <- c("Country", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015");
    retail.prices <- retail.prices %>%
      subset(select = c("Country", input$year[1]:input$year[2])) %>%
      filter(Country == input$country) %>%
      gather_("year","values",as.character(c(input$year[1]:input$year[2]))) %>%
      select(values);
    return(retail.prices);
  })

  output$plot.tp <- renderPlot({
    ploty <- ggplot(data = data.tp(), aes_string(x = colnames(data.tp()[3]), y = colnames(data.tp()[2]), color = colnames(data.tp()[1]))) +
      geom_point(size = 5) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
      labs(x=paste0("Retail price from ",input$country,"(In US$/lb)"), y= paste0("Total production from ",input$tpcountry," (In thousand 60kg bags)"))
    return(ploty)
  })
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if(!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(data.tp(), input$plot_brush)
  })

  # Price paid to the growers vs. Retail Price Plot
  output$gr.plot <- renderPlot({
    
    # Change the column name to take out 'X'
    colnames(retail.country) <- c("Country", "1990", "1991", "1992", "1993", "1994", "1995", "1996",
                             "1997", "1998", "1999", "2000", "2001", "2002", "2003",
                             "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                             "2011", "2012", "2013", "2014", "2015")
    
    # filtering out to the retail country chosen
    retail.country <- filter(retail.country, Country == input$country) %>%
      subset(select = c("Country", input$year[1]:input$year[2]))
    retail.country <- retail.country %>%
      gather_("years",
              "values",
              as.character(c(input$year[1]:input$year[2])))
    
    
    # Change the column name so it takes out X
    colnames(grower.country) <- c("Country", "1990", "1991", "1992", "1993", "1994", "1995", "1996",
                             "1997", "1998", "1999", "2000", "2001", "2002", "2003",
                             "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                             "2011", "2012", "2013", "2014", "2015")
    # filtering out to the growing country chosen
    grower.country <- filter(grower.country, Country == input$cg.country) %>%
      subset(select = c("Country", input$year[1]:input$year[2]))
    grower.country <- grower.country %>%
      gather_("years",
              "values",
              as.character(c(input$year[1]:input$year[2])))
      
    ggplot(data = grower.country) +
      geom_point(mapping = aes(x = years, y = values, color = Country)) +
      geom_point(data = retail.country, mapping = aes(x = years, y = values, color = Country)) +
      labs(list(y = "Price paid to growers vs. Retail price of 'roasted' coffee in US$/lb", x = "years"))
  })
}