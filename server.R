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

grower.country <- grower.prices[-45,]
retail.country <- retail.prices[-29,]

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
  
  # Price paid to the growers vs. Retail Price Plot
  output$gr.plot <- renderPlot({
    # filtering out to the retail country chosen
    gr.retail <- filter(retail.country, Country == input$country)
    
    # Change the column name to take out 'X'
    colnames(gr.retail) <- c("Country", "1990", "1991", "1992", "1993", "1994", "1995", "1996",
                             "1997", "1998", "1999", "2000", "2001", "2002", "2003",
                             "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                             "2011", "2012", "2013", "2014", "2015")
    
    # Transform data wide to long for the plot to read the data. 
    gr.retail <- gather(gr.retail, 
                        key = years,
                        value = values,
                        get("1990"), get('1991'), get('1992'), get('1993'), get('1994'), 
                        get('1995'), get('1996'), get('1997'), get('1998'), get('1999'),
                        get('2000'), get('2001'), get('2002'), get('2003'), get('2004'), 
                        get('2005'), get('2006'), get('2007'), get('2008'), get('2009'),
                        get('2010'), get('2011'), get('2012'), get('2013'), get('2014'), get('2015'))
    
    # filtering out to the growing country chosen
    gr.grower <- filter(grower.country, Country == input$cg.country)
    
    # Change the column name so it takes out X
    colnames(gr.grower) <- c("Country", "1990", "1991", "1992", "1993", "1994", "1995", "1996",
                             "1997", "1998", "1999", "2000", "2001", "2002", "2003",
                             "2004", "2005", "2006", "2007", "2008", "2009", "2010",
                             "2011", "2012", "2013", "2014", "2015")
    
    # Transform data wide to long for the plot to read the data.
    gr.grower <- gather(gr.grower, 
                        key = years,
                        value = values,
                        get("1990"), get('1991'), get('1992'), get('1993'), get('1994'), 
                        get('1995'), get('1996'), get('1997'), get('1998'), get('1999'),
                        get('2000'), get('2001'), get('2002'), get('2003'), get('2004'), 
                        get('2005'), get('2006'), get('2007'), get('2008'), get('2009'),
                        get('2010'), get('2011'), get('2012'), get('2013'), get('2014'), get('2015'))
      
      gr.grower <- filter(gr.grower, years == input$year)
      
    ggplot(data = gr.grower) +
      geom_point(mapping = aes(x = years, y = values, color = Country)) +
      geom_point(data = gr.retail, mapping = aes(x = years, y = values, color = Country))
  })

  
}