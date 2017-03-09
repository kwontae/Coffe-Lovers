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
tp.country <- total.production[[1]];
  #c("Angola","Benin","Bolivia","Brazil","Burundi","Cameroon","Central African Republic","Colombia","Congo, Dem. Rep. of","Congo, Rep. of",
  #              "Costa Rica","CC4te d'Ivoire","Cuba","Dominican Republic","Ecuador", "El Salvador","Equatorial Guinea","Ethiopia","Gabon","Ghana",
  #              "Guatemala","Guinea","Guyana","Haiti","Honduras", "India","Indonesia","Jamaica","Kenya","Lao, People's Dem. Rep. of",
  #              "Liberia","Madagascar","Malawi","Mexico","Nepal","Nicaragua","Nigeria","Panama","Papua New Guinea","Paraguay",
  #              "Peru","Philippines","Rwanda","Sierra Leone","Sri Lanka","Tanzania","Thailand","Timor-Leste","Togo","Trinidad & Tobago",
  #              "Uganda","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe");
country <- retail.prices[[1]];
years <- c(1990:2015)

grower.country <- grower.prices[-45,]

my.ui <- fluidPage(
  titlePanel("Coffee Industry Report"),
  
  p("Hello, and welcome to our application!  In this application you will find data analyses, data tables, and interactive plots, created 
    for the purpose of better understanding the International Coffee Organization's data on the coffee industry.  I will get more into 
    the nitty gritty aspects of the data set under the", strong("Summary"), "tab.  You can also find a sample data table under the", 
    strong("Table"), "tab, and three different interactive plots under the other three tabs.  All of the widgets you see on the left-hand
    side of the application allow you to manipulate the data you see in the plots, so don't be afraid to play around with our visualizations.  
    We are very excited about presenting this data in a way that allows users to ask and answer their own questions, so please, explore 
    our application and learn more about the coffee industry!"),
  
  sidebarLayout(
    sidebarPanel(
      label = "Controls",
      # Drop-Down menu of countries

      selectInput('country', label = "Select Country", choices = country),
      selectInput('tpcountry', label = "Select Total Production Country", choices = tp.country),
      selectInput("cg.country", label = "Select a Coffee-Growing Country", choices = grower.country$Country),
      # Slider for years
      sliderInput('year', label = "Select Year", min = min(years), max = max(years), value = c(min(years),median(years)), step = 1)
      ),
    mainPanel(
      tabsetPanel(type = "tabs",
        # Summary panel
        tabPanel("Summary",
                 p("In this section, I want to tell a story about our group, how we found the International Coffee Organization (ICO), the aspects of their
                   data that attracted us, and how we will be presenting the data in order for our users to learn more about the coffee industry."),
                 p("Let's begin with the story of our group.  From our first day together, we realized that all four of us love coffee (hence our fantastic 
                   group name).  It became immediately apparent that whatever data we decided to analyze had to be related to coffee in some way.  We left the
                   first meeting and all decided to independantly look into various databases with information on the coffee industry.  A few days after our
                   initial discussion, Sojin found the Infernational Coffee Organization, or ICO.  A link to their online data base can be found here:", 
                   a("DATABASE", href="http://www.ico.org/new_historical.asp"), ".  We were immediately impressed by the sheer number of data sets we had
                   at our figer tips, all of which were free."),
                 p("Now that we had a database, we needed to decide which data sets we wanted to work with, and ultimately, what questions we wanted to answer
                   for whom.  Of the 15 available data sets, we chose the four we thought could best analyze for the benefit of coffee growers, coffee manufacturers,
                   and coffee consumers.  These data sets were titled:", em('Total Production, Prices Paid to Growers, Retail Prices, and Consumption'), ".  We thought that
                   an analysis of these four data sets would answer many questions for all three marties mentioned above, including: 1. How does the total production
                   of coffee relate to the retail prices consumers are being charged; 2. What is the difference between what growers are being paid and what retailers are
                   charging; and 3. Does retail price impact consumption rate, and if so, by how much?  These are just three questions we thought we could answer
                   using our data sets of choice, however, it is our goal that when the user interacts with this application, they will formulate their own questions
                   and that interacting with our application will be able to answer those questions."),
                 p("Having decided on four data sets, we now had to do the dirty work of cleaning the data in order to make it analyzable.  Originally it came in
                   highly formatted Excel spreadsheets which in no way translated into a CSV file.  There were useless column names, lots of unnecessary spaces between
                   rows of the data, random comments left by the ICO, and many other silly formatting issues.  To solve this problem, we each took one data set and
                   went to town cleaning it.  We made useful column names, deleted all unnecessary spaces and the comments, and imported the files as CSV files.  With the
                   files cleaned, it was time to begin our analysis."),
                 p("Like I mentioned earlier, our analysis will be presented in four different tabs.  The 'Table' tab contains a sample data table of parts of each data set,
                   the 'Total Production v Retail Price tab contains an interactive plot of total production compared to retail price, the 'Prices Paid to Growers v Retail Price tab
                   also contains an interactive plot, but this one of the prices paid to growers compared to retail price, and the 'Retail Price v Consumption' tab contains yet anther
                   interactive plot, this one of retail prices compared to consumption rates.  Users can interact with each of these plots by", strong("brushing"), "sections of the graphs
                   and then", strong("double clicking"), "applications to change parts of the data visualizations. (Note that the widgets will not effect the data table under the 'Table' tab)."),
                 p("So welcome to our application, we hope that this summary of our project has been sufficient to allow you to begin interacting with our application, as well as understand
                   our process of creating this application.  Please take your time to look around, interact, manipulate, and engage with our analysis, since ultimately this application
                   is for you, the user. Thank you!"),
                 p("- Kyle, Mitesh, Sojin, and Taehyun")),
        # Table panel
        tabPanel("Table", h3("Sample of Retail Prices (in US $/lb)"), tableOutput("table"),
                 h3("Sample of Grower Prices (in US cents/lb)"), tableOutput("table2")),
        # 1 of 3 plot panels
        tabPanel("Total Production v Retail Price", 
             fluidRow(
                column(width = 10, class = "well", h3("Total Production v Retail Price"),
                    h5(em("Brush and double-click to zoom")),
                    plotOutput('plot.tp', height = 500,
                        dblclick = "plot_dblclick",
                        brush = brushOpts(
                          id = "plot_brush",
                          resetOnNew = TRUE
                        )
                    )
                )
            ),
            fluidRow(
              column(width = 10, class = "well",
                     h4("Brushed points"),
                     verbatimTextOutput("brush_info"),
                     p("Above you will see a plot of Total Production versus Retail Price.  You can interact with the plot by brushing over a set of points
                       and then double clicking the brushed area to zoom into the points.  You can exit the zoomed view by double clicking again at any time.  
                       You can change the x-axis in this plot by selecting a different country from the first drop-down menu 'Select Country', and change
                       the y-axis in this plot by selecting a different country from the second drop-down menu 'Select Total Production Country'."))
            )
        ),
        # 2 of 3 plot panels
        tabPanel("Price Paid to Growers v Retail Price", 
          fluidRow(
            column(width = 10, class = "well", h3("Prices Paid to Growers v Retail Price"),
              h5(em("Brush and double-click to zoom")),
              plotOutput('gr.plot', height = 500,
                dblclick = "plot1_dblclick",
                brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE
                   )
                )
              )
            ),
          fluidRow(
            column(width = 10, class = "well",
                   p("Above you will see a plot of Prices Paid to Growers versus Retail Price.  You can interact with the plot by brushing over a set of points
                       and then double clicking the brushed area to zoom into the points.  You can exit the zoomed view by double clicking again at any time.  
                       You can change the x-axis in this plot by selecting a different country from the first drop-down menu 'Select Country', and change
                       the y-axis in this plot by selecting a different country from the third drop-down menu 'Select a Coffee-Growing Country'."))
          )
        ),
        # 3 of 3 plot panels
        tabPanel("Retail Price v Consumption", 
                 fluidRow(
                   column(width = 10, class = "well", h3("Retail Price v Consumption"),
                          h5(em("Brush and double-click to zoom")),
                          plotOutput("rvc.plot", height = 500,
                                     dblclick = "plot_dblclick",
                                     brush = brushOpts(
                                       id = "plot_brush",
                                       resetOnNew = TRUE
                                     )
                          )
                   )
                 ),
                 fluidRow(
                   column(width = 10, class = "well",
                          p("Above you will se a plot that shows the how the Retail Price of Coffee affected its consumption over the years. You can interact with the plot by brushing over a set of points
                       and then double clicking the brushed area to zoom into the points.  You can exit the zoomed view by double clicking again at any time. You can navigate data of different countries by
                            selecting different countries from the drop-down menu. The size of the point is proportional to the consumption during the respective year."))
                ),
                fluidRow(
                  column(width = 10, class = "well",
                         p("Fun Fact : It is well known fact that coffee is one of the best stress-busters. In the plot of USA, one can clearly see the dramatic increase in consumption of coffee after 2008, which
                            was the year of the Crisis."))
                )
        )
      )
    )
  )
)