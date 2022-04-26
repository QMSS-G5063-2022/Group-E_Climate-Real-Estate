library(leaflet)
library(shiny)

# Choices for drop-downs
vars <- c(
  "Annual Home Price Index" = "hpi",
  "Bottom Tier Home Value " = "bottom_tier",
  "Single Family Home Value" = "sfhv")


navbarPage(h4("Real Estate & Natural Disasters"), id="nav",
           
           ### HOME PAGE ###
           
           tabPanel(h6("Home Page"),
                    
                    h2("Real Estate amid Disaster: A Data Visualization Exploration"),
                    h1(" "),
                    h4("Rory Butler, Cindy Chen, Lizabeth Singh, Jeffray Tsai"),
                    h1(" "),
                    h5("As climate change increases both the intensity and frequency of natural disasters, these consequences also have
                       immediate impacts on real estate, be that housing sale prices, rental prices, or home price indices.  In profiling
                       four notable natural disasters in the United States, along with one human-made disaster, we aim to understand
                       the immediate and short-term impact that natural destruction and catastrophe induces on the housing market.
                       
                       Is the housing market more resilient to certain types of disasters?  How quickly does it take to bounce back?
                       Our project explores these questions in an engaging manner."),
                    h1(" "),
                    img(src = "hurricane-katrina.jpg", align = "center", height = "15%", width = "15%"),
                    img(src = 'coffey-park-fire.jpg', align = "center", height = "15%", width = "15%"),
                    img(src = 'boulder-floods.jpg', align = "center", height = "15%", width = "15%"),
                    img(src = 'buffalo-snow.jpg', align = "center", height = "15%", width = "15%"),
                    img(src = 'moore-ok-tornado.jpg', align = "center", height = "15%", width = "15%"),
                    img(src = 'grand-isle-oil.jpg', align = "center", height = "15%", width = "15%")
           ),
           
           ### NEW ORLEANS ###
           tabPanel(h6("New Orleans, LA - Hurricane"),

                    leafletOutput("new_orleans_map", width = "100%", height = "100%"),
                    
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Hurricane"),
                                  h4("New Orleans"),
                                  h6("Aug 2005"),
                      
                                  sliderInput(
                                    inputId = "choose_month_neworleans", 
                                    label = "Choose the month/year",
                                    min = as.Date("2002-01-01"),
                                    max = as.Date("2008-01-01"),
                                    value= as.Date("2005-08-01"),
                                    timeFormat="%b %Y"),
                    
                                  selectInput("choose_metric_neworleans", "Choose Real Estate Metric", vars, selected = "hpi"),
                                              
                      plotly::plotlyOutput("bar_chart_neworleans"),
                      plotly::plotlyOutput("line_chart_neworleans")
                      )),
                    
           ### COFFEY PARK ###
           tabPanel(h6("Coffey Park, CA - Wildfires"),
                    
                    leaflet::leafletOutput("coffey_park_map", width = "100%", height = "100%"),
                    
                    absolutePanel(id = "controls",
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = TRUE,
                                  top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Wildfires"),
                                  h4("Coffey Park, CA"),
                                  h6("Oct 2017"),
                                  
                                  sliderInput(
                                    inputId = "choose_month_coffey", 
                                    label = "Choose the month/year",
                                    min = as.Date("2015-01-01"),
                                    max =as.Date("2020-01-01"),
                                    value= as.Date("2017-07-31"),
                                    timeFormat="%b %Y"),
                                  
                                  selectInput("choose_metric_coffey", "Choose Real Estate Metric", vars, selected = "hpi"),
                                  
                                  plotly::plotlyOutput("bar_chart_coffey"),
                                  plotly::plotlyOutput("line_chart_coffey"))),
           
           ### BOULDER ###
           tabPanel(h6("Boulder, CO - Floods"), 
                    
                    tags$head(
                      includeCSS("styles.css")),
                    
                    leaflet::leafletOutput("boulder_co_map", width = "100%", height = "100%"),
                    
                    absolutePanel(id = "controls",
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = TRUE,
                                  top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Floods"),
                                  h4("Boulder, CO"),
                                  h6("Sep 2013"),
                                  
                                  sliderInput(
                                    inputId = "choose_month_boulder", 
                                    label = "Choose the month/year",
                                    min = as.Date("2011-01-01"),
                                    max =as.Date("2016-01-01"),
                                    value= as.Date("2013-09-01"),
                                    timeFormat="%b %Y"),
                                  
                                  selectInput("choose_metric_boulder", "Choose Real Estate Metric", vars, selected = "hpi"),
                                  
                                  plotly::plotlyOutput("bar_chart_boulder"),
                                  plotly::plotlyOutput("line_chart_boulder"))),
           
           
           ### BUFFALO ###
           tabPanel(h6("Buffalo, NY - Snowstorm"),
                    
                    tags$head(
                      includeCSS("styles.css")),
                    
                    leaflet::leafletOutput("boulder_co_map", width = "100%", height = "100%"),
                    
                    absolutePanel(id = "controls",
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = TRUE,
                                  top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Snowstorm"),
                                  h4("Buffalo, NY"),
                                  h6("Nov 2014"),
                                  
                                  sliderInput(
                                    inputId = "choose_month_buffalo", 
                                    label = "Choose the month/year",
                                    min = as.Date("2012-01-01"),
                                    max =as.Date("2017-01-01"),
                                    value= as.Date("2014-11-01"),
                                    timeFormat="%b %Y"),
                                  
                                  selectInput("choose_metric_buffalo", "Choose Real Estate Metric", vars, selected = "hpi"),
                                  
                                  plotly::plotlyOutput("bar_chart_buffalo"),
                                  plotly::plotlyOutput("line_chart_buffalo"))),
           
           ### MOORE, OK ###
           tabPanel(h6("Moore, OK - Tornado"),
                    tags$head(
                      includeCSS("styles.css")),
                    
                    leaflet::leafletOutput("boulder_co_map", width = "100%", height = "100%"),
                    
                    absolutePanel(id = "controls",
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = TRUE,
                                  top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Tornado"),
                                  h4("Moore, OK"),
                                  h6("May 2013"),
                                  
                                  sliderInput(
                                    inputId = "choose_month_moore", 
                                    label = "Choose the month/year",
                                    min = as.Date("2011-01-01"),
                                    max =as.Date("2016-01-01"),
                                    value= as.Date("2013-05-01"),
                                    timeFormat="%b %Y"),
                                  
                                  selectInput("choose_metric_moore", "Choose Real Estate Metric", vars, selected = "hpi"),
                                  
                                  plotly::plotlyOutput("bar_chart_moore"),
                                  plotly::plotlyOutput("line_chart_moore"))),
           
           ### BP OIL SPILL ###
           tabPanel(h6("Grand Isle, LA - BP Oil Spill"),
                    tags$head(
                      includeCSS("styles.css")),
                    
                    leaflet::leafletOutput("grand_isle_map", width = "100%", height = "100%"),
                    
                    absolutePanel(id = "controls",
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = TRUE,
                                  top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Oil Spill"),
                                  h4("Grand Isle, LA"),
                                  h6("Apr 2010"),
                                  
                                  sliderInput(
                                    inputId = "choose_month_grandisle", 
                                    label = "Choose the month/year",
                                    min = as.Date("2007-01-01"),
                                    max =as.Date("2013-01-01"),
                                    value= as.Date("2010-04-01"),
                                    timeFormat="%b %Y"),
                                  
                                  selectInput("choose_metric_grandisle", "Choose Real Estate Metric", vars, selected = "hpi"),
                                  
                                  plotly::plotlyOutput("bar_chart_grandisle"),
                                  plotly::plotlyOutput("line_chart_grandisle"))))