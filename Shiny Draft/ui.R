library(leaflet)
library(shiny)

# Choices for drop-downs
vars <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Population" = "adultpop"
)


navbarPage(h5("Real Estate & Natural Disasters"), id="nav",
           
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
                    
                    tags$head(
                      includeCSS("styles.css")),
                    
                    leaflet::leafletOutput("new_orleans_map"),
                    
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Hurricane"),
                                  h4("New Orleans"),
                                  h6("Aug 2005"),
                      
                                  selectInput("color", "Color", vars),
                                  selectInput("size", "Size", vars, selected = "adultpop"),
                                  conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                   # Only prompt for threshold when coloring or sizing by superzip
                                                   numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                  ),
                                              
                      plotly::plotlyOutput("bar_chart"),
                      plotly::plotlyOutput("line_chart"))),
           
           ### COFFEY PARK ###
           tabPanel(h6("Coffey Park, CA - Wildfires"),
                    
                    tags$head(
                      includeCSS("styles.css")),
                    
                    leaflet::leafletOutput("coffey_park_map"),
                    
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Wildfires"),
                                  h4("Coffey Park, CA"),
                                  h6("Oct 2017"),
                                  
                                  selectInput("color", "Color", vars),
                                  selectInput("size", "Size", vars, selected = "adultpop"),
                                  conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                   # Only prompt for threshold when coloring or sizing by superzip
                                                   numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                  ),
                                  
                                  plotly::plotlyOutput("bar_chart"),
                                  plotly::plotlyOutput("line_chart"))),
           
           ### BOULDER ###
           tabPanel(h6("Boulder, CO - Floods"), 
                    
                    tags$head(
                      includeCSS("styles.css")),
                    
                    leaflet::leafletOutput("boulder_co_map"),
                    
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Floods"),
                                  h4("Boulder, CO"),
                                  h6("Sep 2013"),
                                  
                                  selectInput("color", "Color", vars),
                                  selectInput("size", "Size", vars, selected = "adultpop"),
                                  conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                   # Only prompt for threshold when coloring or sizing by superzip
                                                   numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                  ),
                                  
                                  plotly::plotlyOutput("bar_chart"),
                                  plotly::plotlyOutput("line_chart"))),
           
           
           ### BUFFALO ###
           tabPanel(h6("Buffalo, NY - Snowstorm"),
                    
                    tags$head(
                      includeCSS("styles.css")),
                    
                    leaflet::leafletOutput("boulder_co_map"),
                    
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Snowstorm"),
                                  h4("Buffalo, NY"),
                                  h6("Nov 2014"),
                                  
                                  selectInput("color", "Color", vars),
                                  selectInput("size", "Size", vars, selected = "adultpop"),
                                  conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                   # Only prompt for threshold when coloring or sizing by superzip
                                                   numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                  ),
                                  
                                  plotly::plotlyOutput("bar_chart"),
                                  plotly::plotlyOutput("line_chart"))),
           
           ### MOORE, OK ###
           tabPanel(h6("Moore, OK - Tornado"),
                    tags$head(
                      includeCSS("styles.css")),
                    
                    leaflet::leafletOutput("boulder_co_map"),
                    
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Tornado"),
                                  h4("Moore, OK"),
                                  h6("May 2013"),
                                  
                                  selectInput("color", "Color", vars),
                                  selectInput("size", "Size", vars, selected = "adultpop"),
                                  conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                   # Only prompt for threshold when coloring or sizing by superzip
                                                   numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                  ),
                                  
                                  plotly::plotlyOutput("bar_chart"),
                                  plotly::plotlyOutput("line_chart"))),
           
           ### BP OIL SPILL ###
           tabPanel(h6("Grand Isle, LA - BP Oil Spill"),
                    tags$head(
                      includeCSS("styles.css")),
                    
                    leaflet::leafletOutput("boulder_co_map"),
                    
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 170, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  
                                  h4("Oil Spill"),
                                  h4("Grand Isle, LA"),
                                  h6("Apr 2010"),
                                  
                                  selectInput("color", "Color", vars),
                                  selectInput("size", "Size", vars, selected = "adultpop"),
                                  conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                   # Only prompt for threshold when coloring or sizing by superzip
                                                   numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                  ),
                                  
                                  plotly::plotlyOutput("bar_chart"),
                                  plotly::plotlyOutput("line_chart"))))