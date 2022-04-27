vars <- c(
  "Annual Home Price Index" = "hpi",
  "Bottom Tier Home Value " = "bottom_tier",
  "Single Family Home Value" = "sfhv")

locations <- c("New Orleans - Hurricane" = "neworleans",
               "Coffey Park - Wildfires" = "coffeypark",
               "Boulder, CO - Floods" = "boulder",
               "Buffalo, NY - Snowstorm" = "buffalo",
               "Moore, OK - Tornado" = "moore",
               "Grand Isle, LA - BP Oil Spill" = "grandisle")

navbarPage(h4("Real Estate & Severe Disasters"),
           id="nav",
           
           tabPanel(h6("Introduction"),
                    
                    h2("Real Estate amid Disaster: A Data Visualization Exploration"),
                    h1(" "),
                    p("Rory Butler, Cindy Chen, Lizabeth Singh, Jeffray Tsai"),
                    h1(" "),
                    h5("As climate change and ongoing human events increases both the intensity and frequency of severe environmental
                    events, these consequences also have immediate impacts on real estate, be that housing sale prices, rental prices, or home price indices.  In profiling
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
           
           tabPanel(h6("Deep Dive"),
                    
                    div(class="outer",
                        tags$head(includeCSS("styles.css")),
                        
                        leaflet::leafletOutput("disaster_map", height = "100%", width = "100%"),
         
         absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                
                sliderInput(
                  inputId = "choose_month", 
                  label = "Choose the month/year",
                  min = as.Date("2002-01-01"),
                  max = as.Date("2021-01-01"),
                  value= as.Date("2014-11-30"),
                  timeFormat="%b %Y"),
                
                selectInput("choose_metric", "Choose Real Estate Metric", vars, selected = "hpi"),
                selectInput("choose_disaster", "Choose Disaster", locations, selected = "neworleans"),
                
                
                h5(htmlOutput("disaster_name")),
                h5(htmlOutput("city_name")),
                h6(htmlOutput("disaster_date")),
                
                plotly::plotlyOutput("bar_chart_neworleans"),
                plotly::plotlyOutput("line_chart_neworleans")
  ))))