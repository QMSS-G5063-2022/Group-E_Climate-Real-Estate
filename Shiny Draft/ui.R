vars <- c(
  "Annual Home Price Index (HPI)" = "HPI",
  "Single Family Home Value" = "single_fam_val",
  "Annual Change in HPI (%)" = "annual_change")

# for grand isle, which doesn't have HPI data
vars_special <- c(  "Single Family Home Value" = "single_fam_val",
                    "Annual Change in HPI (%)" = "annual_change")

locations <- c("New Orleans - Hurricane" = "neworleans",
               "Coffey Park - Wildfires" = "coffeypark",
               "Buffalo, NY - Snowstorm" = "buffalo",
               "Moore, OK - Tornado" = "moore",
               "Grand Isle, LA - BP Oil Spill" = "grandisle")

navbarPage(h4("Major Disasters' Real Estate Effects"),
           id="nav",
           
           tabPanel(h6("Introduction"),
                    id = "intro",
                    
                    h2("Real Estate amid Disaster: A Data Visualization Exploration"),
                    h1(" "),
                    p("Rory Butler, Cindy Chen, Lizabeth Singh, Jeffray Tsai"),
                    h1(" "),
                    h5("As climate change and ongoing human events increases both the scale and frequency of severe environmental
                    events, these consequences also have immediate impacts on real estate, be that housing sale prices, rental prices, or home price indices.  In profiling
                       four (1) notable natural disasters in the United States, along with one human-made disaster (an oil spill), we aim to understand
                       the immediate and short-term impact that natural destruction and catastrophe induces on the housing market.
                       
                       Is the housing market more resilient to certain types of disasters?  How quickly does it take to bounce back?
                       How soon after a disaster is it the ideal time to buy a home and will the value recover?
                       Our project explores these questions in an engaging and interactive manner."),
                    h1(" "),
                    h5("Each subsequent tab in this app spotlights a unique disaster.  You will find an interactive map to explore and understand
                       various home value metrics by zip code.  Use the month/year slider to see how these metrics change over time."),
                    h1(" "),
                    img(id = "neworleanspic", src = "hurricane-katrina-png.png", align = "center", height = "15%", width = "15%"),
                    img(id = "coffeypic", src = 'coffey-park-fire-png.png', align = "center", height = "15%", width = "15%"),
                    img(id = "moorepic", src = 'moore-ok-tornado-png.png', align = "center", height = "15%", width = "15%"),
                    img(id = "buffalopic", src = 'buffalo-snow-png.png', align = "center", height = "15%", width = "15%"),
                    img(id = "grandislepic", src = 'grand-isle-oil-png.png', align = "center", height = "15%", width = "15%"),
                    useShinyjs()
           ),
           
           tabPanel(h6("New Orleans, LA"),
                    id = "neworleans_tab",
                    
                    div(class="outer",
                        id = "div1",
                        tags$head(includeCSS("./style_sheets/styles.css")),
                        
                        leaflet::leafletOutput("disaster_map_neworleans", height = "100%", width = "100%"),
         
         absolutePanel(id = "controls",
                       class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                
                h5("Hurricane"),
                h5("Aug 2005"),
                
                sliderInput(
                  inputId = "choose_month_neworleans", 
                  label = "Choose the month/year",
                  min = as.Date("2002-08-01"),
                  max = as.Date("2008-08-01"),
                  value= as.Date("2005-08-01"),
                  timeFormat="%b %Y"),
                
                selectInput("choose_metric_neworleans", "Choose Real Estate Metric", vars, selected = "annual_change"),
 
        plotly::plotlyOutput("line_chart_neworleans", height = 250),
         plotly::plotlyOutput("bar_chart_neworleans", height = 250)),
         
         tags$div(id="cite",
                  'Data compiled from FHFA and Zillow')
  )),
  
  tabPanel(h6("Coffey Park, CA"),
           id = "coffeypark_tab",
           
           div(class="outer",
               id = "div2",
               tags$head(includeCSS("./style_sheets/styles2.css")),
  
      leaflet::leafletOutput("disaster_map_coffeypark", height = "100%", width = "100%"),
      
      absolutePanel(id = "controls2",
                    class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h5("Wildfires"),
                    h5("Oct 2017"),
                    
                    sliderInput(
                      inputId = "choose_month_coffeypark", 
                      label = "Choose the month/year",
                      min = as.Date("2014-10-01"),
                      max = as.Date("2020-10-01"),
                      value= as.Date("2017-10-01"),
                      timeFormat="%b %Y"),
                    
                    selectInput("choose_metric_coffeypark", "Choose Real Estate Metric", vars, selected = "annual_change"),
                    
                    plotly::plotlyOutput("line_chart_coffeypark", height = 250),
                    plotly::plotlyOutput("bar_chart_coffeypark", height = 250)),
                    
                    tags$div(id="cite2",
                             'Data compiled from FHFA and Zillow')
                    
      )),
      
  tabPanel(h6("Moore, OK"),
           id = "moore_tab",
           
           div(class="outer",
               id = "div3",
               
               tags$head(includeCSS("./style_sheets/styles3.css")),
           
           leaflet::leafletOutput("disaster_map_moore", height = "100%", width = "100%"),
           
           absolutePanel(id = "controls3", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                         width = 330, height = "auto",
                         
                        h5("Tornado"),
                        h5("May 2013"),
           
           sliderInput(
             inputId = "choose_month_moore", 
             label = "Choose the month/year",
             min = as.Date("2010-05-01"),
             max = as.Date("2016-05-01"),
             value= as.Date("2013-05-01"),
             timeFormat="%b %Y"),
           
           selectInput("choose_metric_moore", "Choose Real Estate Metric", vars, selected = "annual_change"),
           
           plotly::plotlyOutput("line_chart_moore", height = 250),
           plotly::plotlyOutput("bar_chart_moore", height = 250),
           
           tags$div(id="cite3",
                    'Data compiled from FHFA and Zillow')
           ))),
  
  tabPanel(h6("Buffalo, NY"),
           id = "buffalo_tab",
           div(class="outer",
               id = "div4",
               tags$head(includeCSS("./style_sheets/styles4.css")),
               
               leaflet::leafletOutput("disaster_map_buffalo", height = "100%", width = "100%"),
               
               absolutePanel(id = "controls4", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             h5("Snowstorm"),
                             h5("Nov 2014"),
                             
                             sliderInput(
                               inputId = "choose_month_buffalo", 
                               label = "Choose the month/year",
                               min = as.Date("2011-11-01"),
                               max = as.Date("2017-11-01"),
                               value= as.Date("2014-11-01"),
                               timeFormat="%b %Y"),
                             
                             selectInput("choose_metric_buffalo", "Choose Real Estate Metric", vars, selected = "annual_change"),
                             
                             plotly::plotlyOutput("line_chart_buffalo", height = 250),
                            plotly::plotlyOutput("bar_chart_buffalo", height = 250)),
               
               tags$div(id="cite4",
                        'Data compiled from FHFA and Zillow')
           )),
  tabPanel(h6("Grand Isle, LA"),
           id = "grandisle_tab",

           div(class="outer",
               id = "div5",
               tags$head(includeCSS("./style_sheets/styles5.css")),
               
               leaflet::leafletOutput("disaster_map_grandisle", height = "100%", width = "100%"),
               
               absolutePanel(id = "controls5", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                             width = 330, height = "auto",
                             
                             h5("BP Oil Spill"),
                             h5("Apr 2010"),
                             
                             sliderInput(
                               inputId = "choose_month_grandisle", 
                               label = "Choose the month/year",
                               min = as.Date("2007-04-01"),
                               max = as.Date("2013-04-01"),
                               value= as.Date("2010-04-01"),
                               timeFormat="%b %Y"),
                             
                             selectInput("choose_metric_grandisle", "Choose Real Estate Metric", vars_special, selected = "annual_change"),
                             
                             plotly::plotlyOutput("line_chart_grandisle", height = 250),
                             plotly::plotlyOutput("bar_chart_grandisle", height = 250)),
                             
                             tags$div(id="cite5",
                                      'Data compiled from FHFA and Zillow')
                             
               )),
  tabPanel(h6("Insights",
              id = "insights_tab")))
                                