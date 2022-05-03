vars <- c(
  "Annual Home Price Index (HPI)" = "HPI",
  "Single Family Home Value" = "single_fam_val",
  "Rolling Annual Change in HPI (%)" = "annual_change")

# for grand isle, which doesn't have HPI data
vars_special <- c(  "Single Family Home Value" = "single_fam_val",
                    "Rolling Annual Change in Single Family Home Value (%)" = "annual_change")

locations <- c("New Orleans - Hurricane" = "neworleans",
               "Coffey Park - Wildfires" = "coffeypark",
               "Buffalo, NY - Snowstorm" = "buffalo",
               "Moore, OK - Tornado" = "moore",
               "Grand Isle, LA - BP Oil Spill" = "grandisle")

navbarPage(h4(strong("THE REAL ESTATE EFFECTS OF MAJOR DISASTERS")),
           id="nav",
           
           tabPanel(h6(strong("Introduction"), style = "color: #e0194d"),
                    id = "intro",
                    
                    h2(strong("REAL ESTATE AMID DISASTER: A VISUAL EXPLORATION")),
                    h1(" "),
                    p("Rory Butler, Cindy Chen, Lizabeth Singh, Jeffray Tsai"),
                    h1(" "),
                    fluidRow(
                      splitLayout(cellsWidths = c("38%", "38%", "38%", "38%", "38%"),
                    img(id = "neworleanspic", src = "hurricane-katrina-png.png", align = "center", height = "55%", width = "55%"),
                    img(id = "coffeypic", src = 'coffey-park-fire-png.png', align = "center", height = "55%", width = "55%"),
                    img(id = "moorepic", src = 'moore-ok-tornado-png.png', align = "center", height = "55%", width = "55%"),
                    img(id = "buffalopic", src = 'buffalo-snow-png.png', align = "center", height = "55%", width = "55%"),
                    img(id = "grandislepic", src = 'grand-isle-oil-png.png', align = "center", height = "55%", width = "55%"))),
                    h4(strong("PROJECT PURPOSE")),
                    h5("As climate change and ongoing human events intensify both the scale and frequency of severe environmental
                    events, these consequences also have immediate impacts on real estate, be that housing sale prices, rental prices, or home price indices.  In profiling
                       four (1) notable natural disasters in the United States, along with one human-made disaster (an oil spill), we aim to understand
                       the immediate and short-term impact that natural destruction and catastrophe induces on the housing market."),
                        tags$li("How is the real estate valuation market affected by natural and man-made disasters (if at all)?"),
                        tags$li("If that's the case, is the housing market more vulnerable to certain types of disasters?"),
                        tags$li("How quickly does it take to bounce back?"),
                        tags$li("How soon after a disaster is it the ideal time to buy a home and will the value recover?"),
                        tags$li("Our project explores these questions in an engaging and interactive manner."),
                         
                    h1(" "),
                    h4(strong("HOW TO USE THIS SHINY APP")),
                    h5("Each subsequent tab in this app spotlights a unique disaster:"),
                    tags$li("Use the month/year slider to see how these real estate valus change over time before/after the disaster."),
                    tags$li("Toggle between real estate metrics like home price index (HPI), mean single family home value, and % annual HPI change."),
                    tags$li("Hover over the interactive map to explore zip codes' real estates "),
                    tags$li("View the complementary plots to understand how the disaster changed real estate values 12 months before and after its consequences."),
                    tags$li("Read through the Insights tab to see our notable trends from our data."),
                    h4(strong("**IT TAKES A MOMENT FOR THE DATA TO LOAD THE FIRST TIME YOU SWITCH TABS**"), style = "color: red")
           ),
           
           tabPanel(h6("New Orleans, LA", style = "color: #d61849"),
                    id = "neworleans_tab",
                    
                    div(class="outer",
                        id = "div1",
                        tags$head(includeCSS("./style_sheets/styles.css")),
                        
                        leaflet::leafletOutput("disaster_map_neworleans", height = "100%", width = "100%"),
         
         absolutePanel(id = "controls",
                       class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 100, left = 20, right = "auto", bottom = "auto",
                width = 330, height = "auto",
                
                h5(strong("HURRICANE")),
                h5(strong("Aug 2005")),
                
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
  
  tabPanel(h6("Coffey Park, CA", style = "color:#c71644"),
           id = "coffeypark_tab",
           
           div(class="outer",
               id = "div2",
               tags$head(includeCSS("./style_sheets/styles2.css")),
  
      leaflet::leafletOutput("disaster_map_coffeypark", height = "100%", width = "100%"),
      
      absolutePanel(id = "controls2",
                    class = "panel panel-default", fixed = TRUE,
                    draggable = TRUE, top = 100, left = 20, right = "auto", bottom = "auto",
                    width = 330, height = "auto",
                    
                    h5(strong("SNELL WILDFIRE")),
                    h5(strong("Sep 2018")),
                    
                    sliderInput(
                      inputId = "choose_month_coffeypark", 
                      label = "Choose the month/year",
                      min = as.Date("2015-09-01"),
                      max = as.Date("2021-09-01"),
                      value= as.Date("2018-09-01"),
                      timeFormat="%b %Y"),
                    
                    selectInput("choose_metric_coffeypark", "Choose Real Estate Metric", vars, selected = "annual_change"),
                    
                    plotly::plotlyOutput("line_chart_coffeypark", height = 250),
                    plotly::plotlyOutput("bar_chart_coffeypark", height = 250)),
                    
                    tags$div(id="cite2",
                             'Data compiled from FHFA and Zillow')
                    
      )),
      
  tabPanel(h6("Moore, OK", style = "color: #b5123c"),
           id = "moore_tab",
           
           div(class="outer",
               id = "div3",
               
               tags$head(includeCSS("./style_sheets/styles3.css")),
           
           leaflet::leafletOutput("disaster_map_moore", height = "100%", width = "100%"),
           
           absolutePanel(id = "controls3", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 100, left = 20, right = "auto", bottom = "auto",
                         width = 330, height = "auto",
                         
                        h5(strong("TORNADO")),
                        h5(strong("May 2013")),
           
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
  
  tabPanel(h6("Buffalo, NY", style = "color: #a30f35"),
           id = "buffalo_tab",
           div(class="outer",
               id = "div4",
               tags$head(includeCSS("./style_sheets/styles4.css")),
               
               leaflet::leafletOutput("disaster_map_buffalo", height = "100%", width = "100%"),
               
               absolutePanel(id = "controls4", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 100, left = 20, right = "auto", bottom = "auto",
                             width = 330, height = "auto",
                             
                             h5(strong("SNOWSTORM")),
                             h5(strong("Nov 2014")),
                             
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
  tabPanel(h6("Grand Isle, LA", style = "color: #960f32"),
           id = "grandisle_tab",

           div(class="outer",
               id = "div5",
               tags$head(includeCSS("./style_sheets/styles5.css")),
               
               leaflet::leafletOutput("disaster_map_grandisle", height = "100%", width = "100%"),
               
               absolutePanel(id = "controls5", class = "panel panel-default", fixed = TRUE,
                             draggable = TRUE, top = 100, left = 20, right = "auto", bottom = "auto",
                             width = 330, height = "auto",
                             
                             h5(strong("BP OIL SPILL")),
                             h5(strong("Apr 2010")),
                             
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
  tabPanel(h6(strong("Insights | Commentary"), style = "color:#96114d"),
              id = "insights_tab",
              
            h2(strong("INSIGHTS BY DISASTER EVENT")),
              h1(" "),
           
            h4(strong("NEW ORLEANS, LA - HURRICANE")),
              tags$li("After Hurricane Katrina hit, single "),
           h1(""),
           
            h4(strong("COFFEY PARK, CA - SNELL WILDFIRES")),
           tags$li("While the Snell wildfires began burning in Sep 2018, the gradual devastation of its destruction is clear on all metrics."),
           tags$li("The average annual % change in HPI plummeted to 0% shortly after Sep 2018 (indicating flat growth in home sales)."),
           tags$li("Average single family home values remained below $200K until 2020, when the pandemic yielded greater single family housing demand."),
           tags$li("On a zip code level, zip codes East of Coffey Park were the hardest hit by the fires based on annual % change in HPI."),
           h1(""),
           
            h4(strong("MOORE, OK - TORNADO")),
           tags$li("The tornado led to a brief dip in the value of single family homes, likely because tornadoes did not appear in Moore again, reassuring prospective homebuyers that Moore would not be hit again."),
           tags$li("Since the disaster, single family home prices stagnated until 2015, and has since skyrocketed."),
           tags$li("Meanwhile, the annual % change in HPI has been positive since 2013 when the tornado hit."),
           tags$li("The only period of time when the annual change in home prices was ever negative was around the 2008 to 2012."),
           tags$li("On a zip code level, there were no notable discrepancies between areas when it came to real estate value effects."),
           h1(""),
           
            h4(strong("BUFFALO, NY - SNOWSTORM")),
           tags$li("Despite the severity of the snowstorm, this disaster did not affect real estate values as all average metrics continued to increase on average."),
           tags$li("Southern suburbs were more heavily impacted by the blizzard (like zip codes 14218, 14219, and 14224);
              we can see this in the Rolling Annual Change % in HPI by zip code as these zip codes become red immediately after the storm."),
           
            h4(strong("GRAND ISLE, LA - BP OIL SPILL")),
           tags$li("After the infamous BP Oil Spill in Apr 2010, Grand Isle's single family (vacation) homes' values steadily declined."),
           tags$li("While home values' rolling 12 months' change had rallied into positive growth the year before the spill, the incident led to a firm decline shortly afterward."),
           tags$li("Home prices only improved in 2016, likely when a significant portion of the oil had been cleaned."),
           h1(""),
           h1(""),
           h1(""),
           h1(""),
           
           p(strong("What is Home Price Value?", style = "font-style: italic")),
           p("Home Price Value (HPI) is a weighted sales index that incorporates average price changes in repeat sales or refinancings on the same properties.", style = "font-style: italic")
           ))
                                