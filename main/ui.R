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
           
           setBackgroundColor(
             color = "#36393B",
             gradient = c("linear", "radial"),
             direction = c("bottom", "top", "right", "left"),
             shinydashboard = FALSE
           ),
           
           tabPanel(h6(strong("Introduction"), style = "color: #F26430"),
                    id = "intro",
                    
                    fluidRow(h1(strong("REAL ESTATE AMID DISASTER: A VISUAL EXPLORATION"), style="color:#FFD16")),
                    fluidRow(h1(" ")),
                    fluidRow(p("Rory Butler, Cindy Chen, Lizabeth Singh, Jeffray Tsai"), style="color: #FFD166"), 
                    h1(" "),
                    fluidRow(
                      column(1, " "),
                      column(2, img(id = "neworleanspic", src = "hurricane-katrina-png.png", align = "center", height = "80%", width = "80%")),
                      column(2, img(id = "coffeypic", src = 'coffey-park-fire-png.png', align = "center", height = "80%", width = "80%")),
                    column(2, img(id = "moorepic", src = 'moore-ok-tornado-png.png', align = "center", height = "80%", width = "80%")),
                    column(2, img(id = "buffalopic", src = 'buffalo-snow-png.png', align = "center", height = "80%", width = "80%")),
                    column(2, img(id = "grandislepic", src = 'grand-isle-oil-png.png', align = "center", height = "80%", width = "80%")),
                    column(1," ")),
                    
                    br(),
                    fluidRow(
                    tabsetPanel(id = "inside",
                                type = "tabs",
                                
                                tabPanel(h6(strong("Project Purpose"), style = "color:#F26430;"),
                                        
                                         column(7,
                                         span(h4("As climate change and ongoing human events intensify both the scale and frequency of severe environmental
                                          events, these consequences also have immediate impacts on real estate, be that housing sale price or home price indices.  In profiling
                                          four (1) notable natural disasters in the United States, along with one human-made disaster (an oil spill), we aim to understand
                                          the immediate and short-term impact that natural destruction and catastrophe induces on the housing market.", style = "color:white;text-align:justify")),
                                          h4(" - How is the real estate valuation market affected by natural and man-made disasters (if at all)?", style = "color:white;text-align:justify"),
                        h4(" - If that's the case, is the housing market more vulnerable to certain types of disasters?", style = "color:white;text-align:justify"),
                        h4(" - How quickly does it take to bounce back?", style = "color:white;text-align:justify"),
                        h4(" - How soon after a disaster is it the ideal time to buy a home and will the value recover?", style = "color:white;text-align:justify"),
                        h4(" - Our project explores these questions in an engaging and interactive manner.", style = "color:white;text-align:justify")),
                        column(5, " ")),

                    tabPanel(h6(strong("HOW TO USE THIS SHINY APP"), style = "color:#F26430"),
                             
                             column(7,
                             h4("Each subsequent tab in this app spotlights a unique disaster:"),
                    h4(" - Use the month/year slider to see how these real estate valus change over time before/after the disaster."),
                    h4(" - Toggle between real estate metrics like home price index (HPI), mean single family home value, and % annual HPI change."),
                    h4(" - Hover over the interactive map to explore zip codes' real estates "),
                    h4(" - Click on the zip code to see the home value impact of immediately before and after the disaster for that specific zip code"),
                    h4(" - View the complementary plots to understand how the disaster changed real estate values 12 months before and after its consequences."),
                    h4(" - Read through the Insights tab to see our notable trends from our data.")),
                    column(5, " ")),
                    
                    tabPanel(h6(strong("DEFINITIONS"), style = "color:#F26430"),
                             fluidRow(column(1, " "),
                                      column(6, 
                      fluidRow(h1(" "),
                               p(strong("What is Home Price Value?", style = "font-style:italic;text-align:justify;color:black;background-color:#FFD166;padding:15px")),
                      p("Home Price Value (HPI) is a weighted sales index that incorporates average price changes in repeat sales or refinancings on the same properties.", style = "font-style:italic;text-align:justify;color:black;background-color:papayawhip;padding:15px")),
                      br(),
                      fluidRow(p(strong("What is Single Family Home Value?", style = "font-style: italic;text-align:justify;color:black;background-color:#FFD166;padding:15px")),
                      p("Single Family Home Value is the selling price for detached single-family properties.", 
                        style = "font-style:italic;text-align:justify;color:black;background-color:papayawhip;padding:15px;"))),
                      column(5, fluidRow()))
                    ))),
                    br(),
                    br(),
                    fluidRow(h4(strong("**IT TAKES A MOMENT FOR THE DATA TO LOAD THE FIRST TIME YOU SWITCH TABS**"),
                                style = "color: white;background-color: #F26430;text-align:center;padding:15px"))
                    ),
           tabPanel(h6(strong("Insights | Commentary"), style = "color:#F26430"),
                    id = "insights_tab",
                    
                    fluidRow(
                      column(7,
                             h2(strong("INSIGHTS BY DISASTER EVENT"),style="color: #FFD166"),
                             br(),
                             br(),
                             
                             tabsetPanel(type = "tabs",
                                         tabPanel(
                                           h6(strong("New Orleans, LA"), style = "color:#F26430"),
                                           fluidRow(12, h2(strong("NEW ORLEANS, LA - HURRICANE"), style = "padding:15px;color:#FFD166;background-color:#36393B")),
                                           fluidRow(column(10,
                                                           h4(" - Single family home values grew steadily, and continued to do so after Hurrican Katrina hit."), style="color: #FFD166;font-weight:bold;font-size:14px",
                                                           h4(" - The annual home price index (HPI) grew bit by bit, but stagnated around a year after Hurricane Katrina came."),style="color: #FFD166;font-weight:bold;font-size:14px",
                                                           h4(" - The rolling HPI percentage was increasing until 2004, but was already decreasing before Hurricane Katrina hit."),style="color:#FFD166;font-weight:bold;font-size:14px",
                                                           h4(" - The affluent Garden District neighborhood of New Orleans (zip codes 70115 and 70130) had a substantial increase in price after Hurricane Katrina, 
                    suggesting their home value resilience amid disaster."),style="color:#FFD166;font-weight:bold;font-size:14px"),
                                                    column(2, " "))),
                                         
                                         tabPanel(
                                           h6(strong("Coffey Park, CA"), style = "color:#F26430"),
                                           fluidRow(12, h2(strong("COFFEY PARK, CA - SNELL WILDFIRES"), style = "padding:15px;color:#FFD166;background-color:#36393B")),
                                           fluidRow(column(10,
                                                           h4(" - While the Snell wildfires began burning in Sep 2018, the gradual devastation of its destruction is clear on all metrics."),style="color:#FFD166;font-weight:bold;font-size:14px",
                                                           h4(" - The average annual % change in HPI plummeted to 0% shortly after Sep 2018 (indicating flat growth in home sales)."),style="color:#FFD166;font-weight:bold;font-size:14px",
                                                           h4(" - Average single family home values remained below $200K until 2020, when the pandemic yielded greater single family housing demand."),style="color:#FFD166;font-weight:bold;font-size:14px",
                                                           h4(" - On a zip code level, zip codes East of Coffey Park were the hardest hit by the fires based on annual % change in HPI."),style="color:#FFD166;font-weight:bold;font-size:14px"),
                                                    column(2, ""))),
                                         
                                         tabPanel(
                                           h6(strong("Moore, OK"), style = "color:#F26430"),
                                           fluidRow(12, h2(strong("MOORE, OK - TORNADO"), style = "padding:15px;color:#FFD166;background-color:#36393B")),
                                           fluidRow(column(10,
                                                           h4(" - The tornado led to a brief dip in the value of single family homes, likely because tornadoes did not appear in Moore again, reassuring prospective homebuyers that Moore would not be hit again."),style="color:#FFD166;font-weight:bold;text-align:justify",
                                                           h4(" - Since the disaster, single family home prices stagnated until 2015, and has since skyrocketed."),style="color:#FFD166;font-weight:bold;text-align:justify",
                                                           h4(" - Meanwhile, the annual % change in HPI has been positive since 2013 when the tornado hit."),style="color:#FFD166;font-weight:bold;text-align:justify",
                                                           h4(" - The only period of time when the annual change in home prices was ever negative was around the 2008 to 2012."),style="color:#FFD166;font-weight:bold;text-align:justify",
                                                           h4(" - On a zip code level, there were no notable discrepancies between areas when it came to real estate value effects."),style="color:#FFD166;font-weight:bold;text-align:justify"),
                                                    column(2, " "))),
                                         
                                         tabPanel(
                                           h6(strong("Buffalo, NY"), style = "color:#F26430"),
                                           fluidRow(12, h2(strong("BUFFALO, NY - SNOWSTORM"), style = "padding:15px;color:#FFD166;background-color:#36393B")),
                                           fluidRow(column(10,
                                                           h4(" - Despite the severity of the snowstorm, this disaster did not affect real estate values as all average metrics continued to increase on average."),style="color:#FFD166;font-weight:bold;text-align:justify",
                                                           h4(" - Single family home prices continue to rise even with the snowstorm, so do the HPI and rolling HPI % change."),style="color:#FFD166;font-weight:bold;text-align:justify",
                                                           h4(" - Southern suburbs were more heavily impacted by the blizzard (like zip codes 14218, 14219, and 14224);
              we can see this in the Rolling Annual Change % in HPI by zip code as these zip codes become red immediately after the storm."),style="color:#FFD166;font-weight:bold;text-align:justify",
                                                           h4(" - Since Buffalo is accustomed to heavy snow and snowstorms, it is noticeable that there will either be a normal increase or no change to home prices."), style="color:#FFD166;font-weight:bold;text-align:justify"),
                                                    column(2, " "))),
                                         
                                         tabPanel(
                                           h6(strong("Grand Isle, LA"), style = "color:#F26430"),
                                           fluidRow(12, h2(strong("GRAND ISLE, LA - BP OIL SPILL"), style = "padding:15px;color:#FFD166;background-color:#36393B")),
                                           fluidRow(column(8, h4(" - After the infamous BP Oil Spill in Apr 2010, Grand Isle's single family (vacation) homes' values steadily declined."),style="color:#FFD166;font-weight:bold;text-align:justify",
                                                           h4(" - While home values' rolling 12 months' change had rallied into positive growth the year before the spill, the incident led to a firm decline shortly afterward."),style="color:#FFD166;font-weight:bold;text-align:justify",
                                                           h4(" - Home prices only improved in 2016, likely when a significant portion of the oil had been cleaned."), style="color:#FFD166;font-weight:bold;text-align:justify",
                                                           h4(" - The rolling annual % change in single family home value decreased for around two years and rose up again in 2012."),style="color:white;font-weight:bold;font-size:14px;text-align:justify"),
                                                    column(4, " ")))
                             )),
                      
                      
                      column(5,
                             fluidRow()),
                      column(4,
                             fluidRow()
                      ))),
           
           tabPanel(h6("New Orleans, LA", style = "color: #F26430"),
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
        br(),
        plotly::plotlyOutput("bar_chart_neworleans", height = 250)),
         
         tags$div(id="cite",
                  'Data compiled from FHFA and Zillow')
  )),
  
  tabPanel(h6("Coffey Park, CA", style = "color:#F26430"),
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
                    br(),
                    plotly::plotlyOutput("bar_chart_coffeypark", height = 250)),
                    
                    tags$div(id="cite2",
                             'Data compiled from FHFA and Zillow')
                    
      )),
      
  tabPanel(h6("Moore, OK", style = "color: #F26430"),
           id = "moore_tab",
           
           div(class="outer",
               id = "div3",
               
               tags$head(includeCSS("./style_sheets/styles3.css")),
           
           leaflet::leafletOutput("disaster_map_moore", height = "100%", width = "100%"),
           
           absolutePanel(id = "controls3", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 100, left = 20, right = "auto", bottom = "auto",
                         width = 330, height = "auto",
                         
                        h5(strong("TORNADO")),
                        h5(strong("Disaster Date: May 2013")),
           
           sliderInput(
             inputId = "choose_month_moore", 
             label = "Choose the month/year",
             min = as.Date("2010-05-01"),
             max = as.Date("2016-05-01"),
             value= as.Date("2013-05-01"),
             timeFormat="%b %Y"),
           
           selectInput("choose_metric_moore", "Choose Real Estate Metric", vars, selected = "annual_change"),
           
           plotly::plotlyOutput("line_chart_moore", height = 250),
           br(),
           plotly::plotlyOutput("bar_chart_moore", height = 250),
           
           tags$div(id="cite3",
                    'Data compiled from FHFA and Zillow')
           ))),
  
  tabPanel(h6("Buffalo, NY", style = "color: #F26430"),
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
                             br(),
                            plotly::plotlyOutput("bar_chart_buffalo", height = 250)),
               
               tags$div(id="cite4",
                        'Data compiled from FHFA and Zillow')
           )),
  tabPanel(h6("Grand Isle, LA", style = "color:#F26430"),
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
                             br(),
                             plotly::plotlyOutput("bar_chart_grandisle", height = 250)),
                             
                             tags$div(id="cite5",
                                      'Data compiled from FHFA and Zillow')
                             
               )),
  tabPanel(h6(strong("Explore the Data"), style = "color: #F26430"),
           id = "datatable_tab",
           
           h1(strong("Explore the Data"), style = "color:#FFD166"),
           dataTableOutput("my_data_table")
           ))
                                