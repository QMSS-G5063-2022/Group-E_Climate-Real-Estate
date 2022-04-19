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


navbarPage("Real Estate & Natural Disasters", id="nav",
           
           ### HOME PAGE ###
           
           tabPanel("Home Page",
                    h1("Real Estate amid Disaster: A Data Visualization Exploration"),
                    h1(" "),
                    h5("As climate change increases both the intensity and frequency of natural disasters, these consequences also have
                       immediate impacts on real estate, be that housing sale prices, rental prices, or home price indices.  In profiling
                       four notable natural disasters in the United States, along with one human-made disaster, we aim to understand
                       the immediate and short-term impact that natural destruction and catastrophe induces on the housing market.
                       
                       Is the housing market more resilient to certain types of disasters?  How quickly does it take to bounce back?
                       Our project explores these questions in an engaging manner."),
                    h1(" "),
                    img(src = "hurricane-katrina.jpg", align = "center"),
                    img(src='coffey-park-fire.jpg', align = "center"),
                    img(src='boulder-floods.jpg', align = "center"),
                    img(src='buffalo-snow.jpg', align = "center"),
                    img(src='moore-ok-tornado.jpg', align = "center"),
                    img(src='grand-isle-oil.jpg', align = "center")
           ),
           
           ### NEW ORLEANS ###
           tabPanel("New Orleans, LA - Hurricane",
                    plotOutput("plot")),
           
           ### COFFEY PARK ###
           tabPanel("Coffey Park, CA - Wildfires",
                    )
           ,
           
           ### DENVER ###
           tabPanel("Boulder, CO - Floods", plotOutput("plot")
           )
           ,
           
           ### BUFFALO ###
           tabPanel("Buffalo, NY - Snowstorm", plotOutput("plot")
           )
           ,
           
           ### MOORE, OK ###
           tabPanel("Moore, OK - Tornado", plotOutput("plot")
           )
           ,
           
           ### BP OIL SPILL ###
           tabPanel("Grand Isle, LA - BP Oil Spill", plotOutput("plot")
           )
           ,
           
           conditionalPanel("false", icon("crosshair")))