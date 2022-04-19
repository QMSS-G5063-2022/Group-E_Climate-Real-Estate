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
                    plotOutput("plot")
           ),
           
           ### NEW ORLEANS ###
           tabPanel("New Orleans, LA - Hurricane",
                    plotOutput("plot")),
           
           ### COFFEY PARK ###
           tabPanel("Coffey Park, CA - Wildfires", plotOutput("plot")
                    )
           ,
           
           ### DENVER ###
           tabPanel("Denver, CO - Floods", plotOutput("plot")
           )
           ,
           
           ### BUFFALO ###
           tabPanel("Buffalo, NY - Snowstorm", plotOutput("plot")
           )
           ,
           
           ### MOORE, OK ###
           tabPanel("Moore, OK - Snowstorm", plotOutput("plot")
           )
           ,
           
           ### BP OIL SPILL ###
           tabPanel("Grand Isle, LA - BP Oil Spill", plotOutput("plot")
           )
           ,
           
           conditionalPanel("false", icon("crosshair"))
)