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
                    town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
                    bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 
                    pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
                    color_id1 <- pal1(town$id)
                    content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
                    townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
                      addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
                      addProviderTiles("Stamen.TonerLite") %>% 
                      addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price"))
          ,
           
           ### COFFEY PARK ###
           tabPanel("Coffey Park, CA - Wildfires",
                    town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
                    bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 
                    pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
                    color_id1 <- pal1(town$id)
                    content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
                    townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
                      addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
                      addProviderTiles("Stamen.TonerLite") %>% 
                      addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price"))
           ,
           
           ### DENVER ###
           tabPanel("Boulder, CO - Floods", plotOutput("plot")
                    town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
                    bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 
                    pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
                    color_id1 <- pal1(town$id)
                    content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
                    townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
                      addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
                      addProviderTiles("Stamen.TonerLite") %>% 
                      addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price"))
           ,
           
           ### BUFFALO ###
           tabPanel("Buffalo, NY - Snowstorm", plotOutput("plot")
                    town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
                    bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 
                    pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
                    color_id1 <- pal1(town$id)
                    content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
                    townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
                      addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
                      addProviderTiles("Stamen.TonerLite") %>% 
                      addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price"))
           ,
           
           ### MOORE, OK ###
           tabPanel("Moore, OK - Tornado", plotOutput("plot")
                    town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
                    bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 
                    pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
                    color_id1 <- pal1(town$id)
                    content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
                    townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
                      addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
                      addProviderTiles("Stamen.TonerLite") %>% 
                      addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price"))
           ,
           
           ### BP OIL SPILL ###
           tabPanel("Grand Isle, LA - BP Oil Spill", plotOutput("plot")
                    town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
                    bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 
                    pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
                    color_id1 <- pal1(town$id)
                    content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
                    townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
                      addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
                      addProviderTiles("Stamen.TonerLite") %>% 
                      addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price"))
           ,
           
           conditionalPanel("false", icon("crosshair")))