library(leaflet)
library(RColorBrewer)
library(scales)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
dataset1 <- read.csv("~\..\data\buffalo.csv")
dataset2 <- read.csv("~\..\data\buffalo.csv")

function(input, output, session){
  
  ### NEW ORLEANS ###
  
  town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
  
  bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 

  pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
  
  color_id1 <- pal1(town$id)
  
  content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
  
  townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
    addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
    addProviderTiles("Stamen.TonerLite") %>% 
    addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price")

  ### COFFEY PARK ###

town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 
pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
color_id1 <- pal1(town$id)
content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
  addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
  addProviderTiles("Stamen.TonerLite") %>% 
  addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price")

### DENVER ###

town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 
pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
color_id1 <- pal1(town$id)
content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
  addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
  addProviderTiles("Stamen.TonerLite") %>% 
  addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price")

### BUFFALO ###

town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 
pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
color_id1 <- pal1(town$id)
content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
  addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
  addProviderTiles("Stamen.TonerLite") %>% 
  addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price")

### MOORE, OK ####

town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 
pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
color_id1 <- pal1(town$id)
content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
  addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
  addProviderTiles("Stamen.TonerLite") %>% 
  addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price")

### GRAND ISLE ###
town <- merge(dataset1, dataset2, by='zipcode',duplicateGeoms=TRUE)
bins <- bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf) 
pal1 <- colorBin("YlOrRd",domain = town$id, bins=bins)
color_id1 <- pal1(town$id)
content1 <- paste("Neighborhood:", town$neighbourhood,"<br/>") #Popup content to annotate the neighborhoods. 
townmap <- leaflet(town) %>% setView(-0, 0, zoom = 10) %>% 
  addPolygons(stroke=TRUE, smoothFactor = 0.5, weight=1, color=color_id1,popup=content1, opacity=1, fillColor=~colorQuantile('YlOrRd', town$id)(town$id),fillOpacity=1) %>% 
  addProviderTiles("Stamen.TonerLite") %>% 
  addLegend(pal=pal1, values= ~town$id, title = "Zipcode Mean of Real Estate Price")

}