library(leaflet)
library(RColorBrewer)
library(scales)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
#zipdata <- read.csv("~\..\data\buffalo.csv")
