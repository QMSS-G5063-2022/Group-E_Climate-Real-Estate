function(input, output){
  
  # input sliders and drop downs
  chosen_month <- reactive({input$choose_month})
  chosen_location <- reactive({input$choose_disaster})
  chosen_metric <- reactive({input$choose_metric})
  
  ### load data sources ###
  single_family_homes <- read.csv("../data/single_family_homes_time_series.csv")
  
  # load shape files
  new_orleans_shape <- readOGR("../data/shape files", "New_Orleans")
  moore_ok_shape <- readOGR(dsn="./data/shape files", layer="Moore_OK")
  
  new_orleans_homes <- single_family_homes %>%
    select(region_id, date, value) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(date = date + 1) %>% # add a day so that it begins on the first of the month
    rename("zip_code" = "region_id", "median_sale_price" = "value") %>%
    mutate(zip_code = as.character(zip_code))
  
  new_orleans_shape <- new_orleans_shape[,-c(2,3,4,5,6,7,8,9)]
  
  new_new_orleans_shape <- new_orleans_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  new_orleans_data2 <- merge(new_new_orleans_shape,
                             new_orleans_homes,
                             by = "zip_code",
                             duplicateGeoms = T) %>% st_as_sf()
  
  interactive_map <- reactive({
    new_orleans_data2 %>%
      filter(date == chosen_month()) %>%
      leaflet()})
  
  # set up map
  bins <- c(100000, 200000, 300000, 400000, 500000)
  #pal <- colorBin("YlOrRd", domain=interactive_map()$median_sale_price, bins=bins)
  
  output$new_orleans_map <- leaflet::renderLeaflet({
    interactive_map() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -90.0715, lat = 29.95, zoom = 10) %>%
      addPolygons(
        #fillColor = ~pal(median_sale_price),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8))})
}