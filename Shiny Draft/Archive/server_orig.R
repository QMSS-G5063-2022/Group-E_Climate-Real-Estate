function(input, output){
  
  # all the date sliders
  chosen_month <- reactive({input$choose_month})

  ### NEW ORLEANS ###
  new_orleans_homes <- read.csv("../data/single_family_homes_time_series.csv")
  new_orleans_shape <- readOGR("../data/shape files", "New_Orleans")
  
  new_orleans_homes <- new_orleans_homes %>%
    select(region_id, date, value) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    rename("zip_code" = "region_id", "median_sale_price" = "value") %>%
    mutate(zip_code = as.character(zip_code))

  new_orleans_shape <- new_orleans_shape[,-c(2,3,4,5,6,7,8,9)]
  
  new_new_orleans_shape <- new_orleans_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

  new_orleans_data2 <- merge(new_new_orleans_shape,
                             new_orleans_homes,
                             by = "zip_code",
                             duplicateGeoms = T) %>%
    st_as_sf()
  
  output$new_orleans_map <- leaflet::renderLeaflet({
  leaflet() %>%
      addProviderTiles("Stamen.Watercolor") %>%
      setView(lng = -100, lat = 50, zoom = 2)})
  
  ### COFFEY PARK ###

  # load data
  cali_rent <- read.csv("../data/single_family_homes_time_series.csv")
  cali_shape <- readOGR("../data/shape files", "California_Zip_Codes")
  
  # transform data - rent data
  cali_rent <- cali_rent %>%
    select(region_id, date, value) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    rename("zip_code" = "region_id", "median_sale_price" = "value") %>%
    mutate(zip_code = as.character(zip_code))
  
  # transform data - shape file
  cali_shape <- cali_shape[,-c(1,4,5,6,7,8,9)]
  
  new_cali_shape <- cali_shape %>%
    rename(zip_code = ZIP_CODE, po_name = PO_NAME) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # merge data sources
  cali_data <- merge(new_cali_shape, cali_rent, by = "zip_code", duplicateGeoms = T)
  
  new_cali_data_2 <- cali_data %>%
    filter(zip_code %in% c('95401', '95402', '95403', '95404', '95405', '95406', '95407', '95409')) %>%
    st_as_sf()
  
  cali_interactive <- reactive({new_cali_data_2 %>%
    filter(date == chosen_month()) %>%
      leaflet()})
  
  # set up map
  bins <- c(100000, 200000, 300000, 400000, 500000)
  pal <- colorBin("YlOrRd", domain=cali_interactive()$median_sale_price, bins=bins)
  
  ### LABELS NEED TO CHANGE BASED ON TIME PERIOD ###
  labels <- sprintf(
    "<strong>%s</strong><br/>Mean Rent: %g",
  cali_interactive()$zip_code, cali_interactive()$median_sale_price) %>%
    lapply(htmltools::HTML)
  
  output$coffey_park_map <- leaflet::renderLeaflet({
      cali_interactive() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(median_sale_price),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8))
        #label = labels,
        #labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
         #                           textsize = "15px",
          #                          direction = "auto")) %>%
      #addLegend(pal=pal, values = ~median_sale_price, opacity = 0.8, title = "Mean Rent")
    })

  ### DENVER ###


  ### BUFFALO ###


  ### MOORE, OK ####
  moore_ok <- base_real_estate %>%
    filter(zip_code %in% c(73160, 73165, 73170),
           year %in% c(2011, 2012, 2013, 2014, 2015))
  
  moore_ok_shape <- readOGR(dsn="./data/shape files", layer="Moore_OK")
  
  moore_ok_shape <- moore_ok_shape[,-c(2,3,4,5,6,7,8,9)]
  
  moore_ok_shape2 <- moore_ok_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # merge data
  moore_ok_data2 <- merge(moore_ok_shape2, moore_ok, by = "zip_code", duplicateGeoms = T) %>%
    st_as_sf()


  ### GRAND ISLE ###

}