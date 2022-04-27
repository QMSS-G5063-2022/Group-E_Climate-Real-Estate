function(input, output){
  
  ### input sliders and drop downs ###
  chosen_month <- reactive({input$choose_month})
  chosen_location <- reactive({input$choose_disaster})
  chosen_metric <- reactive({input$choose_metric})
  
  ### load data sources ###
  single_family_homes <- read.csv("../data/single_family_homes_time_series.csv")
  HPI <- read.csv("../data/HPI_data.csv")
  bottom_tier <- read.csv("../data/all_homes_bottom_tier.csv")
  
  ### load shape files ###
  new_orleans_shape <- readOGR("../data/shape files", "New_Orleans")
  new_orleans_shape <- new_orleans_shape[,-c(2,3,4,5,6,7,8,9)]
  
  moore_ok_shape <- readOGR(dsn="../data/shape files", layer="Moore_OK")
  moore_ok_shape <- moore_ok_shape[,-c(2,3,4,5,6,7,8,9)]
  
  cali_shape <- readOGR("../data/shape files", "California_Zip_Codes")
  cali_shape[,-c(1,4,5,6,7,8,9)]
  
  grand_isle_shape <- readOGR("../data/shape files", "Grand_Isla_LA")
  grand_isle_shape <- grand_isle_shape[,-c(2,3,4,5,6,7,8,9)]
  
  buffalo_shape <- readOGR("../data/shape files", "Buffalo")
  buffalo_shape <- buffalo_shape[,-c(2,3,4,5,6,7,8,9)]
  
  ZCTA_list <- c('70112', '70113', '70114', '70115', '70116', '70117', '70118', '70119',
                 '70121', '70122', '70123', '70124', '70125', '70126', '70127', '70128',
                 '70129', '70130','70131', '70139', '70163', '95401', '95403', '95404',
                 '95405', '95407', '95409', '14201', '14202', '14203', '14204', '14206',
                 '14207', '14208', '14209', '14210', '14211', '14212', '14213', '14214',
                 '14215', '14216', '14217', '14218', '14219', '14220', '14221','14222',
                 '14223', '14224', '14225', '14226', '14227', '14228', '14261', '73160',
                 '73165', '73170', '70358')
  
  ### data manipulation ###
  
  ## HPI ##
  HPI <- HPI %>%
    filter(zip_code %in% ZCTA_list) %>%
    filter(year > 2002)
  
  ## single family homes ##
  single_family_homes <- single_family_homes %>%
    select(region_id, date, value) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(date = date + 1) %>% # add a day so that it begins on the first of the month
    rename("zip_code" = "region_id", "median_sale_price" = "value") %>%
    mutate(zip_code = as.character(zip_code)) %>%
    filter(zip_code %in% ZCTA_list)
  
  ## bottom tier ##
  bottom_tier <- bottom_tier %>%
    select(region_id, date, value) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(date = date + 1) %>% # add a day so that it begins on the first of the month
    rename("zip_code" = "region_id", "median_sale_price" = "value") %>%
    mutate(zip_code = as.character(zip_code)) %>%
    filter(zip_code %in% ZCTA_list)
  
  ## load the default map ##
  output$disaster <- leaflet::renderLeaflet({
    leaflet()
    addProviderTiles(providers$CartoDB.Positron)})
  
  # change values based on selections
  observe({
    
    if (input$choose_disaster == "neworleans") {
      output$disaster_name <- renderText({"<b>Hurricane</b>"})
      output$disaster_date <- renderText({"Aug 2005"})
      output$city_name <- renderText({"<b>New Orleans</b>"})
      }
    
    if (input$choose_disaster == "coffeypark") {
      output$disaster_name <- renderText({"<b>Wildfires</b>"})
      output$disaster_date <- renderText({"Oct 2017"})
      output$city_name <- renderText({"<b>Coffey Park, CA</b>"})}
    
    if (input$choose_disaster == "buffalo") {
      output$disaster_name <- renderText({"<b>Snowstorm</b>"})
      output$disaster_date <- renderText({"Nov 2014"})
      output$city_name <- renderText({"<b>Buffalo, NY</b>"})}
    
    if (input$choose_disaster == "boulder") {
      output$disaster_name <- renderText({"<b>Flood</b>"})
      output$disaster_date <- renderText({"Sep 2013"})
      output$city_name <- renderText({"<b>Boulder, CO</b>"})}
    
    if (input$choose_disaster == "grandisle") {
      output$disaster_name <- renderText({"<b>BP Oil Spill</b>"})
      output$disaster_date <- renderText({"Apr 2010"})
      output$city_name <- renderText({"<b>Grand Isle, LA</b>"})}
    
    if (input$choose_disaster == "moore") {
      output$disaster_name <- renderText({"<b>Tornado</b>"})
      output$disaster_date <- renderText({"May 2013"})
      output$city_name <- renderText({"<b>Moore, OK</b>"})}
    
  })
  
  observe({
    if(input$choose_metric == "hpi"){}
    if(input$choose_metric == "bottom_tier"){}
    if(input$choose_metric == "sfhv"){}
  })
  
  new_new_orleans_shape <- new_orleans_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  new_orleans_data2 <- merge(new_new_orleans_shape,
                             single_family_homes,
                             by = "zip_code",
                             duplicateGeoms = T) %>% st_as_sf()
  
  interactive_map <- reactive({
    new_orleans_data2 %>%
      filter(date == chosen_month())})
  
  # set up map
  bins <- c(100000, 200000, 300000, 400000, 500000)
  pal <- reactive({colorBin("YlOrRd", domain=interactive_map()$median_sale_price, bins=bins)})
  
  #labels <- reactive({
   # sprintf("<strong>%s</strong><br/>Mean Rent: %g",
    #interactive_map()$zip_code, interactive_map()$median_sale_price)})
  
      setView(lng = -90.0715, lat = 29.95, zoom = 10) %>%
      addPolygons(
        #fillColor = pal(),
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
          #                          direction = "auto")) #%>%
      addLegend(pal=pal, values = ~median_sale_price, opacity = 0.8, title = "Mean Rent")
    })
}