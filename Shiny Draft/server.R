function(input, output, session){
  
  ### input sliders and drop downs ###
  
  chosen_month_neworleans <- reactive({input$choose_month_neworleans})
  chosen_metric_neworleans <- reactive({input$choose_metric_neworleans})
  
  chosen_month_coffeypark <- reactive({input$choose_month_coffeypark})
  chosen_metric_coffeypark <- reactive({input$choose_metric_coffeypark})
  
  chosen_month_moore <- reactive({input$choose_month_moore})
  chosen_metric_moore <- reactive({input$choose_metric_moore})
  
  chosen_month_buffalo <- reactive({input$choose_month_buffalo})
  chosen_metric_buffalo <- reactive({input$choose_metric_buffalo})
  
  chosen_month_grandisle <- reactive({input$choose_month_grandisle})
  chosen_metric_grandisle <- reactive({input$choose_metric_grandisle})
  
  ### update sliders ###
  ### THIS IS FAKE CODE 
  ### https://stackoverflow.com/questions/68342780/shiny-if-else-statement
  
 # observeEvent(input$x, 
   #            {updateSelectInput(session,
   #                               inputId = "y",
    #                              label = "Variable 2",
    #                              choices = names(df)[names(df) != input$x])
   #            })
  
  ### load data sources ###
  single_family_homes <- read.csv("../data/single_family_homes_time_series.csv")
  HPI <- read.csv("../data/HPI_data.csv")
  bottom_tier <- read.csv("../data/all_homes_bottom_tier.csv")
  
  ### data manipulation ###
  
  ZCTA_list <- c('70112', '70113', '70114', '70115', '70116', '70117', '70118', '70119',
                 '70121', '70122', '70123', '70124', '70125', '70126', '70127', '70128',
                 '70129', '70130','70131', '70139', '70163', '95401', '95403', '95404',
                 '95405', '95407', '95409', '14201', '14202', '14203', '14204', '14206',
                 '14207', '14208', '14209', '14210', '14211', '14212', '14213', '14214',
                 '14215', '14216', '14217', '14218', '14219', '14220', '14221','14222',
                 '14223', '14224', '14225', '14226', '14227', '14228', '14261', '73160',
                 '73165', '73170', '70358')
  
  new_orleans_zips <- c('70112', '70113', '70114', '70115', '70116', '70117', '70118', '70119',
                        '70121', '70122', '70123', '70124', '70125', '70126', '70127', '70128',
                        '70129', '70130','70131', '70139', '70163')
  
  moore_zips <- c("73160", "73165", "73170")
  
  coffey_zips <- c("95401", "95402", "95403", "95404", "95405", "95406", "95407", "95409")
  
  buffalo_zips <- c('14201', '14202', '14203','14204', '14206', '14207', '14208',
                    '14209', '14210', '14211', '14212', '14213', '14214', '14215', '14216',
                    '14217', '14218', '14219', '14220', '14221','14222', '14223', '14224',
                    '14225', '14226', '14227', '14228', '14261')
  
  grand_isle_zips <- c("70358")
  
  ## HPI ##
  HPI <- HPI %>%
    filter(zip_code %in% ZCTA_list) # refine the list so the data is smaller
  
  months <- expand.grid(year = unique(HPI$year),
                        month = c("01","02","03","04","05","06",
                                  "07", "08", "09", "10", "11", "12"))
  
  HPI <- left_join(HPI, months, by = "year") %>%
    mutate(date = as.Date(paste0(month, "/01/", year), "%m/%d/%Y")) %>%
    select(zip_code, date, HPI_2000, HPI, annual_change) %>%
    mutate(zip_code = as.character(zip_code),
           HPI_2000 = as.numeric(HPI_2000),
           HPI = as.numeric(HPI),
           annual_change = as.numeric(annual_change))
  
  # a curated list of zip codes missing from our data
  missing_zips <- c('70112', '14203', '14204', '14208') %>%
    as.data.frame() %>%
    rename(zip_code = '.') %>%
    mutate(date = NA,
           single_fam_val = NA,
           bottom_tier = NA,
           HPI_2000 = NA,
           HPI = NA,
           annual_change = NA)
  
  # join all the real estate data #
  base_data <- single_family_homes %>%
    left_join(bottom_tier, by = c("region_id", "date")) %>%
    select(region_id, date, value.x, value.y) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    mutate(date = date + 1) %>% # add a day so that it begins on the first of the month
    rename("zip_code" = "region_id",
           "single_fam_val" = "value.x",
           "bottom_tier" = "value.y") %>%
    mutate(zip_code = as.character(zip_code)) %>%
    filter(zip_code %in% ZCTA_list) %>% # refine the list so the data is smaller
    full_join(HPI, by = c("zip_code", "date")) %>%
    filter(date > as.Date('2000-01-01', "%Y-%m-%d")) %>%
    rbind(missing_zips) %>%
    tidyr::complete(date, zip_code) %>% 
    arrange(zip_code, date)# create months for zip codes missing from our data set
  
  rm(bottom_tier, single_family_homes, months, HPI)
  
  #  observe({
  #   if(input$choose_metric == "hpi"){1}
  # else if(input$choose_metric == "bottom_tier"){1}
  # else if(input$choose_metric == "sfhv"){1}
  # else{1}
  #})
  
  ### load shape files ###
  
  # NEW ORLEANS #
  new_orleans_shape <- readOGR("../data/shape files", "New_Orleans")
  new_orleans_shape <- new_orleans_shape[,-c(2,3,4,5,6,7,8,9)]
  
  refined_orleans_data <- new_orleans_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    merge(filter(base_data, zip_code %in% new_orleans_zips),
          by = "zip_code",
          duplicateGeoms = T) %>% st_as_sf()
  
  # MOORE OK #
  moore_ok_shape <- readOGR(dsn="../data/shape files", layer="Moore_OK")
  moore_ok_shape <- moore_ok_shape[,-c(2,3,4,5,6,7,8,9)]
  
  refined_moore_data <- moore_ok_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    merge(filter(base_data, zip_code %in% moore_zips),
          by = "zip_code", duplicateGeoms = T) %>%
    st_as_sf()
  
  # COFFEY PARK #
  cali_shape <- readOGR("../data/shape files", "California_Zip_Codes")
  cali_shape[,-c(1,4,5,6,7,8,9)]
  
  refined_cali_data <- cali_shape %>%
    rename(zip_code = ZIP_CODE, po_name = PO_NAME) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    merge(filter(base_data, zip_code %in% coffey_zips),
          by = "zip_code", duplicateGeoms = T) %>% st_as_sf()
  
  # GRAND ISLE #
  grand_isle_shape <- readOGR("../data/shape files", "Grand_Isla_LA")
  grand_isle_shape <- grand_isle_shape[,-c(2,3,4,5,6,7,8,9)]
  
  refined_grandisle_data <- grand_isle_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    merge(filter(base_data, zip_code %in% grand_isle_zips),
          by = "zip_code", duplicateGeoms = T) %>%
    st_as_sf()
  
  buffalo_shape <- readOGR("../data/shape files", "Buffalo")
  buffalo_shape <- buffalo_shape[,-c(2,3,4,5,6,7,8,9)]
  
  refined_buffalo_data <- buffalo_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    merge(filter(base_data, zip_code %in% buffalo_zips),
          by = "zip_code", duplicateGeoms = T) %>%
    st_as_sf()
  
  rm(buffalo_shape, grand_isle_shape, cali_shape, moore_ok_shape, new_orleans_shape)
  
  interactive_map_neworleans <- reactive({
    refined_orleans_data %>%
      filter(date == chosen_month_neworleans())})
  
  interactive_map_coffeypark <- reactive({
    refined_cali_data %>%
      filter(date == chosen_month_coffeypark())})
  
  interactive_map_moore <- reactive({
    refined_moore_data %>%
      filter(date == chosen_month_moore())})
  
  interactive_map_buffalo <- reactive({
    refined_buffalo_data %>%
      filter(date == chosen_month_buffalo())})
  
  interactive_map_grandisle <- reactive({
    refined_grandisle_data %>%
      filter(date == chosen_month_grandisle())})
  
  # set up map
  bins <- c(100000, 200000, 300000, 400000, 500000)
  
  ## load the default map ##
  output$disaster_map_neworleans <- renderLeaflet({
    interactive_map_neworleans() %>%
      leaflet()  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -90.0715, lat = 29.95, zoom = 11) %>%
      addPolygons(
       # fillColor = ~pal(median_sale_price),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8))
  })
  
  output$disaster_map_coffeypark <- renderLeaflet({
    interactive_map_coffeypark() %>%
      leaflet()  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -122.748, lat = 38.4777, zoom = 11) %>%
      addPolygons(
       # fillColor = ~pal(median_sale_price),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8))
  })

  output$disaster_map_moore <- renderLeaflet({
    interactive_map_moore() %>%
      leaflet()  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -97.4867, lat = 35.3395, zoom = 11) %>%
      addPolygons(
        #fillColor = ~pal(median_sale_price),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8))
  })
  
  output$disaster_map_buffalo <- renderLeaflet({
    interactive_map_buffalo() %>%
      leaflet()  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -78.878738, lat = 42.880230, zoom = 11) %>%
      addPolygons(
        #fillColor = ~pal(median_sale_price),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8))
  })
  
  output$disaster_map_grandisle <- renderLeaflet({
    interactive_map_grandisle() %>%
      leaflet()  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -89.987294, lat = 29.236617, zoom = 11) %>%
      addPolygons(
        #fillColor = ~pal(median_sale_price),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8))
  })
  
}
  
