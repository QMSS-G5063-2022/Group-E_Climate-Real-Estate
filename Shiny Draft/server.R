function(input, output){
  
  ### input sliders and drop downs ###
  chosen_month <- reactive({input$choose_month})
  chosen_location <- reactive({input$choose_disaster})
  chosen_metric <- reactive({input$choose_metric})
  
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
      output$city_name <- renderText({"<b>Coffey Park, CA</b>"})
    }
    
    if (input$choose_disaster == "buffalo") {
      output$disaster_name <- renderText({"<b>Snowstorm</b>"})
      output$disaster_date <- renderText({"Nov 2014"})
      output$city_name <- renderText({"<b>Buffalo, NY</b>"})
    }
    
    if (input$choose_disaster == "grandisle") {
      output$disaster_name <- renderText({"<b>BP Oil Spill</b>"})
      output$disaster_date <- renderText({"Apr 2010"})
      output$city_name <- renderText({"<b>Grand Isle, LA</b>"})
    }
    
    if (input$choose_disaster == "moore") {
      output$disaster_name <- renderText({"<b>Tornado</b>"})
      output$disaster_date <- renderText({"May 2013"})
      output$city_name <- renderText({"<b>Moore, OK</b>"})
    }
  })
  
  
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
    mutate(date = as.Date(paste0(month, "/01/", year), "%m/%d/%y")) %>%
    select(zip_code, date, HPI_2000, HPI, annual_change) %>%
    mutate(zip_code = as.character(zip_code),
           HPI_2000 = as.numeric(HPI_2000),
           HPI = as.numeric(HPI),
           annual_change = as.numeric(annual_change))
  
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
    left_join(HPI, by=c("zip_code", "date"))
  
  rm(bottom_tier, single_family_homes, months, HPI)
  
  new_long <- reactive({
    
    if(input$choose_disaster == "neworleans") {-90.0715}
    else if(input$choose_disaster == "grandisle") {-89.987294}
    else if(input$choose_disaster == "buffalo") {-78.878738}
    else if(input$choose_disaster == "moore") {new_long <- -97.486703}
    else {-122.748}})
  
  new_lat <- reactive({
    
    if(input$choose_disaster == "neworleans") {29.95}
    else if(input$choose_disaster == "grandisle") {29.236617}
    else if(input$choose_disaster == "buffalo") {42.880230}
    else if(input$choose_disaster == "moore") {35.339508}
    else {38.4777}})
  
  ### load shape files ###
  
  # NEW ORLEANS #
  new_orleans_shape <- readOGR("../data/shape files", "New_Orleans")
  new_orleans_shape <- new_orleans_shape[,-c(2,3,4,5,6,7,8,9)]
  
  refined_orleans_shape <- new_orleans_shape %>%
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
  
  #  observe({
  #   if(input$choose_metric == "hpi"){1}
  #  if(input$choose_metric == "bottom_tier"){1}
  # if(input$choose_metric == "sfhv"){1}
  #})
  
  interactive_map <- reactive({
    base_data %>%
      filter(date == chosen_month())
  })
  
  
  # set up map
  bins <- c(100000, 200000, 300000, 400000, 500000)
  
  ## load the default map ##
  output$disaster_map <- renderLeaflet({
    
    interactive_map() %>%
      leaflet()  %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = new_long(), lat = new_lat(), zoom = 10)
  })


}
  
