function(input, output, session){
  
  ### input sliders and drop downs ###
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
    filter(date > as.Date('2000-01-01', "%Y-%m-%d") & date < as.Date('2022-01-01', "%Y-%m-%d")) %>%
    rbind(missing_zips) %>%
    tidyr::complete(date, zip_code) %>% 
    arrange(zip_code, date) %>%
    filter(is.na(date) == FALSE) %>%
    mutate(city = case_when(zip_code %in% c('70112', '70113', '70114', '70115', '70116', '70117',
                                            '70118', '70119', '70121', '70122', '70123', '70124',
                                            '70125', '70126', '70127', '70128', '70129', '70130',
                                            '70131', '70139', '70163') ~ "New Orleans",
                            zip_code %in% c("73160", "73165", "73170") ~ "Moore",
                            zip_code %in% c("95401", "95402", "95403", "95404", "95405", "95406",
                                            "95407", "95409") ~ "Coffey Park",
                            zip_code %in% c('14201', '14202', '14203','14204', '14206', '14207', '14208',
                                            '14209', '14210', '14211', '14212', '14213', '14214', '14215', 
                                            '14216', '14217', '14218', '14219', '14220', '14221','14222', 
                                            '14223', '14224', '14225', '14226', '14227', '14228', '14261') ~ "Buffalo",
                            zip_code == '70358' ~ "Grand Isle"))# create months for zip codes missing from our data set
  
  impute_data_zip <- base_data %>%
    group_by(city, date) %>%
    summarize(avg_single_fam_val = mean(single_fam_val, na.rm = TRUE),
              avg_bottom_tier = mean(bottom_tier, na.rm = TRUE),
              avg_HPI_2000 = mean(HPI_2000, na.rm = TRUE),
              avg_HPI = mean(HPI, na.rm = TRUE),
              avg_annual_change = mean(annual_change, na.rm = TRUE))
  
  # impute the missing zip codes' numbers using the average for the city
  base_data <- base_data %>%
    left_join(impute_data_zip, by = c("city", "date")) %>%
    mutate(single_fam_val = case_when(is.na(single_fam_val) == TRUE ~ avg_single_fam_val,
                                      TRUE ~ as.numeric(single_fam_val)),
           bottom_tier = case_when(is.na(bottom_tier) == TRUE ~ avg_bottom_tier,
                                   TRUE ~ as.numeric(bottom_tier)),
           HPI_2000 = case_when(is.na(HPI_2000) == TRUE ~ avg_HPI_2000,
                                TRUE ~ as.numeric(HPI_2000)),
           HPI = case_when(is.na(HPI) == TRUE ~ avg_HPI,
                           TRUE ~ as.numeric(HPI)),
           annual_change = case_when(is.na(annual_change) == TRUE ~ avg_annual_change,
                                     TRUE ~ as.numeric(annual_change))) %>%
    select(date, zip_code, single_fam_val, bottom_tier, HPI_2000, HPI, annual_change, city)
  
  rm(bottom_tier, single_family_homes, months, HPI)
  
  line_chart_data <- impute_data_zip
  
  ## bar chart data ##
  new_orleans_diff <- base_data %>%
    filter(city == "New Orleans",
           date %in% c(as.Date("06/01/2005",  "%m/%d/%Y"),
                       as.Date("08/01/2005",  "%m/%d/%Y"),
                       as.Date("10/01/2005",  "%m/%d/%Y")))
  
  moore_diff <- base_data  %>%
    filter(city == "Moore",
           date %in% c(as.Date("08/01/2017",  "%m/%d/%Y"),
                       as.Date("10/01/2017",  "%m/%d/%Y"),
                       as.Date("12/01/2017",  "%m/%d/%Y")))
  
  buffalo_diff <- base_data  %>%
    filter(city == "Buffalo",
           date %in% c(as.Date("09/01/2014",  "%m/%d/%Y"),
                       as.Date("11/01/2014",  "%m/%d/%Y"),
                       as.Date("01/01/2015",  "%m/%d/%Y")))
  
  grandisle_diff <- base_data  %>%
    filter(city == "Grand Isle",
           date %in% c(as.Date("02/01/2010",  "%m/%d/%Y"),
                       as.Date("04/01/2010",  "%m/%d/%Y"),
                       as.Date("06/01/2010",  "%m/%d/%Y")))
  
  coffey_diff <- base_data  %>%
    filter(city == "Coffey Park",
           date %in% c(as.Date("08/01/2017",  "%m/%d/%Y"),
                       as.Date("10/01/2017",  "%m/%d/%Y"),
                       as.Date("12/01/2017",  "%m/%d/%Y")))
  
  bar_chart_data <- new_orleans_diff %>%
    bind_rows(coffey_diff, moore_diff, grandisle_diff, buffalo_diff)
  
  rm(new_orleans_diff, coffey_diff, moore_diff, grandisle_diff, buffalo_diff)
  
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
  coffey_park_shape <- readOGR("../data/shape files", "Coffey_Park")
  coffey_park_shape <- coffey_park_shape[,-c(2,3,4,5,6,7,8,9)]
  
  refined_coffey_park_data <- coffey_park_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
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
  
  rm(buffalo_shape, grand_isle_shape, coffey_park_shape, moore_ok_shape, new_orleans_shape)
  
  interactive_map_coffeypark <- reactive({
    refined_coffey_park_data$date2 <- format(refined_coffey_park_data$date, format="%Y %m")
    chosen_month_coffeypark2 <- format(chosen_month_coffeypark(), format="%Y %m")
    refined_coffey_park_data %>%
      filter(date2 == chosen_month_coffeypark2)
  })
  
  interactive_map_moore <- reactive({
    refined_moore_data$date2 <- format(refined_moore_data$date, format="%Y %m")
    chosen_month_moore2 <- format(chosen_month_moore(), format="%Y %m")
    refined_moore_data %>%
      filter(date2 == chosen_month_moore2)
  })
  
  interactive_map_buffalo <- reactive({
    refined_buffalo_data$date2 <- format(refined_buffalo_data$date, format="%Y %m")
    chosen_month_buffalo2 <- format(chosen_month_buffalo(), format="%Y %m")
    refined_buffalo_data %>%
      filter(date2 == chosen_month_buffalo2)
  })
  
  interactive_map_grandisle <- reactive({
    refined_grandisle_data$date2 <- format(refined_grandisle_data$date, format="%Y %m")
    chosen_month_grandisle2 <- format(chosen_month_grandisle(), format="%Y %m")
    refined_grandisle_data %>%
      filter(date2 == chosen_month_grandisle2)
  })
  
  
  
  
  
  output$disaster_map_neworleans <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -90.0715, lat = 29.95, zoom = 11)
  })  
  
  observe({
    
    # input values for date and metric chosen
    chosen_month_neworleans <- input$choose_month_neworleans
    chosen_metric_neworleans <- input$choose_metric_neworleans
    
    # create date field by month only
    refined_orleans_data$date2 <- format(refined_orleans_data$date, format="%Y %m")
    chosen_month_neworleans2 <- format(chosen_month_neworleans, format="%Y %m")
    
    # subsets data based on inputs
    interactive_map_neworleans <- refined_orleans_data %>%
      filter(date2 == chosen_month_neworleans2) %>% 
      select(date2, zip_code, chosen_metric_neworleans) %>% 
      rename(selected_metric = chosen_metric_neworleans)
    
    # set palette
    if(chosen_metric_neworleans == 'HPI') {
      pal = "YlGnBu"
    } else if(chosen_metric_neworleans == 'bottom_tier') {
      pal = "PuBu"
    } else if(chosen_metric_neworleans == 'single_fam_val') {
      pal = "YlOrRd"
    } else {
      pal = "PuRd"
    }
    
    pal_no = colorBin(pal, domain=interactive_map_neworleans$selected_metric, bins=5)
    
    if(chosen_metric_neworleans == 'HPI') {
      hover = "Zip Code: <strong>%s</strong><br/>Home Price Index (HPI): %g"
    } else if(chosen_metric_neworleans == 'bottom_tier') {
      hover = "Zip Code: <strong>%s</strong><br/>Bottom Tier Home Price: $%g"
    } else if(chosen_metric_neworleans == 'single_fam_val') {
      hover = "Zip Code: <strong>%s</strong><br/>Single Family Home Value: $%g"
    } else {
      hover = "Zip Code: <strong>%s</strong><br/>Annual Change in HPI (%%): %g"
    }
    
    labels_no = sprintf(
      hover,
      interactive_map_neworleans$zip_code, interactive_map_neworleans$selected_metric
    ) %>% lapply(htmltools::HTML)
    
    # update map
    leafletProxy("disaster_map_neworleans", data=interactive_map_neworleans) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal_no(selected_metric),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8),
        label = labels_no,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto")) %>%
      addLegend(position="bottomright", pal=pal_no, values = ~selected_metric, opacity = 0.8, title = chosen_metric_neworleans) 
  
  })
  
  
  
  ## load the default map ##
 
  
  
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
  
