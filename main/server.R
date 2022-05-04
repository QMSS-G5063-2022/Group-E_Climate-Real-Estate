function(input, output, session){
  
  ### load data sources ###
  single_family_homes <- read.csv("./data/single_family_homes_time_series.csv")
  HPI <- read.csv("./data/HPI_data.csv")
  bottom_tier <- read.csv("./data/all_homes_bottom_tier.csv")
  
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
              avg_annual_change = mean(annual_change, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(city) %>%
    mutate(avg_annual_change = case_when(is.na(avg_annual_change) == TRUE ~ as.numeric(lag(avg_single_fam_val,0)/lag(avg_single_fam_val,1)-1)*100,
                                         TRUE ~ as.numeric(avg_annual_change))) %>%
    ungroup()
  
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
  
  ### load shape files ###
  
  # NEW ORLEANS #
  new_orleans_shape <- readOGR("./data/shape files", "New_Orleans")
  new_orleans_shape <- new_orleans_shape[,-c(2,3,4,5,6,7,8,9)]
  
  refined_orleans_data <- new_orleans_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    merge(filter(base_data, zip_code %in% new_orleans_zips),
          by = "zip_code",
          duplicateGeoms = T) %>% st_as_sf()
  
  # MOORE OK #
  moore_ok_shape <- readOGR(dsn="./data/shape files", layer="Moore_OK")
  moore_ok_shape <- moore_ok_shape[,-c(2,3,4,5,6,7,8,9)]
  
  refined_moore_data <- moore_ok_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    merge(filter(base_data, zip_code %in% moore_zips),
          by = "zip_code", duplicateGeoms = T) %>%
    st_as_sf()
  
  # COFFEY PARK #
  coffey_park_shape <- readOGR("./data/shape files", "Coffey_Park")
  coffey_park_shape <- coffey_park_shape[,-c(2,3,4,5,6,7,8,9)]
  
  refined_coffey_park_data <- coffey_park_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    merge(filter(base_data, zip_code %in% coffey_zips),
          by = "zip_code", duplicateGeoms = T) %>% st_as_sf()
  
  # GRAND ISLE #
  grand_isle_shape <- readOGR("./data/shape files", "Grand_Isla_LA")
  grand_isle_shape <- grand_isle_shape[,-c(2,3,4,5,6,7,8,9)]
  
  refined_grandisle_data <- grand_isle_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    merge(filter(base_data, zip_code %in% grand_isle_zips),
          by = "zip_code", duplicateGeoms = T) %>%
    st_as_sf()
  
  buffalo_shape <- readOGR("./data/shape files", "Buffalo")
  buffalo_shape <- buffalo_shape[,-c(2,3,4,5,6,7,8,9)]
  
  refined_buffalo_data <- buffalo_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs")) %>%
    merge(filter(base_data, zip_code %in% buffalo_zips),
          by = "zip_code", duplicateGeoms = T) %>%
    st_as_sf()
  
  rm(buffalo_shape, grand_isle_shape, coffey_park_shape, moore_ok_shape, new_orleans_shape)
 
  
  #map for new orleans
  base_orleans <- refined_orleans_data %>%
    filter(between(date,
                   as.Date("2002-08-01", format = "%Y-%m-%d"),
                   as.Date("2008-08-01", format = "%Y-%m-%d")))
  
  bins_no = unname(quantile(base_orleans$annual_change, probs = seq(0, 1, 1/5), na.rm = TRUE))
  
  pal_no = colorBin("RdBu", domain=base_orleans$annual_change, bins=bins_no)
  
  base_orleans_date <- base_orleans %>%
    filter(date == "2005-08-01")
  
  labels_no = sprintf(
    "Zip Code: <strong>%s</strong><br/>Annual Change in HPI: <strong>%g%%</strong>",
    base_orleans_date$zip_code, base_orleans_date$annual_change
  ) %>% lapply(htmltools::HTML)
  
  output$disaster_map_neworleans <- renderLeaflet({
    leaflet(base_orleans_date) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -90.0715, lat = 29.95, zoom = 11) %>%
      addPolygons(
        fillColor = ~pal_no(annual_change),
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
      addLegend(position="bottomright", pal=pal_no, values = ~annual_change, opacity = 0.8, title = "Annual Change in HPI (%)")
  })  
  
  # observing for value changes
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
    
    # doesn't subset on date, use for bins of metrics
    bin_df_neworleans <- refined_orleans_data %>%
      select(date, zip_code, chosen_metric_neworleans) %>% 
      rename(selected_metric = chosen_metric_neworleans) %>%
      filter(between(date,
                     as.Date("2002-08-01", format = "%Y-%m-%d"),
                     as.Date("2008-08-01", format = "%Y-%m-%d")))
      
    bins = unname(quantile(bin_df_neworleans$selected_metric, probs = seq(0, 1, 1/5), na.rm = TRUE))

    # set palette, hover text, y-axis labels on line chart
    if(chosen_metric_neworleans == 'annual_change') {
      
      pal = "RdBu"
      hover = "Zip Code: <strong>%s</strong><br/>Annual Change in HPI: <strong>%g%%</strong>"
      y_lab = "Annual Change in HPI (%)"
      
    } else if(chosen_metric_neworleans == 'HPI') {
      
      pal = "PiYG"
      hover = "Zip Code: <strong>%s</strong><br/>Home Price Index (HPI): <strong>%g</strong>"
      y_lab = "Home Price Index"
      
    } else {
      pal = "RdYlGn"
      hover = "Zip Code: <strong>%s</strong><br/>Single Family Home Value: <strong>$%g</strong>"
      y_lab = "Single Family Home Value ($)"
      
    }
    
    pal_o = colorBin(pal, domain=interactive_map_neworleans$selected_metric, bins=bins)
    
    labels_o = sprintf(
      hover,
      interactive_map_neworleans$zip_code, interactive_map_neworleans$selected_metric
    ) %>% lapply(htmltools::HTML)
    
    # update map
    leafletProxy("disaster_map_neworleans", data=interactive_map_neworleans) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor = ~pal_o(selected_metric),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8),
        label = labels_o,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto")) %>%
      addLegend(position="bottomright", pal=pal_o, values = ~selected_metric, opacity = 0.8, title = y_lab) 
  
    # line chart
    neworleans_line_data <- line_chart_data %>%
      filter(city == 'New Orleans')
    
    avg_metric_neworleans = paste0('avg_', chosen_metric_neworleans)
    
    neworleans_line_data$date2 <- format(neworleans_line_data$date, format="%Y %m")
    
    neworleans_line_data2 <- neworleans_line_data %>%
      filter(between(date,
                     as.Date("2002-08-01", format = "%Y-%m-%d"),
                     as.Date("2008-08-01", format = "%Y-%m-%d"))) %>%
      select(date, date2, avg_metric_neworleans) %>% 
      rename(selected_metric = avg_metric_neworleans)  
      
    output$line_chart_neworleans <- renderPlotly({
      plot_ly(neworleans_line_data2, x = ~date, y =~selected_metric, 
      type = 'scatter', mode = 'lines', name = 'New Orleans Mean Prices') %>%
        add_segments(x = as.Date("2005-08-01", format = "%Y-%m-%d"),
                     xend = as.Date("2005-08-01", format = "%Y-%m-%d"),
                     y = -100000, yend = 1000000) %>%
        add_segments(x = chosen_month_neworleans, format = "%Y-%m-%d",
                     xend = chosen_month_neworleans, format = "%Y-%m-%d",
                     y = -100000, yend = 1000000, line = list(dash="dash")) %>%
        add_text(text = "Hurricane Katrina",
                 textfont = list(size = 10,
                                 color = "darkred"),
                 x = as.Date("2006-08-01", format = "%Y-%m-%d"),
                 y = ifelse(max(neworleans_line_data2$selected_metric) > 0,
                            max(neworleans_line_data2$selected_metric) * 0.8,
                            min(neworleans_line_data2$selected_metric) * 1.2)) %>%
        layout(showlegend = FALSE,
               xaxis = list(range = c(as.Date("2002-08-01", format = "%Y-%m-%d"),
                                      as.Date("2008-08-01", format = "%Y-%m-%d")),
                            tickfont = list(size = 8),
                            title = "Month / Year",
                            titlefont = list(size = 9)),
               yaxis = list(title = y_lab, range = c(ifelse(min(neworleans_line_data2$selected_metric) > 0,
                                             min(neworleans_line_data2$selected_metric) * 0.9,
                                             min(neworleans_line_data2$selected_metric) * 1.1),
                                      ifelse(max(neworleans_line_data2$selected_metric) > 0,
                                             max(neworleans_line_data2$selected_metric) * 1.1,
                                             max(neworleans_line_data2$selected_metric) * 0.9)),
                            title = chosen_metric_neworleans,
                            tickfont = list(size = 8),
                            titlefont = list(size = 9)),
               title = list(text = ifelse(chosen_metric_neworleans == "annual_change",
                                          "Avg Annual % Change in HPI (+/- 3 yrs of Hurricane)",
                                          ifelse(chosen_metric_neworleans == "HPI",
                                                 "Avg Annual HPI \n(+/- 3 yrs of Hurricane)",
                                                 "Avg Monthly Single Family Home \nValue (+/- 3 yrs of Hurricane)")),
                            font = list(size = 11)))
      })
    
    # update bar chart based on what's clicked
    event <- input$disaster_map_neworleans_shape_click
    filtered_zips <- event$id
  
    # update bar chart based on what's clicked
    event <- input$disaster_map_neworleans_shape_click
    print(event$id)
    
    neworleans_bar_data <- bar_chart_data %>%
      filter(city == 'New Orleans') %>%
      filter(zip_code == 70112) %>%
      rename(selected_metric = chosen_metric_neworleans)
    
    output$bar_chart_neworleans <- renderPlotly({
      plot_ly(neworleans_bar_data,
              x = ~date,
              y = ~selected_metric,
              color=~date,
              type='bar') %>%
        layout(legend = list(orientation = "h",
                             xanchor = "center",
                             x = 0.5,
                             y = -0.2),
               xaxis = list(title = "Event Timeline",
                            titlefont = list(size = 9),
                            tickfont = list(size = 8)),
               yaxis = list(range=c(min(neworleans_bar_data$selected_metric) * 0.7,
                                    max(neworleans_bar_data$selected_metric) * 1.1),
                            title = y_lab,
                            titlefont = list(size = 9),
                            tickfont = list(size = 8)),
               title = list(text = "Real Estate Impact by Zip Code \n(+/- 3 months from Hurricane)",
                            font = list(size = 11)))
    })

    
  })
  
  
  
  
  ##### COFFEY PARK #####
  
  base_coffey <- refined_coffey_park_data %>%
    filter(between(date,
                   as.Date("2015-09-01", format = "%Y-%m-%d"),
                   as.Date("2021-09-01", format = "%Y-%m-%d")))
  
  bins_cp = unname(quantile(base_coffey$annual_change, probs = seq(0, 1, 1/5), na.rm = TRUE))
  
  pal_cp = colorBin("RdBu", domain=base_coffey$annual_change, bins=bins_cp)
  
  base_coffey_date <- base_coffey %>%
    filter(date == "2018-09-01")
  
  labels_cp = sprintf(
    "Zip Code: <strong>%s</strong><br/>Annual Change in HPI: <strong>%g%%</strong>",
    base_coffey_date$zip_code, base_coffey_date$annual_change
  ) %>% lapply(htmltools::HTML)
  
  output$disaster_map_coffeypark <- renderLeaflet({
    leaflet(base_coffey_date) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -122.748, lat = 38.4777, zoom = 11) %>%
      addPolygons(
        fillColor = ~pal_cp(annual_change),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8),
        label = labels_cp,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto")) %>%
      addLegend(position="bottomright", pal=pal_cp, values = ~annual_change, opacity = 0.8, title = "Annual Change in HPI (%)")
  })  
  
  # observing for value changes
  observe({
    
    # input values for date and metric chosen
    chosen_month_coffeypark <- input$choose_month_coffeypark
    chosen_metric_coffeypark <- input$choose_metric_coffeypark
    
    # create date field by month only
    refined_coffey_park_data$date2 <- format(refined_coffey_park_data$date, format="%Y %m")
    chosen_month_coffeypark2 <- format(chosen_month_coffeypark, format="%Y %m")
    
    # subsets data based on inputs
    interactive_map_coffeypark <- refined_coffey_park_data %>%
      filter(date2 == chosen_month_coffeypark2) %>% 
      select(date2, zip_code, chosen_metric_coffeypark) %>% 
      rename(selected_metric = chosen_metric_coffeypark)
    
    # doesn't subset on date, use for bins of metrics
    bin_df_coffeypark <- refined_coffey_park_data %>%
      select(date, zip_code, chosen_metric_coffeypark) %>% 
      rename(selected_metric = chosen_metric_coffeypark) %>%
      filter(between(date,
                     as.Date("2015-09-01", format = "%Y-%m-%d"),
                     as.Date("2021-09-01", format = "%Y-%m-%d")))
    
    bins = unname(quantile(bin_df_coffeypark$selected_metric, probs = seq(0, 1, 1/5), na.rm = TRUE))
    
    # set palette, hover text, y-axis labels on line chart
    if(chosen_metric_coffeypark == 'annual_change') {
      
      pal = "RdBu"
      hover = "Zip Code: <strong>%s</strong><br/>Annual Change in HPI: <strong>%g%%</strong>"
      y_lab = "Annual Change in HPI (%)"
      
    } else if(chosen_metric_coffeypark == 'HPI') {
      
      pal = "PiYG"
      hover = "Zip Code: <strong>%s</strong><br/>Home Price Index (HPI): <strong>%g</strong>"
      y_lab = "Home Price Index"
      
    } else {
      pal = "RdYlGn"
      hover = "Zip Code: <strong>%s</strong><br/>Single Family Home Value: <strong>$%g</strong>"
      y_lab = "Single Family Home Value ($)"
      
    }
    
    pal_no = colorBin(pal, domain=interactive_map_coffeypark$selected_metric, bins=bins)
    
    labels_no = sprintf(
      hover,
      interactive_map_coffeypark$zip_code, interactive_map_coffeypark$selected_metric
    ) %>% lapply(htmltools::HTML)
    
    # update map
    leafletProxy("disaster_map_coffeypark", data=interactive_map_coffeypark) %>%
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
      addLegend(position="bottomright", pal=pal_no, values = ~selected_metric, opacity = 0.8, title = y_lab) 
    
    # line chart
    
    coffeypark_line_data <- line_chart_data %>%
      filter(city == 'Coffey Park')
    
    avg_metric_coffeypark = paste0('avg_', chosen_metric_coffeypark)
    
    coffeypark_line_data$date2 <- format(coffeypark_line_data$date, format="%Y %m")
    
    coffeypark_line_data2 <- coffeypark_line_data %>%
      filter(between(date,
                     as.Date("2015-09-01", format = "%Y-%m-%d"),
                     as.Date("2021-09-01", format = "%Y-%m-%d")
                     )) %>%
      select(date, date2, avg_metric_coffeypark) %>% 
      rename(selected_metric = avg_metric_coffeypark)  
    
    output$line_chart_coffeypark <- renderPlotly({
      plot_ly(coffeypark_line_data2, x = ~date, y =~selected_metric, 
              type = 'scatter', mode = 'lines', name = 'Coffey Park Mean Prices') %>%
            add_segments(x = as.Date("2018-09-01", format = "%Y-%m-%d"),
                         xend = as.Date("2018-09-01", format = "%Y-%m-%d"),
                         y = -2000000, yend = 30000000) %>%
            add_segments(x = chosen_month_coffeypark, format = "%Y-%m-%d",
                     xend = chosen_month_coffeypark, format = "%Y-%m-%d",
                     y = -100000, yend = 1000000, line = list(dash="dash")) %>%
        add_text(text = "Snell Wildfire",
                 textfont = list(size = 10,
                                 color = "darkred"),
                 x = as.Date("2019-09-01", format = "%Y-%m-%d"),
                 y = ifelse(max(coffeypark_line_data2$selected_metric) > 0,
                            max(coffeypark_line_data2$selected_metric) * 0.8,
                            min(coffeypark_line_data2$selected_metric) * 1.2)) %>%
            layout(showlegend = FALSE,
                   xaxis = list(range = c(as.Date("2015-09-01", format = "%Y-%m-%d"),
                                          as.Date("2021-09-01", format = "%Y-%m-%d")),
                                tickfont = list(size = 8),
                                title = "Month / Year",
                                titlefont = list(size = 9)),
                   yaxis = list(title = y_lab,
                                range = c(ifelse(min(coffeypark_line_data2$selected_metric) > 0,
                                                 min(coffeypark_line_data2$selected_metric) * 0.9,
                                                 min(coffeypark_line_data2$selected_metric) * 1.1),
                                          ifelse(max(coffeypark_line_data2$selected_metric) > 0,
                                                 max(coffeypark_line_data2$selected_metric) * 1.1,
                                                 max(coffeypark_line_data2$selected_metric) * 0.9)),
                                tickfont = list(size = 8),
                                titlefont = list(size = 9)),
                   title = list(text = ifelse(chosen_metric_coffeypark == "annual_change",
                                              "Avg Annual % Change in HPI (+/- 3 yrs of Wildfire)",
                                              ifelse(chosen_metric_coffeypark == "HPI",
                                                     "Avg Annual HPI \n(+/- 3 yrs of Wildfire)",
                                                     "Avg Monthly Single Family Home \nValue (+/- 3 yrs of Wildfire)")),
                                font = list(size = 11)))
    })
    
    coffeypark_bar_data <- bar_chart_data %>% 
      filter(city == 'Coffey Park') %>%
      filter(zip_code == 95401) %>%
      rename(selected_metric = chosen_metric_coffeypark)
    
    output$bar_chart_coffeypark <- renderPlotly({
      plot_ly(coffeypark_bar_data,
              x=~date,
              y=~selected_metric,
              color=~date,
              type='bar') %>%
        layout(legend = list(orientation = "h",
                             xanchor = "center",
                             x = 0.5,
                             y=-0.2),
               xaxis = list(title = "Event Timeline",
                            titlefont = list(size = 9),
                            tickfont = list(size = 8)),
               yaxis = list(range=c(min(coffeypark_bar_data$selected_metric) * 0.7,
                                    max(coffeypark_bar_data$selected_metric) * 1.1),
                            title = y_lab,
                            titlefont = list(size = 9),
                            tickfont = list(size = 8)),
               title = list(text = "Real Estate Impact by Zip Code \n(+/- 3 months from Wildfire)",
                            font = list(size = 11)))
    })
    
  }) 
  
  
  ############# MOORE ##################

  # map for Moore OK
  base_moore <- refined_moore_data %>%
    filter(between(date,
                   as.Date("2010-05-01", format = "%Y-%m-%d"),
                   as.Date("2016-05-01", format = "%Y-%m-%d")))
  
  bins_m = unname(quantile(base_moore$annual_change, probs = seq(0, 1, 1/5), na.rm = TRUE))
  
  pal_m = colorBin("RdBu", domain=base_moore$annual_change, bins=bins_m)
  
  base_moore_date <- base_moore %>%
    filter(date == "2013-05-01")
  
  labels_m = sprintf(
    "Zip Code: <strong>%s</strong><br/>Annual Change in HPI: <strong>%g%%</strong>",
    base_moore_date$zip_code, base_moore_date$annual_change
  ) %>% lapply(htmltools::HTML)
  
  output$disaster_map_moore <- renderLeaflet({
    leaflet(base_moore_date) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -97.4867, lat = 35.3395, zoom = 11) %>%
      addPolygons(
        fillColor = ~pal_m(annual_change),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8),
        label = labels_m,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto")) %>%
      addLegend(position="bottomright", pal=pal_m, values = ~annual_change, opacity = 0.8, title = "Annual Change in HPI (%)")
  })  
  
  # observing for value changes
  observe({
    
    # input values for date and metric chosen
    chosen_month_moore <- input$choose_month_moore
    chosen_metric_moore <- input$choose_metric_moore
    
    # create date field by month only
    refined_moore_data$date2 <- format(refined_moore_data$date, format="%Y %m")
    chosen_month_moore2 <- format(chosen_month_moore, format="%Y %m")
    
    # subsets data based on inputs
    interactive_map_moore <- refined_moore_data %>%
      filter(date2 == chosen_month_moore2) %>% 
      select(date2, zip_code, chosen_metric_moore) %>% 
      rename(selected_metric = chosen_metric_moore)
    
    # doesn't subset on date, use for bins of metrics
    bin_df_moore <- refined_moore_data %>%
      select(date, zip_code, chosen_metric_moore) %>% 
      rename(selected_metric = chosen_metric_moore) %>%
      filter(between(date,
                     as.Date("2010-05-01", format = "%Y-%m-%d"),
                     as.Date("2016-05-01", format = "%Y-%m-%d")))
    
    bins = unname(quantile(bin_df_moore$selected_metric, probs = seq(0, 1, 1/5), na.rm = TRUE))
    
    
    # set palette, hover text, y-axis labels on line chart
    if(chosen_metric_moore == 'annual_change') {
      
      pal = "RdBu"
      hover = "Zip Code: <strong>%s</strong><br/>Annual Change in HPI: <strong>%g%%</strong>"
      y_lab = "Annual Change in HPI (%)"
      
    } else if(chosen_metric_moore == 'HPI') {
      
      pal = "PiYG"
      hover = "Zip Code: <strong>%s</strong><br/>Home Price Index (HPI): <strong>%g</strong>"
      y_lab = "Home Price Index"
      
    } else {
      pal = "RdYlGn"
      hover = "Zip Code: <strong>%s</strong><br/>Single Family Home Value: <strong>$%g</strong>"
      y_lab = "Single Family Home Value ($)"
      
    }
    
    pal_no = colorBin(pal, domain=interactive_map_moore$selected_metric, bins=bins)
    
    labels_no = sprintf(
      hover,
      interactive_map_moore$zip_code, interactive_map_moore$selected_metric
    ) %>% lapply(htmltools::HTML)
    
    # update map
    leafletProxy("disaster_map_moore", data=interactive_map_moore) %>%
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
      addLegend(position="bottomright", pal=pal_no, values = ~selected_metric, opacity = 0.8, title = y_lab) 
    
    # line chart

    moore_line_data <- line_chart_data %>%
      filter(city == 'Moore')
    
    avg_metric_moore = paste0('avg_', chosen_metric_moore)
    
    moore_line_data$date2 <- format(moore_line_data$date, format="%Y %m")
    
    moore_line_data2 <- moore_line_data %>%
      filter(between(date,
                     as.Date("2010-05-01", format = "%Y-%m-%d"),
                     as.Date("2016-05-01", format = "%Y-%m-%d"))) %>%
      select(date, date2, avg_metric_moore) %>% 
      rename(selected_metric = avg_metric_moore)  
    
    output$line_chart_moore <- renderPlotly({
      plot_ly(moore_line_data2, x = ~date, y =~selected_metric, 
              type = 'scatter', mode = 'lines', name = 'Moore Mean Prices') %>%
        add_segments(x = as.Date("2013-05-01", format = "%Y-%m-%d"),
                     xend = as.Date("2013-05-01", format = "%Y-%m-%d"),
                     y = -2000000, yend = 300000000) %>%
        add_segments(x = chosen_month_moore, format = "%Y-%m-%d",
                     xend = chosen_month_moore, format = "%Y-%m-%d",
                     y = -100000, yend = 1000000, line = list(dash="dash")) %>%
      add_text(text = "Tornado",
               textfont = list(size = 10,
                               color = "darkred"),
               x = as.Date("2014-05-01", format = "%Y-%m-%d"),
               y = ifelse(max(moore_line_data2$selected_metric) > 0,
                          max(moore_line_data2$selected_metric) * 0.8,
                          min(moore_line_data2$selected_metric) * 1.2)) %>%
        layout(showlegend = FALSE,
               xaxis = list(range = c(as.Date("2010-05-01", format = "%Y-%m-%d"),
                                      as.Date("2016-05-01", format = "%Y-%m-%d")),
                            tickfont = list(size = 8),
                            title = "Month / Year",
                            titlefont = list(size = 9)),
               yaxis = list(title = y_lab,
                            range = c(ifelse(min(moore_line_data2$selected_metric) > 0,
                                             min(moore_line_data2$selected_metric) * 0.9,
                                             min(moore_line_data2$selected_metric) * 1.1),
                                      ifelse(max(moore_line_data2$selected_metric) > 0,
                                             max(moore_line_data2$selected_metric) * 1.1,
                                             max(moore_line_data2$selected_metric) * 0.9)),
                            tickfont = list(size = 8),
                            titlefont = list(size = 9)),
               title = list(text = ifelse(chosen_metric_moore == "annual_change",
                                          "Avg Annual % Change in HPI \n(+/- 3 yrs of Tornado)",
                                          ifelse(chosen_metric_moore == "HPI",
                                                 "Avg Annual HPI (+/- 3 yrs of Tornado)",
                                                 "Avg Monthly Single Family Home \nValue (+/- 3 yrs of Tornado)")),
                            font = list(size = 11))
               )})
    
    moore_bar_data <- bar_chart_data %>% 
      filter(city == 'Moore') %>%
      filter(zip_code == 73160) %>%
      rename(selected_metric = chosen_metric_moore)
    
    output$bar_chart_moore <- renderPlotly({
      plot_ly(moore_bar_data,
              x=~date,
              y=~selected_metric,
              color=~date,
              type='bar') %>%
        layout(legend = list(orientation = "h",
                             xanchor = "center",
                             x = 0.5,
                             y = -0.2),
               xaxis = list(title = "Event Timeline",
                            titlefont = list(size = 9),
                            tickfont = list(size = 8)),
               yaxis = list(title = y_lab,
                            range=c(min(moore_bar_data$selected_metric) * 0.7,
                                    max(moore_bar_data$selected_metric) * 1.1),
                            tickfont = list(size = 8),
                            titlefont = list(size = 9)),
               title = list(text = "Real Estate Impact by Zip Code \n(+/- 3 months from Tornado)",
                            font = list(size = 11)))
    })
  }) 
  
  
  ### BUFFALO ###
  
  # map for Buffalo NY
  base_buffalo <- refined_buffalo_data %>%
    filter(between(date,
                   as.Date("2011-11-01", format = "%Y-%m-%d"),
                   as.Date("2017-11-01", format = "%Y-%m-%d")))
  
  bins_b = unname(quantile(base_buffalo$annual_change, probs = seq(0, 1, 1/5), na.rm = TRUE))
  
  pal_b = colorBin("RdBu", domain=base_buffalo$annual_change, bins=bins_b)
  
  base_buffalo_date <- base_buffalo %>%
    filter(date == "2014-11-01")
  
  labels_b = sprintf(
    "Zip Code: <strong>%s</strong><br/>Annual Change in HPI: <strong>%g%%</strong>",
    base_buffalo_date$zip_code, base_buffalo_date$annual_change
  ) %>% lapply(htmltools::HTML)
  
  output$disaster_map_buffalo <- renderLeaflet({
    leaflet(base_buffalo_date) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -78.878738, lat = 42.880230, zoom = 11) %>%
      addPolygons(
        fillColor = ~pal_b(annual_change),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8),
        label = labels_b,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto")) %>%
      addLegend(position="bottomright", pal=pal_b, values = ~annual_change, opacity = 0.8, title = "Annual Change in HPI (%)")
  })  
  
  # observing for value changes
  observe({
    
    # input values for date and metric chosen
    chosen_month_buffalo <- input$choose_month_buffalo
    chosen_metric_buffalo <- input$choose_metric_buffalo
    
    # create date field by month only
    refined_buffalo_data$date2 <- format(refined_buffalo_data$date, format="%Y %m")
    chosen_month_buffalo2 <- format(chosen_month_buffalo, format="%Y %m")
    
    # subsets data based on inputs
    interactive_map_buffalo <- refined_buffalo_data %>%
      filter(date2 == chosen_month_buffalo2) %>% 
      select(date2, zip_code, chosen_metric_buffalo) %>% 
      rename(selected_metric = chosen_metric_buffalo)
    
    # doesn't subset on date, use for bins of metrics
    bin_df_buffalo <- refined_buffalo_data %>%
      select(date, zip_code, chosen_metric_buffalo) %>% 
      rename(selected_metric = chosen_metric_buffalo) %>%
      filter(between(date,
                     as.Date("2011-11-01", format = "%Y-%m-%d"),
                     as.Date("2017-11-01", format = "%Y-%m-%d")))
    
    bins = unname(quantile(bin_df_buffalo$selected_metric, probs = seq(0, 1, 1/5), na.rm = TRUE))
    
    # set palette
    if(chosen_metric_buffalo == 'annual_change') {
      
      pal = "RdBu"
      hover = "Zip Code: <strong>%s</strong><br/>Annual Change in HPI: <strong>%g%%</strong>"
      y_lab = "Annual Change in HPI (%)"
      
    } else if(chosen_metric_buffalo == 'HPI') {
      
      pal = "PiYG"
      hover = "Zip Code: <strong>%s</strong><br/>Home Price Index (HPI): <strong>%g</strong>"
      y_lab = "Home Price Index"
      
    } else {
      pal = "RdYlGn"
      hover = "Zip Code: <strong>%s</strong><br/>Single Family Home Value: <strong>$%g</strong>"
      y_lab = "Single Family Home Value ($)"
      
    }
    
    pal_no = colorBin(pal, domain=interactive_map_buffalo$selected_metric, bins=bins)
    
    labels_no = sprintf(
      hover,
      interactive_map_buffalo$zip_code, interactive_map_buffalo$selected_metric
    ) %>% lapply(htmltools::HTML)
    
    # update map
    leafletProxy("disaster_map_buffalo", data=interactive_map_buffalo) %>%
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
      addLegend(position="bottomright", pal=pal_no, values = ~selected_metric, opacity = 0.8, title = y_lab) 
    
    # line chart
    
    buffalo_line_data <- line_chart_data %>%
      filter(city == 'Buffalo')
    
    avg_metric_buffalo = paste0('avg_', chosen_metric_buffalo)
    
    buffalo_line_data$date2 <- format(buffalo_line_data$date, format="%Y %m")
    
    buffalo_line_data2 <- buffalo_line_data %>%
      filter(between(date,
                     as.Date("2011-11-01", format = "%Y-%m-%d"),
                     as.Date("2017-11-01", format = "%Y-%m-%d"))) %>%
      select(date, date2, avg_metric_buffalo) %>% 
      rename(selected_metric = avg_metric_buffalo)  
    
    output$line_chart_buffalo <- renderPlotly({
      plot_ly(buffalo_line_data2, x = ~date, y =~selected_metric, 
              type = 'scatter', mode = 'lines', name = 'Buffalo Mean Prices') %>%
        add_segments(x = as.Date("2014-11-01", format = "%Y-%m-%d"),
                     xend = as.Date("2014-11-01", format = "%Y-%m-%d"),
                     y = -100000, yend = 30000000) %>%
        add_segments(x = chosen_month_buffalo, format = "%Y-%m-%d",
                     xend = chosen_month_buffalo, format = "%Y-%m-%d",
                     y = -100000, yend = 1000000, line = list(dash="dash")) %>%
        add_text(text = "Snowstorm",
                 textfont = list(size = 10,
                                 color = "darkred"),
                 x = as.Date("2014-11-01", format = "%Y-%m-%d"),
                 y = ifelse(max(buffalo_line_data2$selected_metric) > 0,
                            max(buffalo_line_data2$selected_metric) * 0.8,
                            min(buffalo_line_data2$selected_metric) * 1.2)) %>%
        layout(showlegend = FALSE,
               xaxis = list(range = c(as.Date("2011-11-01", format = "%Y-%m-%d"),
                                      as.Date("2017-11-01", format = "%Y-%m-%d")),
                            tickfont = list(size = 8),
                            title = "Month/Year",
                            titlefont = list(size = 9)),
               yaxis = list(title = y_lab,
                            range = c(ifelse(min(buffalo_line_data2$selected_metric) > 0,
                                             min(buffalo_line_data2$selected_metric) * 0.9,
                                             min(buffalo_line_data2$selected_metric) * 1.1),
                                      ifelse(max(buffalo_line_data2$selected_metric) > 0,
                                             max(buffalo_line_data2$selected_metric) * 1.1,
                                             max(buffalo_line_data2$selected_metric) * 0.9)),
                            title = chosen_metric_buffalo,
                            titlefont = list(size = 9),
                            tickfont = list(size = 8)),
               title = list(text = ifelse(chosen_metric_buffalo == "annual_change",
                                          "Avg Annual % Change in HPI (+/- 3 yrs of Snowstorm)",
                                          ifelse(chosen_metric_buffalo == "HPI",
                                          "Avg Annual HPI \n(+/- 3 yrs of Snowstorm)",
                                          "Avg Monthly Single Family Home \nValue (+/- 3 yrs of Snowstorm)")),
                            font = list(size = 11))
               )
      })
    
    buffalo_bar_data <- bar_chart_data %>%
      filter(city == 'Buffalo') %>%
      filter(zip_code == 14201) %>%
      rename(selected_metric = chosen_metric_buffalo)
    
    output$bar_chart_buffalo <- renderPlotly({
      plot_ly(buffalo_bar_data,
              x=~date,
              y=~selected_metric,
              color=~date,
              type='bar') %>%
        layout(legend = list(orientation = "h",
                             xanchor = "center",
                             x = 0.5,
                             y=-0.2),
               xaxis = list(tickfont = list(size = 8),
                            title = "Event Timeline",
                            titlefont = list(size = 9)),
               yaxis = list(title = y_lab,
                            titlefont = list(size = 9),
                            tickfont = list(size = 8),
                            range=c(min(buffalo_bar_data$selected_metric) * 0.7,
                                    max(buffalo_bar_data$selected_metric) * 1.1)),
               title = list(text = "Real Estate Impact by Zip Code \n(+/- 3 months from Snowstorm)",
                            font = list(size = 11)))})
    
  }) 
  
  ##### GRAND ISLE ##############
  
  # map for Grand Isle LA
  base_grandisle <- refined_grandisle_data %>%
    filter(between(date,
                   as.Date("2007-04-01", format = "%Y-%m-%d"),
                   as.Date("2013-04-01", format = "%Y-%m-%d")))
  
  bins_gi = unname(quantile(base_grandisle$annual_change, probs = seq(0, 1, 1/5), na.rm = TRUE))
  
  pal_gi = colorBin("RdBu", domain=base_grandisle$annual_change, bins=bins_gi)
  
  base_grandisle_date <- base_grandisle %>%
    filter(date == "2010-04-01")
  
  labels_gi = sprintf(
    "Zip Code: <strong>%s</strong><br/>Annual Change in HPI: <strong>%g%%</strong>",
    base_grandisle_date$zip_code, base_grandisle_date$annual_change
  ) %>% lapply(htmltools::HTML)
  
  output$disaster_map_grandisle <- renderLeaflet({
    leaflet(base_grandisle_date) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -89.987294, lat = 29.236617, zoom = 11) %>%
      addPolygons(
        fillColor = ~pal_gi(annual_change),
        weight = 2,
        opacity = 1,
        color = "gray",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          fillOpacity = 0.8),
        label = labels_gi,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto")) %>%
      addLegend(position="bottomright", pal=pal_gi, values = ~annual_change, opacity = 0.8, title = "Annual Change in HPI (%)")
  })  
  
  # observing for value changes
  observe({
    
    # input values for date and metric chosen
    chosen_month_grandisle <- input$choose_month_grandisle
    chosen_metric_grandisle <- input$choose_metric_grandisle
    
    # create date field by month only
    refined_grandisle_data$date2 <- format(refined_grandisle_data$date, format="%Y %m")
    chosen_month_grandisle2 <- format(chosen_month_grandisle, format="%Y %m")
    
    # subsets data based on inputs
    interactive_map_grandisle <- refined_grandisle_data %>%
      filter(date2 == chosen_month_grandisle2) %>% 
      select(date2, zip_code, chosen_metric_grandisle) %>% 
      rename(selected_metric = chosen_metric_grandisle)
    
    # doesn't subset on date, use for bins of metrics
    bin_df_grandisle <- refined_grandisle_data %>%
      select(date, zip_code, chosen_metric_grandisle) %>% 
      rename(selected_metric = chosen_metric_grandisle) %>%
      filter(between(date,
                     as.Date("2007-04-01", format = "%Y-%m-%d"),
                     as.Date("2013-04-01", format = "%Y-%m-%d")))
    
    bins = unname(quantile(bin_df_grandisle$selected_metric, probs = seq(0, 1, 1/5), na.rm = TRUE))
    
    if(chosen_metric_grandisle == 'annual_change') {
      
      pal = "RdBu"
      hover = "Zip Code: <strong>%s</strong><br/>Annual Change in HPI: <strong>%g%%</strong>"
      y_lab = "Annual Change in HPI (%)"
      
    } else {
      pal = "RdYlGn"
      hover = "Zip Code: <strong>%s</strong><br/>Single Family Home Value: <strong>$%g</strong>"
      y_lab = "Single Family Home Value ($)"
      
    }
    
    pal_no = colorBin(pal, domain=interactive_map_grandisle$selected_metric, bins=bins)
    
    labels_no = sprintf(
      hover,
      interactive_map_grandisle$zip_code, interactive_map_grandisle$selected_metric
    ) %>% lapply(htmltools::HTML)
    
    # update map
    leafletProxy("disaster_map_grandisle", data=interactive_map_grandisle) %>%
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
      addLegend(position="bottomright", pal=pal_no, values = ~selected_metric, opacity = 0.8, title = y_lab) 
    
    # line chart
    grandisle_line_data <- line_chart_data %>%
      filter(city == 'Grand Isle')
    
    avg_metric_grandisle = paste0('avg_', chosen_metric_grandisle)
    
    grandisle_line_data$date2 <- format(grandisle_line_data$date, format="%Y %m")
    
    grandisle_line_data2 <- grandisle_line_data %>%
      filter(between(date,
                     as.Date("2007-04-01", format = "%Y-%m-%d"),
                     as.Date("2013-04-01", format = "%Y-%m-%d"))) %>%
      select(date, date2, avg_metric_grandisle) %>% 
      rename(selected_metric = avg_metric_grandisle)  
    
    output$line_chart_grandisle <- renderPlotly({
      plot_ly(grandisle_line_data2, x = ~date, y =~selected_metric, 
              type = 'scatter', mode = 'lines', name = 'Grand Isle Mean Prices') %>%
        add_segments(x = as.Date("2010-04-01", format = "%Y-%m-%d"),
                     xend = as.Date("2010-04-01", format = "%Y-%m-%d"),
                     y = -100000000, yend = 3000000) %>%
        add_segments(x = chosen_month_grandisle, format = "%Y-%m-%d",
                     xend = chosen_month_grandisle, format = "%Y-%m-%d",
                     y = -100000, yend = 1000000, line = list(dash="dash")) %>%
        add_text(text = "BP Oil Spill",
                 textfont = list(size = 10,
                                 color = "darkred"),
                 x = as.Date("2010-04-01", format = "%Y-%m-%d"),
                 y = ifelse(max(grandisle_line_data2$selected_metric) > 0,
                            max(grandisle_line_data2$selected_metric) * 0.8,
                            min(grandisle_line_data2$selected_metric) * 1.2)) %>%
        layout(showlegend = FALSE,
               xaxis = list(range = c(as.Date("2007-04-01", format = "%Y-%m-%d"),
                                      as.Date("2013-04-01", format = "%Y-%m-%d")),
                            tickfont = list(size = 8),
                            title = "Month / Year",
                            titlefont = list(size = 9)),
               yaxis = list(title = y_lab,
                            range = c(ifelse(min(grandisle_line_data2$selected_metric) > 0,
                                         min(grandisle_line_data2$selected_metric) * 0.9,
                                         min(grandisle_line_data2$selected_metric) * 1.1),
                                      ifelse(max(grandisle_line_data2$selected_metric) > 0,
                                             max(grandisle_line_data2$selected_metric) * 1.1,
                                             max(grandisle_line_data2$selected_metric) * 0.9)),
                            title = chosen_metric_grandisle,
                            titlefont = list(size = 9),
                            tickfont = list(size = 8)),
               title = list(text = ifelse(chosen_metric_grandisle == 'annual_change',
                              "Avg Annual % Change in Single Family \n Home Value (+/- 3 yrs of Spill)",
                              "Avg Monthly Single Family Home \nValue (+/- 3 yrs of Spill)"),
                            font = list(size = 11)))
    
  }) 
    
    #LINE CHART
    grandisle_bar_data <- bar_chart_data %>%
      filter(city == 'Grand Isle') %>% # only one zip code
      rename(selected_metric = chosen_metric_grandisle)
    
    output$bar_chart_grandisle <- renderPlotly({
      plot_ly(grandisle_bar_data,
              x=~date,
              y=~selected_metric,
              color=~date,
              type='bar') %>%
        layout(legend = list(orientation = "h",
                             xanchor = "center",
                             x = 0.5,
                             y = -0.2),
               yaxis = list(title = y_lab,
                            range=c(ifelse(min(grandisle_bar_data$selected_metric) > 0,
                                           min(grandisle_bar_data$selected_metric) * 0.7,
                                           min(grandisle_bar_data$selected_metric) * 1.1),
                                    ifelse(max(grandisle_bar_data$selected_metric) > 0,
                                           max(grandisle_bar_data$selected_metric) * 1.1,
                                           max(grandisle_bar_data$selected_metric) * 0.7)),
                            tickfont = list(size = 8),
                            titlefont = list(size = 9)),
               xaxis = list(tickfont = list(size = 8),
                            title = "Event Timeline",
                            titlefont = list(size = 9)),
               title = list(text = "Real Estate Impact by Zip Code \n(+/- 3 months from BP Oil Spill)",
                            font = list(size = 11)))
    })
    }) #these closing brackets are for observe
  
}
  
