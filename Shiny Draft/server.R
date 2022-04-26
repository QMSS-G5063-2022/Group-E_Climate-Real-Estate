


# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)

function(input, output, session){
  
  date_neworleans <- reactive({input$choose_month_neworleans})
  date_coffeypark <- reactive({input$choose_month_coffey})
  date_mooreok <- reactive({input$choose_month_moore})
  date_buffalo <- reactive({input$choose_month_buffalo})
  date_boulder <- reactive({input$choose_month_boulder})
  date_grandisle <- reactive({input$choose_month_grandisle})
  
  ### NEW ORLEANS ###
  
  
  ### COFFEY PARK ###

  # load data
  cali_rent <- read.csv("../data/single_family_homes_time_series.csv")
  cali_shape <- readOGR("../data/shape files", "California_Zip_Codes")
  
  # transform data - rent data
  cali_rent <- cali_rent %>%
    select(region_id, date, value) %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d"))
  
  colnames(cali_rent) <- c("zip_code", "date", "median_sale_price")
  
  # transform data - shape file
  cali_shape <- cali_shape[,-c(1,4,5,6,7,8,9)]
  
  new_cali_shape <- cali_shape %>%
    rename(zip_code = ZIP_CODE, po_name = PO_NAME) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # merge data sources
  cali_data <- merge(new_cali_shape, cali_rent, by = "zip_code", duplicateGeoms = T)
  
  new_cali_data_2 <- cali_data %>%
    filter(zip_code %in% c(95401, 95402, 95403, 95404, 95405, 95406, 95407, 95409)) %>%
    st_as_sf(cali_data)
  
  bins <-c(100000, 200000, 300000, 400000, 500000)
  pal <-colorBin("YlOrRd", domain=cali_data_data_26$median_sale_price, bins=bins)
  
  output$coffey_park_map <- leaflet::renderLeaflet({
    new_cali_data_2 %>%
      filter(date == input$choose_month_coffey) %>%
      leaflet() %>%
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
          fillOpacity = 0.8),
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                    textsize = "15px",
                                    direction = "auto")) %>%
      addLegend(pal=pal, values = ~median_sale_price, opacity = 0.8, title = "Mean Rent")})
  
  labels <- sprintf(
    "<strong>%s</strong><br/>Mean Rent: %g",
    cali_data$zip_code, cali_data_sep_2016$median_salce_price
  ) %>% lapply(htmltools::HTML)
  

  ### DENVER ###


  ### BUFFALO ###


  ### MOORE, OK ####
  moore_ok <- base_real_estate %>%
    filter(zip_code %in% c(73160, 73165, 73170),  year %in% c(2011, 2012, 2013, 2014, 2015))
  
  moore_ok_shape <- readOGR(dsn="./data/shape files", layer="Moore_OK")
  
  moore_ok_shape <- moore_ok_shape[,-c(2,3,4,5,6,7,8,9)]
  
  moore_ok_shape2 <- moore_ok_shape %>%
    rename(zip_code = ZCTA5CE10) %>%
    spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # merge data
  moore_ok_data2 <- merge(moore_ok_shape2, moore_ok, by = "zip_code", duplicateGeoms = T) %>%
    st_as_sf()

  st_write(moore_ok_data2, './data/shape files/Moore_Tornado.shp')


  ### GRAND ISLE ###

}