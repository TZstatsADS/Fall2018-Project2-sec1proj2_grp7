source("./global.R")

server <- function(input, output){
  
  # map data
  df <- reactive({
    map_data <- geo_join(nyc_neighborhoods, points_crime %>% dplyr::filter(DATE>=as.numeric(input$date[1]),
                                                                           DATE<=as.numeric(input$date[2]),
                                                                           hour>=input$timeinterval[1],
                                                                           hour<=input$timeinterval[2],
                                                                           #shiny_timegroup %in% input$timegroup,
                                                                           shiny_boro %in% input$boro) %>% dplyr::arrange(CMPLNT_DHM) %>% group_by(neighborhood) %>% summarize(num_points=n()), "neighborhood", "neighborhood")
    
    
    
  })
  
  
  # summary crime number 
  df_num <- reactive({
    points_crime %>% dplyr::filter(DATE>=as.numeric(input$date[1]),
                                   DATE<=as.numeric(input$date[2]),
                                   hour>=input$timeinterval[1],
                                   hour<=input$timeinterval[2],
                                   #shiny_timegroup %in% input$timegroup,
                                   shiny_boro %in% input$boro) %>% dplyr::arrange(CMPLNT_DHM) %>% group_by(neighborhood) %>% summarize(num_points=n())
    
    
    
  })
  
  # restaurant data
  
  res_df <- reactive({
    yelp_data %>% dplyr::filter(rating >= input$res_rating[1],
                                rating <= input$res_rating[2],
                                price %in% input$res_price) 
  })
  
  
  output$nyc_map <- renderLeaflet({
    #the restaurant icon
    restaurantIcon <- icons(
      iconUrl = "../doc/figs/icon.png",
      iconWidth = 25, iconHeight = 20,
      iconAnchorX = 22, iconAnchorY = 94
    )
    
    # crime in 0.5 mile radius circle of a restaurant
    
    
    res_popup <- paste0("<a href='",
                        yelp_data$url,
                        "' target='_blank'>",
                        yelp_data$name,"</a>","<br>",
                        yelp_data$address,"<br>",
                        yelp_data$city,"\t",
                        yelp_data$state,"\t",
                        yelp_data$zip_code,"<br>",
                        "Rating: ",
                        yelp_data$rating_sign,"<br>",
                        "Price: ",
                        yelp_data$price_sign,"<br>",
                        "Crime Number in 0.5 mile radius circle: ","<br>",
                        res_crime_count$counts, "<br>")
    
    
    # the crime label
    crime_popup <- paste0("Neighborhood: ",
                          df()$neighborhood,"</ a>","<br>",
                          "Crime Number: ",
                          df()$num_points,"<br>")
    
    
    
    #the police precinct icon
    policeIcon <- icons(
      iconUrl = "../doc/figs/police_icon.png",
      iconWidth = 25, iconHeight = 20,
      iconAnchorX = 22, iconAnchorY = 94
    ) 
    
    
    qpal <- colorQuantile("Blues", points_by_neighborhood$num_points, n = 5)
    pal <- colorNumeric(
      palette = "Blues",
      domain = points_by_neighborhood$num_points
    )
    
    leaflet()%>% 
      addTiles()%>%
      addProviderTiles("CartoDB.Positron") %>%
      #the restaurant map
      addMarkers( data = as.data.frame(res_df()),  ~longitude, ~latitude, popup = res_popup, icon = restaurantIcon,
                  clusterOptions = markerClusterOptions(), group = "Restaurants") %>%
      #the police map 
      addMarkers( data = police, ~Longitude, ~Latitude, popup = ~as.character(police.precinct.address), icon = policeIcon,
                  group = "Police Stations")  %>%
      #the crime map
      addPolygons(data = df(), fillOpacity = 0.8, fillColor = ~qpal(num_points), weight = 0.5,
                  popup = crime_popup, group = "Crime") %>%
      # addLegend(data = df(),"bottomright", pal = pal, values = ~num_points,
      #           title = "Crime Counts",
      #           labFormat = labelFormat(),
      #           opacity = 1, group = "Crime"
      # )%>%
      addLayersControl(
        overlayGroups = c("Crime","Restaurants", "Police Stations"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(-73.98, 40.75, zoom = 13)
  })
  
  # print summary
  output$summary_text <- renderText({
    paste("The map exhibits the total number of crime is", as.character(nrow(df_num())),"between",input$date[1], "and",input$date[2] )
    #paste(nrow(res_df()))
    
  })
  
  # crime trend
  output$crime_trend <- renderPlotly({
    plot_ly(crime_trend, x = crime_trend$CMPLNT_FR_DT) %>%
      add_lines(y = crime_trend$n, name = "Number of Crimes")%>%
      layout(
        title = "Crime Trend", 
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(
                count = 7,
                label = "1 Week ",
                step = "day",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 Month",
                step = "month",
                stepmode = "backward"),
              list(step = "all"))),
          
          rangeslider = list(type = "date")),
        
        yaxis = list(title = "Crime")) %>%
      
      
      layout( shapes  = list(
        list(type = "line", fillcolor = "red", 
             line    = list(color = "red"), 
             opacity = 1, 
             x0      = as.Date("2018-01-01"),
             x1      = as.Date("2018-01-01"),
             xref    = "x",
             y0      = 0,
             y1      = 1396,
             yref    = "y"),
        list(type = "line", fillcolor = "red", 
             line    = list(color = "red"), 
             opacity = 1, 
             x0      = as.Date("2018-02-01"),
             x1      = as.Date("2018-02-01"),
             xref    = "x",
             y0      = 0,
             y1      = 1346,
             yref    = "y"),
        list(type = "line", fillcolor = "red", 
             line    = list(color = "red"), 
             opacity = 1, 
             x0      = as.Date("2018-03-01"),
             x1      = as.Date("2018-03-01"),
             xref    = "x",
             y0      = 0,
             y1      = 1327,
             yref    = "y")))
  })
  
  
  # the proportion of offense level
  output$offense_hour <- renderPlotly({
    offense_hour <- crime18 %>%
      select(hour,CRM_ATPT_CPTD_CD,LAW_CAT_CD)%>%
      group_by(hour,CRM_ATPT_CPTD_CD,LAW_CAT_CD)%>%
      summarise(n = n())%>%
      mutate(freq = n / sum(n))
    
    offense_hour_plot <- offense_hour %>%
      plot_ly(
        x = ~hour, 
        y = ~freq, 
        size = ~n, 
        color = ~LAW_CAT_CD, 
        frame = ~CRM_ATPT_CPTD_CD, 
        hoverinfo = "text",
        type = 'scatter',
        mode = 'markers'
      ) %>%
      
      layout(
        xaxis = list(
          title = "Hour",
          zeroline = F
        ),
        yaxis = list(
          title = "The Proportion of Level of Offense",
          zeroline = F
        ))
    offense_hour_plot
    
    #%>%
    #animation_slider(
    #hide = T)
    
  })
  
  
  # crime proportion by borough
  output$crime_type <- renderPlotly({
    df_crime_type <- crime18%>%
      filter(SUSP_SEX %in% c('F', 'M'))%>%
      filter(BORO_NM %in% c("BRONX", "BROOKLYN","MANHATTAN" ,"QUEENS","STATEN ISLAND"))%>%
      mutate(Boro = as.factor(BORO_NM))%>%
      group_by(SUSP_SEX,Boro,OFNS_DESC)%>%
      summarise(n = n())%>%
      mutate(freq = n / sum(n))
    
    top_5_crime_type <- df_crime_type%>%
      filter(OFNS_DESC %in% c("HARRASSMENT 2","ASSAULT 3 & RELATED OFFENSES","PETIT LARCENY",
                              "DANGEROUS DRUGS","OFF. AGNST PUB ORD SENSBLTY &"))
    
    top_5_crime_type%>%
      plot_ly(
        x = ~Boro, 
        y = ~freq, 
        color = ~SUSP_SEX,
        frame = ~OFNS_DESC, 
        opacity=0.8,
        hoverinfo = "text",
        type = 'bar'
      )%>%
      layout(
        xaxis = list(
          title = "Borough",
          zeroline = F
        ),
        yaxis = list(
          title = "The Proportion of Crime",
          zeroline = F))
  })
  
  # Heatmap
  output$heatmap <- renderPlotly({
    df_hour_week <- crime18%>%
      group_by(week, hour)%>%
      summarise(n = n_distinct(CMPLNT_NUM))
    
    df_hour_week$week <-ordered(df_hour_week$week, levels = c("Sunday", "Monday", 
                                                              "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    df_hour_week <- df_hour_week[order(df_hour_week$week),]
    
    
    plot_ly(df_hour_week, x = ~week, y = ~hour) %>%
      add_heatmap(z = ~n) %>%
      colorbar() %>%
      layout(
        xaxis = list(
          title = "Week",
          zeroline = F
        ),
        yaxis = list(
          title = "Time",
          zeroline = F))
  })
  
  output$about_us <- renderText({
    paste("Our group members: ", "\t","<br>",
          "Mengqi Chen",  "\t","<br>",
          "Sen Fu","\t", "<br>",
          "Shiqing Long", "\t", "<br>",
          "Anke Xu", "\t", "<br>","<br>",
          "Data Source: ","\t","<br>",
          "NYC Open Data")
  })
  
  
}

shinyApp(ui, server)

# library(rsconnect)
# rsconnect::setAccountInfo(name='mc4398', 
#                           token='59C79368DCAD21E928BB9EC5E1CB4B1D', 
#                           secret='PlZLzSZee2x74CWlyeicfQDvHmFI7DLdMdozQQW6')
# deployApp(account='mc4398')
