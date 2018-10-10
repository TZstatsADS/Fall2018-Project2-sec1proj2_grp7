
############################ Final Version #####################

packages <- c("RColorBrewer", "wordcloud", "ggplot2", "ggmap", "dplyr", "shiny",
              "lubridate", "maps", "mapproj", "gganimate", "shinythemes","leaflet","rgdal","tigris","ggraph","gapminder")

# # check packages that need to be installed.
packages.needed=setdiff(packages,
                        intersect(installed.packages()[,1],
                                  packages))
# # install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(devtools)
library(RCurl)
library(httr)
#set_config( config( ssl_verifypeer = 0L ) )
#install_github("dgrtwo/gganimate")

library(RColorBrewer)
library(bitops)
library(wordcloud)
library(ggplot2)
library(ggmap)
library(dplyr)
library(shiny)
library(lubridate)
library(maps)
library(mapproj)
library(gganimate)
library(shinythemes)
library(leaflet)
library(rgdal)
library(tigris)
library(ggraph)
library(plotly)
library(gapminder)
library(gifski)

############################# Data Process #############################
ori_data <- read.csv("../data/NYPD_Complaint_Data_Current_YTD.csv")

ori_data$CMPLNT_DHM <- paste(ori_data$CMPLNT_FR_DT,ori_data$CMPLNT_FR_TM)
ori_data$CMPLNT_DHM <- mdy_hms(ori_data$CMPLNT_DHM)
ori_data$CMPLNT_WEEK <- ifelse(wday(ori_data$CMPLNT_DHM)==1,"Sunday", 
                               ifelse(wday(ori_data$CMPLNT_DHM)==2,"Monday",
                                      ifelse(wday(ori_data$CMPLNT_DHM)==3,"Tuesday",
                                             ifelse(wday(ori_data$CMPLNT_DHM)==4,"Wednesday",
                                                    ifelse(wday(ori_data$CMPLNT_DHM)==5,"Thursday",
                                                           ifelse(wday(ori_data$CMPLNT_DHM)==6,"Friday","Saturday") ) ))) )

ori_data$year <- year(ori_data$CMPLNT_DHM)



vis_data <- ori_data[,c("CMPLNT_NUM","BORO_NM","ADDR_PCT_CD","JURIS_DESC","KY_CD","LAW_CAT_CD",
                        "OFNS_DESC","SUSP_AGE_GROUP","SUSP_RACE","SUSP_SEX","VIC_AGE_GROUP",
                        "VIC_RACE","VIC_SEX","Latitude","Longitude","CMPLNT_DHM","CMPLNT_WEEK","year")] %>% dplyr::filter(year>=2015)  %>% dplyr::arrange(CMPLNT_DHM)


vis_data$DATE <- date(vis_data$CMPLNT_DHM)
vis_data$hour <- hour(vis_data$CMPLNT_DHM)

vis_data$timeframe <- ifelse(vis_data$hour %in% seq(3,6),"wee hours",
                             ifelse(vis_data$hour %in% seq(7,10),"morning",
                                    ifelse(vis_data$hour %in% seq(11,14),"noon",
                                           ifelse(vis_data$hour %in% seq(15,18),"afternoon",
                                                  ifelse(vis_data$hour %in% seq(19,22),"night","midnight")))))
vis_data$shiny_timegroup <- ifelse(vis_data$timeframe == "wee hours",1,
                                   ifelse(vis_data$timeframe == "morning",2,
                                          ifelse(vis_data$timeframe == "noon",3,
                                                 ifelse(vis_data$timeframe=="afternoon",4,
                                                        ifelse(vis_data$timeframe=="night",5,6)))))

vis_data$shiny_boro <- ifelse(vis_data$BORO_NM == "BROOKLYN",1,
                              ifelse(vis_data$BORO_NM == "QUEENS",2,
                                     ifelse(vis_data$BORO_NM == "BRONX",3,
                                            ifelse(vis_data$BORO_NM == "MANHATTAN",4,5))))

# Keep the same name of latitude and longitude 
colnames(vis_data)[14:15] <- c("latitude", "longitude")

# Remove the NA in latitude and longitude columns in vis_data
vis_data <- vis_data[!is.na(vis_data$longitude),]
vis_data <- vis_data[!is.na(vis_data$latitude),]

max_date <- max(vis_data$DATE)
min_date <- min(vis_data$DATE)
# connect the api from google
register_google(key = "AIzaSyBP-M-7-h0N7vEtk4hicfMato8iYsCPNB0")
register_google(key = "AIzaSyBP-M-7-h0N7vEtk4hicfMato8iYsCPNB0", account_type = "premium", day_limit = 5000)
# get NYC map from Google 
leaflet() %>%
  addTiles() %>%
  setView(-74.00, 40.71, zoom = 12)

# divide the nyc neighborhood
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
summary(nyc_neighborhoods)

# get the nyc neighborhoods map
leaflet(nyc_neighborhoods) %>%
  addTiles() %>% 
  addPolygons(popup = ~neighborhood) %>%
  addProviderTiles("CartoDB.Positron")

# read yelp data
yelp_data <- read.csv("../data/yelp_data.csv", header = T, stringsAsFactors = F)

res_crime_count <- read.csv("../data/rescount18.csv", stringsAsFactors = F)
yelp_data$res_crime_count <- res_crime_count$counts

yelp_data$rating <- as.character(yelp_data$rating)
yelp_data <- yelp_data[which(yelp_data$rating %in% c("1","2","3","3.5","4","4.5","5")),]
yelp_data$rating <- as.numeric(yelp_data$rating)
# the restaurant label
yelp_data$rating_sign[yelp_data$rating >= 4.5] <- "★★★★☆"
yelp_data$rating_sign[(yelp_data$rating < 4.5) & (yelp_data$rating >= 4)] <- "★★★★"
yelp_data$rating_sign[(yelp_data$rating < 4) & (yelp_data$rating >= 3.5)] <- "★★★☆"
yelp_data$rating_sign[(yelp_data$rating < 3.5) & (yelp_data$rating >= 3)] <- "★★★"
yelp_data$rating_sign[(yelp_data$rating < 3) & (yelp_data$rating >= 2.5)] <- "★★☆"
yelp_data$rating_sign[(yelp_data$rating < 2.5) & (yelp_data$rating >= 2)] <- "★★"
yelp_data$rating_sign[(yelp_data$rating < 2) & (yelp_data$rating >= 1.5)] <- "★☆"
yelp_data$rating_sign[(yelp_data$rating < 1.5) & (yelp_data$rating >= 1)] <- "★"
yelp_data$rating_sign[(yelp_data$rating < 1) & (yelp_data$rating >= 0.5)] <- "☆"

yelp_data$price_sign[yelp_data$price == 4] <- "$$$$"
yelp_data$price_sign[yelp_data$price == 3] <- "$$$"
yelp_data$price_sign[yelp_data$price == 2] <- "$$"
yelp_data$price_sign[yelp_data$price == 1] <- "$"



# transfer yelp data frame to a spatial data frame
points_yelp <- data.frame(yelp_data)
points_spdf_y <- points_yelp
coordinates(points_spdf_y) <- ~ longitude + latitude
proj4string(points_spdf_y) <- proj4string(nyc_neighborhoods)
matches_yelp <- over(points_spdf_y, nyc_neighborhoods)
points_yelp  <- cbind(points_yelp , matches_yelp)
points_yelp

# transfer crime data frame to a spatial data frame
points_crime <- data.frame(vis_data)
points_spdf_c <- points_crime
coordinates(points_spdf_c) <- ~ longitude + latitude
proj4string(points_spdf_c) <- proj4string(nyc_neighborhoods)
matches_crime <- over(points_spdf_c, nyc_neighborhoods)
points_crime <- cbind(points_crime, matches_crime)
points_crime

## count the number of points in crime data in each polygon and map that to a color
points_by_neighborhood <- points_crime %>%
  group_by(neighborhood) %>%
  summarize(num_points=n())

#map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")



# add labels to the points in the map
content <- c()
for (i in 1:length(yelp_data)) {
  content[i] <- paste(sep = "<br/>",
                      yelp_data$url[i],
                      yelp_data$address[i],
                      yelp_data$city[i],
                      yelp_data$state[i],
                      yelp_data$zip_code[i]
  )
}


leaflet() %>% addTiles() %>%
  addPopups(-122.327298, 47.597131, content,
            options = popupOptions(closeButton = FALSE)
  )


## Crime Trend Visualization 
crime <- read.csv("../data/crime.csv", stringsAsFactors = F)


crime18 <- crime %>%
  filter(CMPLNT_FR_DT > "2018-01-00") %>%
  mutate(CMPLNT_FR_DT = as.Date(CMPLNT_FR_DT)) %>%
  mutate(LAW_CAT_CD = as.factor(LAW_CAT_CD)) %>%
  mutate(CRM_ATPT_CPTD_CD = as.factor(CRM_ATPT_CPTD_CD))
crime18$month <- gsub(x = crime18$CMPLNT_FR_DT, pattern ="[0-9]{4}-", replacement ="")
crime18$month <- as.factor(gsub(x = crime18$month, pattern ="-[0-9]{2}", replacement =""))
crime18$hour <- as.factor(gsub(x = crime18$CMPLNT_FR_TM, pattern =":[0-9]{2}:[0-9]{2}", replacement =""))
crime18$week <- as.factor(weekdays(crime18$CMPLNT_FR_DT))

crime_trend <- crime18%>%
  select(CMPLNT_FR_DT,CMPLNT_FR_TM)%>%
  group_by(CMPLNT_FR_DT)%>%
  summarise(n = n())

crime_trend <- data.frame(crime_trend)




# read police precinct data
police <- read.csv("../data/police.csv", stringsAsFactors = F)
colnames(police) <- c("id", "police.precinct.address","Latitude", "Longitude")

## The Amimated plot for Proportion of Level of Offense by hours


############################# Shiny ui & server #############################

##################
ui <- fluidPage(
  # themeSelector(),
  titlePanel("NYC Crime Data Visualization"),
  tabsetPanel(
    tabPanel("Data Map Visualization",
             sidebarLayout(
               sidebarPanel(
                 h2("Select Factors:"),
                 dateRangeInput("date", label = h3("Date input"), start = "2018-01-01",
                                end = "2018-03-31",min = min_date,max = max_date),
                 sliderInput("timeinterval", label = h3("Select Time:"), min = 0, max = 23,
                             value = c(0,23)),
                 selectizeInput("boro",label = "Select Borough:",
                                choices = list("BROOKLYN" = 1, "QUEENS" = 2,
                                               "BRONX" = 3, "MANHATTAN" = 4, 
                                               "STATEN ISLAND" = 5), multiple = TRUE),
                 sliderInput("res_rating", label = h3("Restaurant Rating"), min = 1.0, max = 5.0,
                             value = c(3.5,4.5),step = 0.5),
                 checkboxGroupInput("res_price", label = h3("Restaurant Price Level"),
                                    choices = list("$"=1, "$$"=2, "$$$"=3, "$$$$"=4),
                                    selected = 2)
              
               ),
               mainPanel("map",
                         leafletOutput("nyc_map")
                         )
             )
    ),
    tabPanel("Crime Trend",
             #sidebarLayout(
             mainPanel(
               plotlyOutput("crime_trend")
               #)
             )
    ),
    tabPanel("Offense Proportion by Hour",
            mainPanel(plotlyOutput("offense_hour"))
    ),
    tabPanel("Offense Proportion by Borough",
             mainPanel(plotlyOutput("crime_type"))
    ),
    tabPanel("Offense Date and Time",
             mainPanel(plotlyOutput("heatmap"))
    ),
    tabPanel("Contact",fluidPage(
      sidebarLayout(
        sidebarPanel(
          h2("Contact Information"),
          hr(),
          h6("We are all Columbia University students at Department of Statistics.
             If you are interested in our project, you can contact us."),
          hr(),
          h6("Mengqi Chen  "),
          h6("Shiqing Long   "),
          h6("Anke Xu   "),
          h6("Sen Fu  "),
          h6("Data Source: "),
          h6("NYC Open Data")),
          mainPanel(
            fluidRow(tags$img(height = 120, src = "../doc/figs/smile.png"),align="center"))
          )
        )
      )
    )
  )
    


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
      # iconUrl = "C:\\Users\\Anke Xu\\Documents\\GitHub\\Fall2018-Project2-sec1proj2_grp7\\doc\\figs\\Restaurant.png",
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
       addLegend(data = df(),"bottomright", pal = pal, values = ~num_points,
                 title = "Crime Counts",
                labFormat = labelFormat(),
                 opacity = 1, group = "Crime"
       )%>%
      addLayersControl(
        overlayGroups = c("Crime","Restaurants", "Police Stations"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(-73.98, 40.75, zoom = 13)
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
  
  # output$about_us <- renderText({
  #   paste("Our group members: ", "\t","<br>",
  #         "Mengqi Chen",  "\t","<br>",
  #         "Sen Fu","\t", "<br>",
  #         "Shiqing Long", "\t", "<br>",
  #         "Anke Xu", "\t", "<br>","<br>",
  #         "Data Source: ","\t","<br>",
  #         "NYC Open Data")
  # })
  
  
}

shinyApp(ui, server)

# library(rsconnect)
# rsconnect::setAccountInfo(name='mc4398',
#                           token='59C79368DCAD21E928BB9EC5E1CB4B1D',
#                           secret='PlZLzSZee2x74CWlyeicfQDvHmFI7DLdMdozQQW6')
# deployApp(account='mc4398')
