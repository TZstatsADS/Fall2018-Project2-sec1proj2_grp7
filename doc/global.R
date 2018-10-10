
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
library(httr)
#httr::config(ssl_verifypeer=0L)
#install_github("dgrtwo/gganimate")



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