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
                         leafletOutput("nyc_map"),"Summary",
                         verbatimTextOutput("summary_text"))
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
    tabPanel("About us",
             mainPanel(textOutput("about_us"))
    )
    
  ))  