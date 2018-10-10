# Project 2: Open Data App - an RShiny app development project - Foodie Safeguard

### [Project Description](/project2_desc.md)

![screenshot](doc/Homepage.png)



## Project Title "Foodies Safeguard in NYC"
Term: Fall 2018
#################################################
Final Version: [Project 2 by Group 7](https://mc4398.shinyapps.io/proj/)

+ Team #07

 + team member 1: Mengqi Chen
 + team member 2: Shiqing Long
 + team member 3: Anke Xu
 + team member 4: Sen Fu

+ **Project summary**: 
This project mainly focuses on the relation between crimes and restaurants of year 2018 happened in the new yrok city. Our shiny app is desiged to help people find the safest place to eat. And We tried to provide new visitors who want to have tasty food but also have concerns about safety with some suggestions.


 + Overall crime trend, pattern that crime happened, and crime types difference in each boroughs, and offense level.
 + Which nieghborhood is dangerous? How many crime happened in each neighborhood?
 + Which restaurant to eat when considering price and rating, date, time, and borough?
 + For a selected restaurants, how many crimes happped there during period of Jan,2018 to March,2018?
 + What is the nearest NYPD station to ask for help?

We visualize the information about restaurants and crimes using geograph, time series plot, animated bar chart and animated scatter plot using ploty package.
And we visualize the combination of the information about restaurants and crimes on NYC map using leaflet package.
![screenshot](doc/screenshot1.png)


Our Target Users:
1. Travellers who are new to a place and want to have something to eat
2. Restaurants Owner who want to relocate
3. NYPD

In this second project of GR5243 Applied Data Science, we develop an *Exploratory Data Analysis and Visualization* shiny app on a topic of foodies safeguard using NYC crime data released on the [The Official Website of the City of New York
NYC](https://www1.nyc.gov) and [NYC Open Data] website (https://opendata.cityofnewyork.us). See [Project 2 Description](doc/project2_desc.md) for more details.  

The **Main Goals** for this project is:

- Design and Develop Shiny App/Shiny Server to facilitate users with decision making
- Using Data Science techniques to find business intelligence from data
- Data Merging, Processing and Visualization using Animated Plot, Interactive Geograph
- Design Product to improve user experience and efficiency


+ **Contribution Statement**:

Mengqi Chen: Processed yelp data and crime data. Transfer numeric data into geometric data. Apply the package "leaflet" into the server and ui system. Connect the google API and wrote the neighborhood crime layer. Combined individual parts together and debugged. Improved the server and ui system. Organized the github and completed the readme files. Responsible for presentation. 

Shiqing Long: Cleaned Crime data, Exploratory Data Analysis using package "Plotly", plotted time series plot with range slider and selector buttons, animated scatter plot with mulitple trace, interactive heatmap and animated barchart and summurized business insights. Merged, processed restaurant data to compute the number of crimes around a given restaurants. Added pictures to modify project. Wrote github readme file.

Anke Xu: Constructed the framework of the shinyapp, including the basic ui and server system. Enabled the yelp data, crime data and police station data reactive. Design the ui part, choosing the colors, font styles and the layout. Debugged and improved the shinyapp. Provided ideas to the storytelling and discovered the business insights of the NYC map and each figure.  
Sen Fu: Wrote the police station layer, including data collecting, data processing, points plotting and label adding. Design the algorithm of how to calculate the crime in half a mile circle of each restaurant. Beautify the ui system, such as changing the font styles and colors using CSS and R. 

All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement.


Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.






