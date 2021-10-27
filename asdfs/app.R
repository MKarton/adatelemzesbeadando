library(leaflet)
library(tidyverse)
library(ggplot2)
library(sf)
library(shiny)

USA <- st_read(dsn = 'C:/Users/Felhasznalo/Downloads/cb_2018_us_county_500k.shp')

### Get data
mydata <- read.csv("https://www.betydb.org/miscanthus_county_avg_yield.csv",
                   stringsAsFactors = FALSE)

states_sf <- st_as_sf(USA)

mydata2<-mydata[,c("COUNTY_NAME","Avg_yield")]
colnames(mydata2)[1]<-"NAME"

## merge shape file with data
states_sf_coef <- left_join(states_sf, mydata2, by = "NAME")


ui <- fluidPage(
    
    leafletOutput("map", height = "100vh")
)

server <- function(input, output, session) {
    
    bins <- c(0, 5, 10, 15, 20, 25, 30, 35, 40)
    mypal <- colorBin("YlOrRd", domain = states_sf_coef$Avg_yield, bins = bins)
    
    #Sortie map
    output$map <- renderLeaflet({
        leaflet()%>%
            addProviderTiles("OpenStreetMap.Mapnik")%>%
            setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
            addPolygons(
                data = states_sf_coef,
                fillColor = ~mypal(states_sf_coef$Avg_yield),
                stroke = FALSE,
                smoothFactor = 0.2,
                fillOpacity = 0.3,
                popup = paste("Region: ", states_sf_coef$NAME_2, "<br>",
                              "Avg_yield: ", states_sf_coef$Avg_yield, "<br>"))%>%
            addLegend(position = "bottomleft",
                      pal = mypal,
                      values = states_sf_coef$Avg_yield,
                      title = "Avg_yield",
                      opacity = 1)
    })
    
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)