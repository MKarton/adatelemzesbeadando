library(shiny)
library(usmap)
library(sf)
library(tidyverse)
library(leaflet)
library(stringr)


votes <- readxl::read_excel("C:/Users/Felhasznalo/Documents/votes_projekt.xlsx")
votes$county_name <- str_replace(votes$county_name, " County", "")
USA <- st_read(dsn = "C:/Users/Felhasznalo/Downloads/cb_2018_us_county_500k.shp")
df <- data.frame(USA)
states_sf <- st_as_sf(USA)

ordered_states <- arrange(states_sf, states_sf$NAME)
ordered_votes <- arrange(votes, votes$county_name)

hiany <- setdiff(unique(ordered_votes$county_name), unique(ordered_states$NAME))

for (i in 1:106){
df_USA <- subset(ordered_states, ordered_states$NAME != hiany[i])
}


ui <- fluidPage(
    titlePanel("USA megyék"),
    
    leafletOutput("map", height = "100vh")
    
)

server <- function(input, output, session) {
    
    bins <- c(0, 5, 10, 15, 20, 25, 30, 35, 40)

    output$map <- renderLeaflet({
        leaflet()%>%
            addProviderTiles("OpenStreetMap.Mapnik")%>%
            setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
            addPolygons(
                data = USA,
                stroke = FALSE,
                smoothFactor = 0.2,
                fillOpacity = 0.3,
                popup = paste("Region:"))
    })
    
}

shinyApp(ui = ui, server = server)
