library(shiny)
library(usmap)
library(sf)


mydata <- read.csv("https://www.betydb.org/miscanthus_county_avg_yield.csv",
                   stringsAsFactors = FALSE)
votes <- readxl::read_excel("C:/Users/Felhasznalo/Documents/votes_projekt.xlsx")
USA <- st_read(dsn = "C:/Users/Felhasznalo/Downloads/cb_2018_us_county_500k.shp")
df <- data.frame(USA)
states_sf <- st_as_sf(USA)

ordered_states <- arrange(states_sf, states_sf$NAME_2)
ordered_votes <- arrange(votes, votes$county_name)

summed_data <- cbind(ordered_states, ordered_votes$votes_dem_2016, ordered_votes$votes_gop_2016)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("USA megyék"),
    
    leafletOutput("map", height = "100vh")
#l
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    bins <- c(0, 5, 10, 15, 20, 25, 30, 35, 40)

    #Sortie map
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

# Run the application 
shinyApp(ui = ui, server = server)
