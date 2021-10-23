install.packages(shiny)
install.packages(usmap)
install.packages(sf)
install.packages(tidyverse)
install.packages(leaflet)
install.packages(stringr)
install.packages(rgdal)
install.packages(RColorBrewer)

library(shiny)
library(usmap)
library(sf)
library(tidyverse)
library(leaflet)
library(stringr)
library(rgdal)
library(RColorBrewer)


votes <- readxl::read_excel("C:/Users/Felhasznalo/Documents/votes_projekt.xlsx")
votes$county_name <- str_replace(votes$county_name, " County", "")
USA <- st_read(dsn = "C:/Users/Felhasznalo/Downloads/cb_2018_us_county_500k.shp")

USA$county_name <-  USA$NAME

x <- merge(votes, USA, by = "county_name", all.x = TRUE)


# SHINY -------------------------------------------------------------------



ui <- fluidPage(
    titlePanel(h1("USA megyék")),
    
    sidebarLayout (
        sidebarPanel (
            radioButtons (inputId = "gomb",  
                          label = "Változók",
                          choices = c("Trump", "Clinton", "Race",
                                      "Népsűrűség", "Jövedelmi szint",
                                      "Százalékpontos eltérés"))
        ),
        mainPanel (
            img(src = "Donald-Trump-election-Hillary-Clinton-2016.jpg", height = 140, width = 200),
            leafletOutput("map", height = "100vh")
        ))
    

    
)

server <- function(input, output, session) {
    
    subdata <- reactive({
        valt <- switch (input$gomb,
            "Trump" = x$Trump,
            "Clinton" = x$Clinton,
            "Race" = x$Black,
            "Népsűrűség" = x$Density,
            "Jövedelmi szint" = x$Income,
            "Százalékpontos eltérés" =x$per_point_diff_2016)
    })

    
    paletta <- colorBin("Red", domain = NULL)
    

    output$map <- renderLeaflet({
        leaflet()%>%
            addProviderTiles("OpenStreetMap.Mapnik")%>%
            setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>%
            addPolygons(
                data = x$geometry,
                stroke = T,
                weight = 1,
                color = "black",
                fillColor = paletta(subdata()),
                smoothFactor = 0.2,
                fillOpacity = 0.2,
                popup = paste(" Megye: ", x$county_name,
                              "<br>", "Republikánus: ", x$votes_gop_2016, 
                              "<br>", "Demokrata: ", x$votes_dem_2016,
                              "<br>", "Fehérek: ", x$White,
                              "<br>", "Feketék: ", x$Black,
                              "<br>", "Latinok: ", x$Hispanic,
                              "<br>", "Jövedelmi szint: ", x$Income))

    })
    
}

shinyApp(ui = ui, server = server)
