#install.packages("tidyverse")
#install.packages("shiny")
#install.packages("sf")
#install.packages("leaflet")
#install.packages("RColorBrewer")
#install.packages("devtools")
#install.packages("shinyWidgets")
#install.packages("ggcorrplot")

# Könyvtárak betöltése
library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(RColorBrewer)
library(devtools)
library(shinyWidgets)
library(ggcorrplot)

# A következő package-hez a githubról installálnunk kell egy fájlt
#devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

# Megyei adatbázis behívása
county_data <- readxl::read_excel(str_c(getwd(), "/votes.xlsx"))
map_data_raw <- get_urbn_map("counties", sf = TRUE)
map_data <- select(map_data_raw, 1)
map_data <- st_transform(map_data, "+proj=longlat +datum=WGS84") #2x kell lefuttatni
map_data <- st_transform(map_data, "+proj=longlat +datum=WGS84") #2x kell lefuttatni

# Vallási adatbázis behívása, tisztítása
religion_data_raw <- readxl::read_excel(str_c(getwd(),"/religion.xlsx"))
religion_data <- religion_data_raw[c("FIPS", "TOTRATE", "EVANRATE", "CATHRATE", "LDSRATE")]
religion_data[,2:5] <- religion_data[,2:5] / 10
religion_data <- rename(religion_data, "county_fips" = "FIPS")
religion_data[is.na(religion_data)] <- 0

# Adattáblák összefűzése megyék neve szerint. Left joinnal operálunk, a county_data a mérvadó nekünk.
x <- merge(merge(county_data, map_data, by = "county_fips", all.x = T),
           religion_data, by = "county_fips", all.x = T)

# Új változó létrehozása, mely akkor vesz fel IGAZ (1) értéket, ha az adott megyében Trump nyert.
x$Trump_győzelem <- x$per_point_diff_2016 < 0


# SHINY -------------------------------------------------------------------

ui <- fluidPage(
    # Háttérkép kiválasztása dnn-ről
    setBackgroundImage(
        src = "https://cdn.cnn.com/cnnnext/dam/assets/160811134259-hillary-clinton-donald-trump-joyce-tseng-composite-super-169.jpg"
    ),
    titlePanel("2016-os elnökválasztás az Amerikai Egyesült Államokban"),
    # Navigációs menü létrehozása
    navbarPage("Menü",
               # Első fül: térkép
               tabPanel("Térkép",
                        sidebarLayout (
                            sidebarPanel (
                                # gomb típusának beállítása
                                radioButtons (inputId = "gomb",  
                                              label = "Változók",
                                              choices = c("Százalékpontos különbség", "Feketék", "Latinók",
                                                          "Jövedelmi szint", "Katolikusok"
                                              ))),
                            
                            # output
                            mainPanel (leafletOutput("map", height = "70vh")))),
               
               # Pontdiagram
               tabPanel ("Pontdiagramok",
                         sidebarLayout(sidebarPanel (
                             radioButtons (inputId = "gomb2",  
                                           label = "Y-tengely",
                                           choices = c("Fehérek", "Feketék", "Latinók", "Jövedelmi szint",
                                                       "Főiskolát végzettek", "65 éven felüliek",
                                                       "Ingatlanok átlagos értéke", "Evangélikusok",
                                                       "Katolikusok", "Mormonok"))),
                             # Output
                             mainPanel(
                                 plotOutput ("scatter")))),
               
               # Hipotézisvizsgálat, ábrával együtt
               tabPanel("Vegyes kapcsolatok",
                        sidebarLayout(sidebarPanel(
                            selectInput (inputId = "numvált",
                                         label = "Num. vált." ,
                                         choices = c ("Fehérek",
                                                      "Feketék",
                                                      "Latinók",
                                                      "Népsűrűség",
                                                      "Jövedelmi szint",
                                                      "Főiskolát végzettek",
                                                      "65 éven felüliek",
                                                      "Ingatlanok átlagos értéke",
                                                      "Evangélikusok",
                                                      "Katolikusok",
                                                      "Mormonok"))),
                            #Output     
                            mainPanel(
                                verbatimTextOutput("kruskal"),
                                plotOutput("ábra"))
                        )
               ),
               # Hisztogram készítése, mely a jövedelem függvényében megmutattja a népességet és a szavazati különbséget államonként
               tabPanel ("Hisztogramok",
                         sidebarLayout(sidebarPanel (
                             selectInput (inputId = "gomb3",  
                                          label = "Állam",
                                          choices = unique(x$state_abbreviation))),
                             mainPanel(plotOutput ("histogram"),
                             ))),
               
               # Alapvető mutatók lehívása
               tabPanel("Leíró statisztika",
                        sidebarLayout(sidebarPanel (
                            radioButtons (inputId = "gomb4",
                                          label = "Ismérvek" ,
                                          choices = c ("Fehérek",
                                                       "Feketék",
                                                       "Latinók",
                                                       "Jövedelmi szint",
                                                       "Főiskolát végzettek",
                                                       "65 éven felüliek",
                                                       "Ingatlanok átlagos értéke",
                                                       "Evangélikusok",
                                                       "Katolikusok",
                                                       "Mormonok"))),
                            mainPanel(verbatimTextOutput ("leiro")))),
               
               # Korrelációs mátrix
               tabPanel("Korrelogram", plotOutput ("korrelogram"))
               
    ))

# Szerveroldal
server <- function(input, output, session) {
    # Térképhez reaktiválás
    subdata <- reactive({
        valt <- switch (input$gomb,
                        "Százalékpontos különbség" = x$per_point_diff_2016,
                        "Feketék" = x$Black,
                        "Latinók" = x$Hispanic,
                        "Népsűrűség" = x$Density,
                        "Jövedelmi szint" = x$Income,
                        "Katolikusok" = x$CATHRATE
        )
    })
    # Pontdiagramhoz reaktiválás
    subdata2 <- reactive({
        valt <- switch (input$gomb2,
                        "Fehérek" = x$White,
                        "Feketék" = x$Black,
                        "Latinók" = x$Hispanic,
                        "Jövedelmi szint" = x$Income,
                        "Főiskolát végzettek" = x$Edu_batchelors,
                        "65 éven felüliek" = x$age65plus,
                        "Ingatlanok átlagos értéke" = x$HSG495213,
                        "Evangélikusok" = x$EVANRATE,
                        "Katolikusok" = x$CATHRATE,
                        "Mormonok" = x$LDSRATE)
    })
    
    # Hipotézisvizsgálathoz reaktiválás
    subdata4 <- reactive({
        valt <- switch (input$gomb4,
                        "Fehérek" = x$White,
                        "Feketék" = x$Black,
                        "Latinók" = x$Hispanic,
                        "Jövedelmi szint" = x$Income,
                        "Főiskolát végzettek" = x$Edu_batchelors,
                        "65 éven felüliek" = x$age65plus,
                        "Ingatlanok átlagos értéke" = x$HSG495213,
                        "Evangélikusok" = x$EVANRATE,
                        "Katolikusok" = x$CATHRATE,
                        "Mormonok" = x$LDSRATE)
    })
    
    # Változók átnevezése
    x2 <- rename(x, "Százalékpontos különbség" = per_point_diff_2016,
                 "Feketék" = Black,
                 "Latinók" = Hispanic,
                 "Jövedelmi szint" = Income,
                 "Főiskolát végzettek" = Edu_batchelors,
                 "Evangélikusok" = EVANRATE,
                 "Katolikusok" = CATHRATE,
                 "Mormonok" = LDSRATE)    
    
    subdata5 <- x2[,c("Százalékpontos különbség", "Feketék", "Latinók", "Jövedelmi szint",
                      "Főiskolát végzettek", "Evangélikusok", "Katolikusok", "Mormonok")]
    
    num_vált <- reactive({
        switch (input$numvált,
                "Fehérek" = x$White,
                "Feketék" = x$Black,
                "Latinók" = x$Hispanic,
                "Népsűrűség" = x$Density,
                "Jövedelmi szint"= x$Income,
                "Főiskolát végzettek" = x$Edu_batchelors,
                "65 éven felüliek" = x$age65plus,
                "Ingatlanok átlagos értéke" = x$HSG495213,
                "Evangélikusok" = x$EVANRATE,
                "Katolikusok" = x$CATHRATE,
                "Mormonok" = x$LDSRATE)})
    
    #Színpaletta megváltoztatása, hogy illeszkedjen a hagyományos republikánus-demokrata színekhez
    szin <- colorRampPalette(c("red", "white", "blue"))
    szinek <- szin(8)
    paletta <- colorNumeric(szinek, x$valt)
    
    # Térkép printelése
    output$map <- renderLeaflet({
        leaflet()%>%
            #addProviderTiles("OpenStreetMap.Mapnik")%>%
            setView(lat = 39.8283, lng = -98.5795, zoom = 4) %>% 
            addPolygons(
                data = x$geometry,
                stroke = T,
                weight = 1,
                color = "black",
                fillColor = paletta(subdata()),
                smoothFactor = 0.2,
                fillOpacity = 1,
                popup = paste(" Megye: ", x$county_name,
                              "<br>", "Republikánus: ", x$votes_gop_2016, 
                              "<br>", "Demokrata: ", x$votes_dem_2016,
                              "<br>", "Fehérek: ", x$White,
                              "<br>", "Feketék: ", x$Black,
                              "<br>", "Latinók: ", x$Hispanic,
                              "<br>", "Jövedelmi szint: ", x$Income,
                              "<br>", "Százalékpontos különbség", 100*round(x$per_point_diff_2016, 4),
                              "<br>", "Evangélikusok: ", round(x$EVANRATE,2),
                              "<br>", "Katolikusok: ", round(x$CATHRATE, 2),
                              "<br>", "Mormonok: ", round(x$LDSRATE,2))
            )
    })
    
    # Pontdiagram printelése. Dinamikus a cím
    output$scatter <- renderPlot ({
        
        ggplot(x, aes(subdata2(), per_point_diff_2016)) +
            geom_smooth(method = "lm", se = F) + geom_hline(yintercept = 0, color = "green2") +
            geom_point () +
            theme_minimal() + ggtitle("Szignif") + xlab(input$gomb2) + ylab("Százalékpontos különbség") +
            ggtitle(paste("A két jelölt közötti százalékos különbség és a(z)", input$gomb2, "közötti összefüggés")) +
            theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = scales::percent)
        
    })
    
    # Vegyes kapcsolat printelése.
    output$kruskal <- renderPrint ({
        
        kruskal.test(Trump_győzelem ~ num_vált(), data=x)
        
    })
    
    # Boxplot printelése. A cím most még inkább dinamikus, mivel attól függően, hogy el lett-e fogadva a Kruskal-Wallis-teszt, más fog szerepelni a címben
    output$ábra<-renderPlot({
        
        if(kruskal.test(Trump_győzelem~num_vált(), data = x)[["p.value"]]<0.05){
            ggplot(x, aes(x = num_vált(), y = Trump_győzelem)) + geom_boxplot() + coord_flip() + xlab(input$numvált) +
                ylab("Győzött-e Trump?") + theme_minimal() + ggtitle(paste("A Trump-győzelem összefügg a(z)", input$numvált, "változóval")) + theme(plot.title = element_text(hjust = 0.5))
        } else{
            ggplot(x, aes(x = num_vált(), y = Trump_győzelem)) + geom_boxplot() + coord_flip() + xlab(input$numvált) +
                ylab("Győzött-e Trump?") + theme_minimal() + ggtitle(paste("A Trump-győzelem nem függ össze a(z)", input$numvált, "változóval")) + theme(plot.title = element_text(hjust = 0.5))
        }
        
        
    })
    
    # Hisztogram printelése, szintén dinamikus címmel
    output$histogram <- renderPlot ({
        
        #Adatsor átalakítása
        x <- x[x$state_abbreviation == input$gomb3,]
        
        # Alapvetően az Income változó numerikus, így kategóriákra osztom, hogy hisztogramon szépen ábrázolható legyen az adatsor
        x$bardata <- 0 
        x$Income_month <- x$Income/12
        x$bardata[x$Income_month < 1201] <- "0-1200 $ / hó"
        x$bardata[x$Income_month > 1200 & x$Income_month < 1601] <- "1200-1600 $ / hó"  
        x$bardata[x$Income_month > 1600 & x$Income_month < 2001] <- "1600-2000 $ / hó"
        x$bardata[x$Income_month > 2000 & x$Income_month < 2401] <- "2000-2400 $ / hó"
        x$bardata[x$Income_month > 2400 & x$Income_month < 2801] <- "2400-2800 $ / hó"
        x$bardata[x$Income_month > 2800 & x$Income_month < 3201] <- "2800-3200 $ / hó"
        x$bardata[x$Income_month > 3200] <- "3200- $ / hó"
        x$population2014_scaled <- x$population2014/ 1000000
        pop_barplot <- aggregate(x$population2014_scaled, by = list(x$bardata), sum)
        x$diff_2016 <- x$votes_dem_2016 - x$votes_gop_2016
        pop_barplot2 <- aggregate(x$diff_2016, by = list(x$bardata), sum)
        pop_barplot3 <- aggregate(x$total_votes_2016, by = list(x$bardata), sum)
        pop_barplot4 <- as.data.frame(pop_barplot2$x / pop_barplot3$x)
        pop_barplot$per_point_diff_2016 <- 100 * pop_barplot4[,1]
        colnames(pop_barplot) <- c("Income", "Population", "Difference")
        
        # Kirajzolás (külön színskálával)
        ggplot(data = pop_barplot) + geom_col(aes(x = factor(Income), y = Population, fill = Difference)) +
            theme_minimal() + scale_fill_gradient2(low = "red", mid = "white", high = "blue", space = "Lab") +
            xlab("Income") + labs(colour= "Clinton és Trump közötti differencia (%)") +
            ggtitle(paste("A népesség, politikai beállítottság jövedelmi csoportok szerint", input$gomb3, "államban")) +
            theme(plot.title = element_text(hjust = 0.5)) + labs(fill = "Clinton-Trump különbség (%)") +
            xlab("Jövedelmi szint") + ylab("Népesség (millió fő)")
    })
    # Leíró statisztika
    output$leiro <- renderPrint ({
        
        psych::describe (subdata4())
    })
    
    # Korrelációs mátrix megjelenítése
    output$korrelogram <- renderPlot (
        ggcorrplot::ggcorrplot (
            correlations <- cor(subdata5),
            show.diag = F,
            lab = T,
            type = "lower",
            digits = 2
        ))
}

shinyApp(ui = ui, server = server)