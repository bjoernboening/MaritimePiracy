### Please cite Christopher Cosler and the https://github.com/ChristopherCosler/ShinyMap/ repo if you use this code.

### Load libraries
library(shiny)
library(leaflet)
library(httr)
library(dplyr)
library(XML)
library(maptools)
library(sp)
library(rgdal)
library(gsubfn)

### Prepare Coordinates
Attacken <- read.csv("E:/bjoer/Documents/Google Drive/Universität/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Tennessee/MaritimePiracyTennessee.csv", header = TRUE, sep = ";")
Attacken$Longitude <- as.numeric(Attacken$longitude)
Attacken$Latitude <- as.numeric(Attacken$latitude)
Coordinates <- select(Attacken, 9 10) 
Coordinates <- data.frame(matrix(unlist(Coordinates), nrow=646, byrow=T)) # Convert to dataframe
Coordinates <- select(Coordinates, c(Longitude=X1, Latitude=X2, Height=X3)) # Rename


Initiativen <- GET("https://mapsengine.google.com/map/kml?mid=zc6TdvfelKuY.kUvriXoSREXw&forcekml=1") # Download data
Initiativen <- as.character(Initiativen) # Concert to character
Names <- strapplyc(Initiativen, "<name>(.*?)</name>", simplify = c) # Extract names
Coordinates <- strapplyc(Initiativen, "<coordinates>(.*?)</coordinates>", simplify = c) # Extract coordinates
Coordinates <- strsplit(Coordinates, split = ",") # Split into two rows
Coordinates <- data.frame(matrix(unlist(Coordinates), nrow=646, byrow=T)) # Convert to dataframe
Coordinates <- select(Coordinates, c(Longitude=X1, Latitude=X2, Height=X3)) # Rename
Coordinates
Coordinates$Longitude <- as.numeric(as.character(Coordinates$Longitude))
Coordinates$Latitude <- as.numeric(as.character(Coordinates$Latitude))
data <- Coordinates

### Prepare Shapefiles
Shapes_krs <- readOGR(dsn = "./Data/Shapefiles", layer = "vg2500_krs")
Shapes_krs<-spTransform(Shapes_krs, CRS("+init=epsg:4326"))
Shapes_krs$test <- rep(1:4, len = 402)

### Initiatives per district
Coordinates_transformed <- Coordinates[c("Longitude", "Latitude")] # Make new df
coordinates(Coordinates_transformed) <- ~Longitude+Latitude # Convert to spatial object
proj4string(Coordinates_transformed) <- proj4string(Shapes_krs) # Nicht ganz klar was das macht, im Internet steht assign CSR coordinates
over(Coordinates_transformed, Shapes_krs)$GEN # In welchem Kreis liegt es?
Coordinates <- cbind.data.frame(Coordinates_transformed, Kreis=over(Coordinates_transformed, Shapes_krs)$GEN) # Cbind zum dataframe
table(Coordinates$Kreis)
temp <- data.frame(Shapes_krs$GEN) # Create dataframe with one row per district
temp <- rename(temp, Kreis= Shapes_krs.GEN) # Rename
Initiatives <- Coordinates %>% count(Kreis) # Calculate number of initiatives per district
Initiatives <- rename(Initiatives, Initiatives = n) # Rename
temp <- full_join(temp, Initiatives) # Join to data
temp$Initiatives[is.na(temp$Initiatives)] <- 0 # Replace NA with 0

Shapes_krs$Initiatives <- temp$Initiatives




### Set color (for dots?)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  selectInput("data", "Data per district:",
              c("Refugee initiatives" = "Initiatives"))
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(data$Longitude, data$Latitude)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data=Shapes_krs, weight=2, fillOpacity = 0.8, 
                  smoothFactor = 0.5, 
                  color = ~colorBin("YlOrBr", bins = c(0, 1, 2, 3,4, 10, 40), Shapes_krs$Initiatives)(Initiatives)) %>%
      addMarkers(data=points())
  })
}

shinyApp(ui, server)