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
Attacken <- read.csv("E:/bjoer/Documents/GitHub/MaritimePiracy/MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
Attacken$longitude <- gsub(",", ".", Attacken$longitude)
Attacken$latitude <- gsub(",", ".", Attacken$latitude)
Attacken$longitude <- as.numeric(Attacken$longitude)
Attacken$latitude <- as.numeric(Attacken$latitude)

Coordinates <- select(Attacken, 9, 10) 
Coordinates <- data.frame(matrix(unlist(Coordinates), nrow=6434, byrow=T)) # Convert to dataframe
Coordinates <- select(Coordinates, c(Longitude=X1, Latitude=X2, Height=X3)) # Rename

###Interactive Test
outline <- Attacken[Attacken(Attacken$longitude, Attacken$latitude),]

data(Coordinates)

# Show first 20 rows from the `quakes` dataset
leaflet(data = Coordinates[1:20,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag))

map <- leaflet(quakes) %>%
  # Base groups
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("Stamen.Toner", group = "Toner") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  # Overlay groups
  addCircles(~long, ~lat, ~10^mag/5, stroke = F, group = "Quakes") %>%
  addPolygons(data = outline, lng = ~long, lat = ~lat,
              fill = F, weight = 2, color = "#FFFFCC", group = "Outline") %>%
  # Layers control
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("Quakes", "Outline"),
    options = layersControlOptions(collapsed = FALSE)
  )
map
###End of Interactive Test
### Set color (for dots?)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p()
)

server <- function(input, output, session) {
  # for background calculation
  # data points
  points <- eventReactive(input$recalc, {
    cbind(Attacken$longitude, Attacken$latitude)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      
      addMarkers(data=points())
  })
}

shinyApp(ui, server)