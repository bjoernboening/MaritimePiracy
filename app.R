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
Attacken <- read.csv("E:/bjoer/Documents/Google Drive/Universität/Hertie/03_Fall 2015/05_Master Thesis/00_Piracy_2015-16/03_Data/Tennessee/MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
Attacken$longitude <- gsub(",", ".", Attacken$longitude)
Attacken$latitude <- gsub(",", ".", Attacken$latitude)
Attacken$longitude <- as.numeric(Attacken$longitude)
Attacken$latitude <- as.numeric(Attacken$latitude)

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