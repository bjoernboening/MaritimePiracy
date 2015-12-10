
#cities <- world.cities[order(world.cities$pop,decreasing=TRUE)[1:1000],]

Attacken <- read.csv("/Users/laurencehendry/GitHub/MaritimePiracyTennessee.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)
Attacken$longitude <- gsub(",", ".", Attacken$longitude)
Attacken$longitude <- gsub(",", ".", Attacken$longitude)
Attacken$latitude <- gsub(",", ".", Attacken$latitude)
Attacken$longitude <- as.numeric(Attacken$longitude)
Attacken$latitude <- as.numeric(Attacken$latitude)

Coordinates <- select(Attacken, 9 10) 
Coordinatzes <- data.frame(matrix(unlist(Coordinates), nrow=646, byrow=T)) # Convert to dataframe
Coordinates <- select(Coordinates, c(Longitude=X1, Latitude=X2, Height=X3)) # Rename

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





> bgcolor <- "#000025"
> earth <- tempfile(fileext=".jpg")
> jpeg(earth, width=2048, height=1024, quality=100, bg=bgcolor, antialias="default")
> par(mar = c(0,0,0,0), pin = c(4,2), pty = "m", xaxs = "i",
      +     xaxt = "n", xpd = FALSE, yaxs = "i", yaxt = "n")
> 
  > col <- rep("black",length(wrld_simpl$NAME))    # Set a default country color
  > col[wrld_simpl$NAME=="Brazil"] <- "#003344"    # Highlight Brazil
  > 
    > plot(wrld_simpl, col=col,          bg=bgcolor, border="#111111", ann=FALSE,  axes=FALSE, 
           +      xpd=FALSE,  xlim=c(-180,180), ylim=c(-90,90),  setParUsrBB=TRUE)
  > 
    > graphics.off()
  > 
    > globejs(img=earth, bg="black", lat=cities$lat,     long=cities$long, value=value, 
              +         rotationlat=-0.34,     rotationlong=-0.38, fov=30)
  > View(Attacken)
  > summary(Attacken$latitude)
  Length     Class      Mode 
  6343 character character 
  > Attacken$latitude <- gsub(',', '.', Attacken$latitude)
  > Attacken$latitude <- as.numberic(gsub(',', '.', Attacken$latitude))
  Error: could not find function "as.numberic"
  > Attacken$latitude <- as.numeric(gsub(',', '.', Attacken$latitude))
  > Attacken$longitude <- as.numeric(gsub(',', '.', Attacken$longitude))
  > globejs(img=system.file("image/world.jpg", package = "threejs"), Attacken$latitude, Attacken$longitude, value=40, color=40,color="00ffff",arcs,arcsColor="#99aaff",
            + arcsHeight = 0.4, arcsLwd = 1, arcsOpacity = 0.2, atmosphere = FALSE, height = NULL, width = NULL, ...  )
  Error: '...' used in an incorrect context
  > globejs(img=system.file("image/world.jpg", package = "threejs"), Attacken$latitude, Attacken$longitude, value=40, color=40,color="00ffff",arcs,arcsColor="#99aaff",
            + arcsHeight = 0.4, arcsLwd = 1, arcsOpacity = 0.2, atmosphere = FALSE, height = NULL, width = NULL)
  Error in globejs(img = system.file("image/world.jpg", package = "threejs"),  : 
                     formal argument "color" matched by multiple actual arguments
                   > globejs(img=system.file("image/world.jpg", package = "threejs"), Attacken$latitude, Attacken$longitude
                             + )
                   Error in paste(prefix, if (is.null(encoding)) .Call(C_URIencode, data,  : 
                                                                         RAW() can only be applied to a 'raw', not a 'character'
                                                                       In addition: Warning message:
                                                                         In file(file, "rb") :
                                                                         file("") only supports open = "w+" and open = "w+b": using the former
                                                                       > data(wrld_simpl, package="maptools")
                                                                       > data(world.cities, package="maps")
                                                                       > cities <- world.cities[order(world.cities$pop,decreasing=TRUE)[1:1000],]
                                                                       > View(cities)
                                                                       > globejs(img=earth, bg="black", lat=cities$lat,     long=cities$long, value=value,
                                                                                 + rotationlat=-0.34,     rotationlong=-0.38, fov=30)
                                                                       > globejs(img=earth, bg="black", lat = Attacken$latitude, long = Attacken$longitude, value=value,
                                                                                 + rotationlat=-0.34,     rotationlong=-0.38, fov=30)
                                                                       > View(Attacken)
                                                                       > 