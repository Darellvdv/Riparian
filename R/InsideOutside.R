# InsideOutside function
# 
# Here the user can input a file with lat long coordinates of possible locations for cutting trees. The return is an interactive map
# which shows the protection buffer and the points the user added. Also the user will see which points are suitable for cutting trees and 
# which are protected.
#
# Necessary inputs:
# - data: a .csv with long lat coordinates
#
# Returns:
#  - Interactivemap: a Leaflet containing the buffer and the input points that show which points are inside the buffer and which aren't.



InsideOutside <- function(data){
  # Make dataframe spatial
  xy <- Coordinates[,c(1,2)]
  LocationsSpatial <- SpatialPointsDataFrame(coords = xy, data = data, 
                                             proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  # Reproject points to UTM for intersection with buffer                                          
  lcwarp <- spTransform(LocationsSpatial, CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  # Intersect points with buffer
  PointsinBuffer <- gIntersection(lcwarp, polybuf)
  
  # Reproject points in buffer to Lat Long for adding to Leaflet viewer
  PointsbufferWarp <-suppressWarnings(spTransform(PointsinBuffer, CRS("+proj=longlat +datum=WGS84")))
  PointsinBufferDF <- suppressWarnings(as.data.frame(PointsbufferWarp))
  
  # Create icon for Leaflet map
  stopIcon <- makeIcon(
    iconUrl = "http://2.bp.blogspot.com/-HGSk_RX3ObI/UuozFLl4QWI/AAAAAAAAJGk/VklvrAP2e3g/s1600/Dialog-stop_icon.png",
    iconWidth = 40, iconHeight = 40,
    iconAnchorX = 19, iconAnchorY = 40
  )
  
  # Create Leaflet interactive map
  InteractiveMap <- leaflet() %>% addTiles () %>% addRasterImage(riverbuf, opacity = 0.4, colors = "Orange") %>% 
    addMarkers(lng = Coordinates[,1], lat = Coordinates[,2]) %>%
    addMarkers(lng = PointsinBufferDF[,1], lat = PointsinBufferDF[,2], icon = stopIcon)
  
  return(InteractiveMap)
}