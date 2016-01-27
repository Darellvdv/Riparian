# Normalized Vegetation Index Calculator function for Landsat 7 data
# 
# This function calculates normalized vegetation indices for Landsat 7 bands
# 
# Necessary inputs:
# - x: Band layer
# - y: Band layer
#
# Returns:
#  - ndvi: a raster containing the outcome of the calculation


ViCalc <- function(x, y) {
  ndvi <- (y - x) / (y + x)
  return(ndvi)
}
