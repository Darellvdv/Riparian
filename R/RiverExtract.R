# Extract water pixels function
# 
# This function extracts water pixels from a NDVI raster based on a treshold value which is set by the user. 
# After extraction the small water areas (area size set by user) are removed using the areaSieve function
#
# Necessary inputs:
# - inputlayer: NDVI layer
# - tresholdmax: numeric value between -0.5 and 1
# - areatreshold: numeric value (1000 = 1ha)
#
# Returns:
#  - river: a raster containing the water pixels with all other pixels masked



RiverExtract <- function(inputlayer) {
  river <- inputlayer <= tresholdmax
  river[river == 0] <- NA
    river <- areaSieve(river, thresh = areatreshold, directions = 4)
      return(river)
}
