# Clean and Drop function for Landsat 7 data
# 
# This function masks cloud in combination with the cloud shadow and has an optio to fill the missing data from 
# the broken scanline corrector on Landsat 7. After masking this function drops the mask layers
#
# Necessary inputs:
# - dataset: A rasterstack containing all the Landsat 7 files called
# - x: Cloud mask layer (not Nullable)
# - y: Fill mask layer (optional)
#
# Returns:
#  - StudyAreaClean: a RasterBrick containing the masked bands



CleanAndDrop <- function(dataset, x, y) {
  if(missing(y)) {
    clouds <- dataset[[x]]
    cloudshadows <- dataset[[10]]
      CloudMask <- merge(clouds, cloudshadows)
        StudyAreaClean <- dropLayer(dataset, c(1, 2, 9:14))
        StudyAreaClean[CloudMask == 255] <- NA
          return (StudyAreaClean)
  } else {
    fill <- dataset[[y]]
      CloudMask <- merge(dataset[[9]], dataset[[10]], fill)
        StudyAreaClean <- dropLayer(dataset, c(1, 2, 9:14))
        StudyAreaClean[CloudMask == 255] <- NA
          return (StudyAreaClean)
  }
}

