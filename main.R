
#                                                                                          
# TTTTTTT                            TTTTTTT                       iii                lll  
#   TTT     eee    aa aa mm mm mmmm    TTT   rr rr   oooo  pp pp         cccc   aa aa lll  
#   TTT   ee   e  aa aaa mmm  mm  mm   TTT   rrr  r oo  oo ppp  pp iii cc      aa aaa lll  
#   TTT   eeeee  aa  aaa mmm  mm  mm   TTT   rr     oo  oo pppppp  iii cc     aa  aaa lll  
#   TTT    eeeee  aaa aa mmm  mm  mm   TTT   rr      oooo  pp      iii  ccccc  aaa aa lll  
#                                                          pp                                                  

# 27-01-2016
# Project for WUR geoscripting course
# TeamTropical ~ Darell.vanderVoort & Froede.Vrolijk

#-------------------------------------------------------------------------------------------------------------------#
#                                                                                                                   #
# Tool for analyzing Landsat 7 imagery developed for generating riparian buffers in tropical regions to protect     #
# vulnarable ecological areas.                                                                                      #
#                                                                                                                   #
# Mask clouds, extract rivers, generates riparian buffer zones and applies BfastMonitor to monitor deforestation    #
# in these zones. Also returns interactive maps for specific harvesting locations.                                  #
#                                                                                                                   #
# This script is made for higher level data products from Landsat 7 and downloaded from the Earth Explorer portal.  #
# Download should include:                                                                                          #
#   - Surface Refelctance                                                                                           #
#   - Cloud Mask                                                                                                    #
#                                                                                                                   #
#-------------------------------------------------------------------------------------------------------------------#


# Load Libraries
packages <- c('sp', 'rgdal', 'rgeos', 'raster', 'bfast', 'bfastSpatial', 'igraph', 'magrittr', 'leaflet')
lapply(packages, require, character.only = TRUE)


# Load functions
functions <- c("R/CleanAndDrop.R", "R/ViCalc.R",  "R/RiverExtract.R", "R/InsideOutside.R")
lapply(functions, source)


source("R/CleanAndDrop.R")
source("R/ViCalc.R")
source("R/RiverExtract.R")
source("R/InsideOutside.R")


# Script Parameters:

tresholdmax = 0.2 # Define the treshold for extracting water pixels from NDVI. To see histogram to set correct treshold value go to line 62
areatreshold = 12000 # Define the river areas to be excluded. 10000 = 1ha
BufferWidth = 600 # Define the bufferwidth around river in meters
WorkingDirectory = "/home/darell/Documents/Rprojects/Riparian/" # Set your working directory
  
#-------------------------------------------------------------------------------------------------------------------#

## START OF SCRIPT ##

# Set working directory
setwd(WorkingDirectory)

# List files
files <- list.files(path= "Data/ExampleData/", pattern = ".tif", full.names = TRUE)

# Make a rasterstack of a layers
StudyArea <- stack(files)

#----------------------------------------------------------------------#

# Pre-procesing of example dataset (cloudmask)
names(StudyArea)
StudyAreaCleaned <- CleanAndDrop(StudyArea, 9, 12) # Select 9 as first argument for cloud and shadow mask, second argument 12 as optionall fill mask

#----------------------------------------------------------------------#

# Calculate NDVI
ndvi <- overlay(x = StudyAreaCleaned[[3]], y = StudyAreaCleaned[[4]], fun = ViCalc)
names(ndvi) <- "NDVI"

# Inspect NDVI histoggram to determine treshold value for water pixels
par(mfrow=c(1,2)) 
plot(ndvi, main = "Map")
hist(ndvi, main = "NDVI frequency")

#----------------------------------------------------------------------#

# Extract the river from NDVI layer
river <- RiverExtract(ndvi)

# Plot extracted water pixels for inspection
par(mfrow=c(1,1)) 
objectsLegend1 <- c("Water Pixels")
plot(river, col = 'Blue', legend=FALSE, main = 'Plot of extracted water pixels') 
# insert custom legend
legend("bottomright", legend=objectsLegend1, cex=0.9, fill='Blue', ncol=1)

#----------------------------------------------------------------------#

# Create variable buffer around river pixels
riverbuf <- buffer(river, width = BufferWidth, doEdge = TRUE, dissolve = TRUE)

# Transform buffer to polygon for further analysis to see intersections and mask NDVI layers for Bfast
polybuf <- rasterToPolygons(riverbuf, na.rm = TRUE, dissolve = TRUE)

# Visualize the buffer
objectsLegend2 <- c("River", "Buffer")
cols <- c("Blue", "Orange")
plot(riverbuf, col=cols[2], legend=FALSE, main = 'Plot of river with buffer')
plot(river, col=cols[1], legend=FALSE, add = TRUE)
# insert custom legend
legend("bottomright", legend=objectsLegend2, cex=0.9, fill=cols, ncol=2)

#----------------------------------------------------------------------#

# Inside Outside: see if input coordinates are inside or outside the buffer and generate an interactive map

# Load CSV file with coordinates
Coordinates <- read.table("./Data/Coordinates.csv", header = TRUE, sep = ",")

# Apply function and load map
InsideOutside(Coordinates)

#-------------------------------------------------------------------------------------------------------------------------------#
#                                                            !!!                                                                #
# From now on we will use Bfast Spatial to load in a time series of Landsat 7 from the same area conisting out of 31 batches.   # 
# This dataset is to large for GitHub. If you want to test this part of the script with the buffer from the example dataset,    #
# download Landsat 7 path:9 row:63. A sufficient amount of images are needed for Bfast to work.                                 #
#                                                            !!!                                                                #
#-------------------------------------------------------------------------------------------------------------------------------#

## Create folders and load data ##

# Create a tempororay folder for output files
outputdir <- dirout <- file.path(dirname(rasterTmpFile()), 'bfmspatial_2')
dir.create(dirout, showWarning=FALSE)

# Create output folders for processing
Extentdir <- diroutExtent <- file.path('./Data/Extent/')
NDVIdir <- diroutNDVI <- file.path('./Data/NDVI/')
diroutStack <- 'stackNDVI'
dir.create(diroutExtent, showWarning=FALSE)
dir.create(diroutNDVI, showWarning=FALSE)

dataDir <- './Data/Landsat Data/'

# Process batch Landsat images
processLandsatBatch(x=dataDir, pattern=glob2rx('*.tar.gz'), outdir=diroutNDVI, srdir=NDVIdir,
                    delete=TRUE, vi='ndvi', mask='fmask', keep=0, overwrite=TRUE,  e=ndvi)

#--------------------------------------------------------------------------------------------#

# Mask ndvi layers to buffer area

# list ndvi data and file names for masking
ndvifiles <- list.files(path= "./Data/NDVI/", pattern = ".grd", full.names = TRUE)
namesndvi <- list.files(path= "./Data/NDVI/", pattern = ".grd", full.names = FALSE)

# Create a data frame with column names of original filenames
col.names = c(namesndvi)
df <- read.table(text = "", col.names = col.names)

# Loop trough the ndvi files, load them as rasters, apply mask and write with original filename
dir.create('./Data/NDVI/mask')
i <- 1
for (file in ndvifiles) {
  ndviraster <- raster(file) # load as rasters
  # apply mask
  out <- mask(ndviraster, polybuf)
  out[out < 3000] <- NA # remove river pixels from buffer
  writeRaster(out, file.path('./Data/NDVI/mask/', names(df[i])), format = 'raster')
  i <- i + 1
}

#---------------------------------------------------------------------------------------------#

# Create a new subdirectory in the temporary directory for stack
dirout <- file.path(dirname(rasterTmpFile()), 'stack')
dir.create(dirout, showWarnings=FALSE)

# Generate a file name for the output stack
stackName <- file.path(dirout, 'stackRiverbuf.grd')

# Stack the layers
riverbufStack <- timeStack(x= './Data/NDVI/mask/', pattern = glob2rx('*.grd'), 
                           filename=stackName, datatype='INT2S', overwrite=TRUE)


# Count the amount of available pixels troughout the time series and visualize
obsRiverbuf <- countObs(riverbufStack)
plot(obsRiverbuf, main = 'Count of the amount of available pixels')

# Calculate mean and visualize
meanNDVI <- summaryBrick(riverbufStack, fun=mean, na.rm=TRUE)
plot(meanNDVI, main = 'Map of the mean NDVI 2004 - 2015')

#-------------------------------------------------------------------------------------------#

## BFAST MONITORING ##


# Run BfastSpatial and starting monitoring period in 2014
bfm2014 <- bfmSpatial(riverbufStack, start=c(2014, 1), order=3, formula = response ~ trend)

names(bfm2014) <- c('Breakpoint timing', 'Change magnitude', 'Error flag')

# plot the result for inspection
plot(bfm2014)

# Extract change raster
ChangeRaster <- raster(bfm2014, 1)

# Plot the change per month for the last monitoring year
months <- changeMonth(ChangeRaster)
# set up labels and colourmap for months
monthlabs <- c("jan", "feb", "mar", "apr", "may", "jun", 
               "jul", "aug", "sep", "oct", "nov", "dec")
cols <- rainbow(12)
plot(months, col=cols, breaks=c(1:12), legend=FALSE, main = "Change per month for 2015")
# insert custom legend
legend("bottomright", legend=monthlabs, cex=0.7, fill=cols, ncol=2)


# extract magn raster
MagnRaster <- raster(bfm2014, 2) / 10000

# make a version showing only breakpoing pixels
magn_bkp <- MagnRaster
magn_bkp[is.na(ChangeRaster)] <- NA
op <- par(mfrow=c(1, 2))
plot(magn_bkp, main="Magnitude: breakpoints")
plot(MagnRaster, main="Magnitude: all pixels")

#-------------------------------------------------------------------------------------------#

# Inspect possible deforestation areas manually

# plot the 2nd layer (or a layer without much clouds)
par(mfrow=c(1, 1))
plot(riverbufStack, 2)
# run bfmPixel() in interactive mode with a monitoring period 
bfm <- bfmPixel(riverbufStack, start=c(2014, 1), interactive=TRUE, formula = response ~ trend)
# choose the pixel whose time series you want to see by clicking on the map you plotted a moment ago

plot(bfm$bfm)
