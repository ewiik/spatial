## grab weather stations closest to sites in BC by lat long match
## the weather station data have all been sourced from 
##    http://tools.pacificclimate.org/dataportal/pcds/map/
##    https://www.pacificclimate.org/data/bc-station-data-disclaimer
## FIXME: worth just using same package as Heather and copy pasting the code from SK?

## load necessary packages
library("sp")

## get necessary data sets
statmeta <- read.csv("../data/pcds_data/crmp_network_geoserver.csv")
statsub <- subset(statmeta, select = c(network_name, native_id, station_name, lon, lat, elev))

if (!file.exists("../data/private/piscesallorgh.csv")) {
  stop("Get piscesallorgh.csv from emma")
}
pisces <- read.csv("../data/private/piscesallorgh.csv") 

if (!file.exists("../data/googlecoords.csv")) {
  source("../scripts/co2flux_spatial.R")
}
googlecoords <- read.csv("../data/googlecoords.csv") 
## FIXME: these lat longs still not plotting in the exact location of the lake but close 
##    enough for now in terms of checking how to match the right station

## look at distances between our data
## Sim up two sets of 100 points, we'll call them set a and set b:
a <- SpatialPoints(coords = data.frame(x = googlecoords$longdecim, y = googlecoords$latdecim), 
                   proj4string=CRS("+proj=longlat +datum=WGS84"))
b <- SpatialPoints(coords = data.frame(x = statsub$lon, y = statsub$lat), 
                                       proj4string=CRS("+proj=longlat +datum=WGS84"))

## Find the distance from each point in a to each point in b, store
##    the results in a matrix.
results <- spDists(a, b, longlat=T)

# or check out package pdist?
