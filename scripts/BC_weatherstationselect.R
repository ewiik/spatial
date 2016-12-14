## grab weather stations closest to sites in BC by lat long match
## the weather station data have all been sourced from 
##    http://tools.pacificclimate.org/dataportal/pcds/map/
##    https://www.pacificclimate.org/data/bc-station-data-disclaimer

## load necessary packages
library("sp")
library("rgdal")

## load open data from NaturalEarthData.com
world_shp = rgdal::readOGR("../data/ne_10m_land", layer = "ne_10m_land")
summary(world_shp)

lakes_shp = rgdal::readOGR("../data/ne_10m_lakes_north_america/", layer = "ne_10m_lakes_north_america")
summary(lakes_shp)

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

## find closest station using geosphere method
## grab long lats (in that order) for lakes and stations
decims <- grep("decim", names(googlecoords))
lakes_latlong <- as.matrix(googlecoords[,rev(decims)])

decims <- which(names(statsub) %in% c("lon", "lat"))
stat_latlong <- as.matrix(statsub[,decims])

distance <- distm(lakes_latlong, stat_latlong, fun =distCosine)

# rename the headers as the lakes
rownames(distance)<- googlecoords[,'lakename']
colnames(distance) <- statsub[,'station_name']

#finding the minimum distance between the lake and closest climate station
mindist <- do.call(rbind, as.list(apply(distance, 1, FUN = min)))
mindist <- as.data.frame(mindist)
names(mindist) <- 'closest'

distance <- as.data.frame(distance)
distance$closest <- mindist$closest

stationlist <- list()
for (i in 1:nrow(distance)) {
  stationlist[i] <- colnames(distance)[which(distance[i,-which(colnames(distance) == 'closest')] 
                                             %in% distance[i,'closest'])]
}

lakestations <- as.data.frame(cbind(rownames(distance), do.call(rbind, stationlist)))
colnames(lakestations) <- c("Lake", "Station")

## sub my stations data frame to the closest ones.
lakestats <- merge(data.frame(Station=lakestations$Station), statsub, by.x="Station", by.y="station_name")
## remove duplicates for plotting
lakestatsu <- unique(lakestats)

## try to plot all the stations and lakes on a map... note that @coords numeric depends on projection!!

a <- sp::SpatialPointsDataFrame(coords = cbind(googlecoords$longdecim, googlecoords$latdecim), 
                                data=googlecoords,
                                proj4string=CRS("+proj=longlat +datum=WGS84"))
b <- sp::SpatialPointsDataFrame(coords = cbind(lakestatsu$lon, lakestatsu$lat), data=lakestatsu,
                                proj4string=CRS("+proj=longlat +datum=WGS84"))
## derive map base for portrayal!
mbase <- as(raster::extent(a), "SpatialPolygons")
proj4string(mbase) <- sp::CRS(proj4string(world_shp))
#Clip the map
world_clip <- rgeos::gIntersection(world_shp, mbase, byid=T, drop_lower_td = T)
lake_clip <- rgeos::gIntersection(lakes_shp, mbase, byid=T, drop_lower_td = T)
## myproj
utmCRS <- sp::CRS("+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")  #zone 12 may do too

mbaseu <- sp::spTransform(world_clip, utmCRS)
lbaseu <- sp::spTransform(lake_clip, utmCRS)
## update a and b 
au <- sp::spTransform(a, utmCRS)
bu <- sp::spTransform(b, utmCRS)

plot(mbaseu)
points(au)
points(bu, pch=19)
lines(lbaseu)
