## Read in and annotate spatial data set; from Cumming1995
## the spreadsheet we have is for late summer. but could also redo analysis for spring using
##    Cumming1995

## units: nutrients; elements; DIC + DOC: mg/L; conductivity uS (assuming cm-1); note that 
##    specific conductance is reported in Cumming1995, and we are using conductivity;
##    alt m; 

## load necessary packages
library("sp")
library("rgdal")

## read in data; the csv is my revamp of the xls kerri sent; it now has lat longs and altitude
##    of each site
if (!file.exists("../data/piscesallorgh.csv")) {
  stop("Get piscesallorgh.csv from emma")
}
pisces <- read.csv("../data/piscesallorgh.csv") 

## coordinates currently as lat longs in separate columns. need to make into decimal degrees
lats <- data.frame(latdegmin = paste0(pisces$latdeg,"d", sprintf("%.1f", pisces$latmin), '\\ "N"'))

# coerce all instances of even number minutes to have trailing zeroes with sprintf
longs <- data.frame(longdegmin = paste0(pisces$longdeg, "d", sprintf("%.1f", pisces$longmin), "W"))

latsdec <- within(lats, { # see https://stat.ethz.ch/pipermail/r-help/2010-August/249374.html
  latdegmins <- do.call(rbind, strsplit(as.character(latdegmin), ".", fixed = TRUE))
  latdec <- as.numeric(latdegmins[,1]) + 
    (as.numeric(latdegmins[,2]) + as.numeric(latdegmins[,3])/60)/60
  rm(latdegmins)
  })

longsdec <- within(longs, { # see https://stat.ethz.ch/pipermail/r-help/2010-August/249374.html
  longdegmins <- do.call(rbind, strsplit(as.character(longdegmin), ".", fixed = TRUE))
  longdec <- abs(as.numeric(longdegmins[,1])) + 
    (as.numeric(longdegmins[,2]) + as.numeric(longdegmins[,3])/60)/60
  longdec = -longdec
  rm(longdegmins)
})

googlecoords <- data.frame(LAKE = pisces$LAKE, lakename = pisces$lakename, 
                           latdegminsec = lats$latdegmin, longdegminsec = longs$longdegmin, 
                          latdecim = latsdec$latdec, longdecim = longsdec$longdec)

testing <- cbind(as.character(googlecoords$latdegminsec[1:3]), 
                 as.character(googlecoords$longdegminsec[1:3]))
testing[,2] <- gsub("-", "", testing[,2])
testing[,1] <- paste(testing[,1], "N")
testing[,2] <- paste(testing[,2], "E")
char2dms(testing, chd = ".", chm = ".", chs = "")

write.csv(googlecoords, "data/googlecoords.csv")

coordinates(googlecoords) <- c("longdecim", "latdecim") 
proj4string(googlecoords) <- CRS("+proj=longlat +datum=WGS84")
#then coordinates(dfProj) will give you back projected coordinates.
state.ll83 <- spTransform(states, CRS("+proj=longlat +ellps=GRS80"))
writeOGR(googlecoords, dsn = "data/testing.kml", layer = "wtf", driver = "KML")

head(pisces)
