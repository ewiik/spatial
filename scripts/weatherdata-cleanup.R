## script that computes nearest neighbours for SK sites and weather stations and tidies up weather
##    data
## data files in /fromheather/ received by email October 17, 2016 
## the code here is modified from Heather's script massBalanceSkils.R

## read files  
location <- read.csv("../data/private/fromheather/climateStations.csv", stringsAsFactors = FALSE, 
                     fileEncoding = "latin1") 
climateDaily <-readRDS("../data/private/fromheather/daily-data-skils.rds")
climateHourly <- readRDS("../data/private/fromheather/hourly-data-skils-cleaned.rds")

sksites <- readRDS("~/git/flux/data/private/heathergasfluxsupp.rds")
skflux <- read.csv("~/git/flux/data/private/co2fluxheathersites.csv")

## clean up data
## ====================================================================================================
## use only locations that have both hourly and daily data
location <- subset(location, hourly == "Y" & daily == "Y")

## really ittitating issue where some towns have renamed station 3+ times.  For Yorkton this is becoming 
##    an issue.  
## One site Station ID = 48977 it does not have the full set of data so if a lake gets paired with it 
##    then it comes up with NA.  To fix that I am reassigning its station ID as the same number as 
##    the other yorkton station ID 49808
location$stationID[location$stationID == "48977"]<-"49808"

#remove the stations with no data
location <- location[! location$stationID %in% c(50977,50091,2956,2967,3300,46609,2930,3186,3261,52458, 
                                                 51798, 2981,2933,6867,2933,2890,3157,3151,3071,3195,
                                                 2947,3448,3449,3446,2984,3270,45827,2933,2859,2980,
                                                 2875,2947),]

# There are some stations that have different stationIDs but are the same location.
# for those stations I need to combine the records using mean because if the site
# had overlap we do no want to add the temp and precip together

#fixing medow lake make all station ID for meadow lake 10198
climateDaily$StationID[climateDaily$StationID == "52000"]<-"10198"
climateDaily$StationID[climateDaily$StationID == "3386"]<-"10198"
climateDaily$StationID[climateDaily$StationID == "53021"]<-"10198"

#fixing Moose Jaw which has two records making both 27476
climateDaily$StationID[climateDaily$StationID == "46067"]<-"27476"

#fixing Swift Current which has two records making both 48976
climateDaily$StationID[climateDaily$StationID == "3157"]<-"48976"
#fixing Nipiwan which has two records making both 10195
climateDaily$StationID[climateDaily$StationID == "48608"]<-"10195"

#fixing North Battleford which has two records making both 48609
climateDaily$StationID[climateDaily$StationID == "44344"]<-"48609"

#fixing Prince Albert which has two records making both 3322
climateDaily$StationID[climateDaily$StationID == "51878"]<-"3322"

#fixing Waseca t which has two records making both 51798
climateDaily$StationID[climateDaily$StationID == "3270"]<-"51798"

#fixing La Ronge which has two records making both 29371
climateDaily$StationID[climateDaily$StationID == "3381"]<-"29371"
climateDaily$StationID[climateDaily$StationID == "51739"]<-"29371"
#fixing Kindersley which has two records making both 3231
climateDaily$StationID[climateDaily$StationID == "51739"]<-"3231"

#fixing Yorkton which has three records making all 49808
climateDaily$StationID[climateDaily$StationID == "48977"]<-"49808"
climateDaily$StationID[climateDaily$StationID == "46609"]<-"49808"
climateDaily$StationID[climateDaily$StationID == "44203"]<-"49808"
#fixing Regina which has seven records making all 46587
climateDaily$StationID[climateDaily$StationID == "3007"]<-"46587"
climateDaily$StationID[climateDaily$StationID == "3002"]<-"46587"
climateDaily$StationID[climateDaily$StationID == "51441"]<-"46587"
climateDaily$StationID[climateDaily$StationID == "46607"]<-"46587"
climateDaily$StationID[climateDaily$StationID == "28011"]<-"46587"
climateDaily$StationID[climateDaily$StationID == "46588"]<-"46587"

##remove flag columns, Quality columns and make max gust back into numeric
takeout <- grep("Flag", names(climateDaily))
climateDaily <- climateDaily[,-takeout]
climateDaily <- climateDaily[,-grep("Quality", names(climateDaily))]

climateDaily$`Spd of Max Gust (10s deg)`<- gsub("<", "", climateDaily$`Spd of Max Gust (10s deg)`)
climateDaily$`Spd of Max Gust (10s deg)` <- as.numeric(climateDaily$`Spd of Max Gust (10s deg)`)

#aggregate all the climate data so that any common stationIDs get averaged into one record
climateDaily<-aggregate (climateDaily[,-which(names(climateDaily) %in% c("StationID", "Date/Time", "Year", 
                                          "Month", "Day"))], 
                         by = climateDaily[c("StationID", "Date/Time", "Year", 
                                             "Month", "Day")], FUN= mean, na.rm = TRUE)


climateDaily2 <- aggregate (climateHourly[c("Temperature","RelativeHumidity")],
                            by = climateHourly[c("StationID", "Year", "Month", "Day")], 
                            FUN= mean, na.rm = TRUE)
## EW; I don't see the reason for two temperature columns after merging? (PS they're also different!!)
## Let's remove the one from the Hourly data
climateDaily2 <- climateDaily2[,-grep("Temperature", names(climateDaily2))]
# Merge data so we have records that have both Temp and relative humidty 

climateData <- merge(climateDaily, climateDaily2, by= c("StationID", "Year", "Month", "Day"))

climateData$StationID <- as.numeric(climateData$StationID)
# we found that some of the stations did not actually have data so we needed to delete them from the list
# STATION IDs to DELETE 50977, 50091,2956,2967,3300,46609
climateData <- climateData[-which(climateData$StationID %in% c(50977,50091,2956,2967,3300,46609)),]
#Station IDs that only have daily data or incomplete data also had to be excluded 
climateData <- climateData[-which(climateData$StationID %in% c(3261,3186,2930, 51798)),]

## make climateData Date column into a real date (note, some dates were in dash, others in slash)
slashes <- grep("/", climateData$`Date/Time`)
climateData1 <- climateData[-slashes,]
climateData2 <- climateData[slashes,]
climateData1$`Date/Time` <- as.POSIXct(climateData1$`Date/Time`, 
                                                        format="%Y-%m-%d", tz="Canada/Saskatchewan")
climateData2$`Date/Time` <- as.POSIXct(climateData2$`Date/Time`, 
                                                format="%m/%d/%Y", tz="Canada/Saskatchewan")
climateData <- rbind(climateData1, climateData2)

## start finding nearest neighbours
## ====================================================================================================
## take just the information I need out of location file and make it a matrix because that is what is 
##    required of the geoshphere package
location_latlong <- as.matrix(location[,c("long","lat")])

# taking just long and lat of my lakes
sk_latlong <- as.matrix(sksites[,c("longitude","latitude")])

distance <- distm(sk_latlong, location_latlong, fun =distCosine)

# rename the headers as the lakes
colnames(distance)<- location[,'stationID']
rownames(distance) <- sksites[,'lakeName']

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
colnames(lakestations) <- c("lakeName", "stationID")
lakestations$stationID <- as.character(lakestations$stationID)
lakestations$stationID <- as.numeric(lakestations$stationID)

sksites <- merge(sksites, lakestations)
names(sksites)[names(sksites) == "stationID"] <- "StationID"

sksites <- merge(sksites, skflux[,c("lakeName",
                                    names(skflux)[which(!names(skflux) %in% names(sksites))])])

## save modified files
saveRDS(sksites, "../data/private/sksites.rds")
saveRDS(climateData, "../data/sk-climatedata.rds")
