## script to compare the spatial data set flux relationships

## read in files
sk <- read.csv("~/git/flux/data/private/co2fluxheathersites.csv")
bc <- read.csv("../data/BC-fluxdata.csv") 

## match names for melting
names(sk)
names(bc)
bcsub <- subset(bc, select = c("Year", "Month", "lakename", "alt", "CHLA", "DIC", "CONDUCT", 
                               "O2TOP", "PH", "TEMPSURF", "salgl", "wind", "fluxenh", "pco2"))
sksub <- subset(sk, select = c("Year", "Month", "lakeName", "Altitude", "chlvalue", 
                               "DIC", "conductivity", "oxygenConcentration", "pH", "temperature",
                               "salinity", "wind", "CO2fluxmmolm2d", "pCO2uatm"))
names(bcsub) <- names(sksub)                
bcsub$region <- rep("bc")
sksub$region <- rep("sk")

subs <- rbind(bcsub, sksub)

write.csv(subs, "../data/bc-sk-fluxes.csv", row.names = FALSE)
