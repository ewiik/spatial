## script to calculate CO2 flux for Brian's sites.
## units: nutrients; elements; DIC + DOC: mg/L; conductivity uS (assuming cm-1); note that 
##    specific conductance is reported in Cumming1995, and we are using conductivity;
##    alt m; salinity in gL-1 (i.e. ppt)
## note that for gases in atmosphere, assuming ideal gas relationships
##    https://www.researchgate.net/post/How_can_I_convert_micro_atmosphere_pCO2_into_PPM_pCO2
##    ppm CO2 = uatm CO2 --- however for aquatic situations, not quite the same...??

## required variables for gasexchangeflex: 
## Temperature Celsius ("temp"), pH ("ph"), Conductivity [uS/cm] ("cond"), 
## Atmospheric pCO2 ppm ("pco2atm"), Wind m/s ("wind"), Salinity ppt|g/L ("salt"), 
## "either or" etc variables: Barometric pressure kP ("kpa") & Altitude (m) ("alt");   
##                        DIC uM ("dic") may be calculated from cond for kerri

## FIXME: were the data really in August 1993??
## FIXME: get realistic wind
## FIXME: what is the salinity unit in the document I got from Kerri?

## read in data; the csv is my revamp of the xls kerri sent; it now has lat longs and altitude
##    of each site
if (!file.exists("../data/piscesallorgh.csv")) {
  stop("Get piscesallorgh.csv from emma")
}
pisces <- read.csv("../data/piscesallorgh.csv") 

if (!file.exists("../data/pisces-sal-gL.csv")) {
  stop("Get pisces salinity data in g/L from Emma")
}
salgl <- read.csv("../data/pisces-sal-gL.csv")

if (!file.exists("~/git/flux/data/maunaloa.csv")) {
  source("~/git/flux/scripts/getmaunaloa.R")
}
ml <- read.csv("~/git/flux/data/maunaloa.csv") 

## get atmospheric co2 into the data frame
mlsub <- subset(ml, select = c('Year', 'Month', 'pCO2'))
names(mlsub) <- c("Year", "Month", "pco2atm")

pisces$Year <- rep(1993)
pisces$Month <- rep(8)

pisces <- merge(pisces, mlsub, by = c("Year", "Month"))

## make units right
pisces <- transform(pisces, DICumol = DIC / 0.012) # mg/L --> umol

pisces$salgl <- salgl$salgl
## here I had to type in manually from Cumming1995 appendix

## add in some dummy wind for now
pisces$wind <- rep(4)

## source function from flux directory
source("~/git/flux/functions/gasExchangeFlex.R")

args(gasExchangeFlex) # couldn't remember...
co2fluxz <- with(pisces, gasExchangeFlex(temp = TEMPSURF, cond = CONDUCT, ph = PH,
                          wind = wind, salt = salgl, dic = DICumol, 
                          alt = alt, altnotkpa = TRUE,
                          pco2atm = pco2atm))

piscesz <- cbind(pisces, co2fluxz) 
## note we would have temp bottom too

## change all column names to have units and stuff
## some in spreadsheet are not in Cumming1995 appendix material so not sure
piscesunits <- piscesz
names(piscesunits)[names(piscesunits) %in% c("DIC", "CONDUCT", "O2TOP", "O2BOT", "pco2atm", "salgl", 
                                     "wind", "fluxenh", "pco2")] <-
  c("DICmgL", "CONDuScm", "O2TOP?", "O2BOT?", "pCO2ATMppm", "SALgL", "WINDms", "CO2FLUXmmolm2d",
    "pCO2uatm")


## save output and some diagnostic plots
write.csv(piscesz, "../data/BC-fluxdata.csv", row.names = FALSE)
write.csv(piscesunits, "../data/BC-fluxdata-units.csv", row.names = FALSE)

pdf("../data/BC-fluxdata.pdf", onefile = TRUE)
with(piscesz, plot(fluxenh ~ PH, col = ifelse(log(DIC + 1) > 10, "red", "black")))
legend("topright", legend = "red: log(DIC) > 10")
abline(1,0)
with(piscesz, plot(pco2 ~ PH, col = ifelse(log(DIC + 1) > 10, "red", "black")))
legend("topright", legend = "red: log(DIC) > 10")
with(piscesz, plot(log(DIC + 1) ~ PH))
dev.off()


