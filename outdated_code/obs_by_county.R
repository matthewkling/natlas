#Working with observations at county level

setwd("/Users/lauraalexander/Desktop/taxorama")
#FIRST RUN data_download.R, iNat_obs_tidy.R, and county_data_cleaning.R

obs <- read.csv("processed_data/clean_us_obs.csv", header = T)
countyData <- read.csv("processed_data/county_info.csv", header = T)
countyData$FIPS <- as.factor(countyData$FIPS)

# load counties shapefile
s <- readOGR("raw_data/cb_2015_us_county_5m", "cb_2015_us_county_5m")
coordinates(usObs) <- c("decimalLongitude","decimalLatitude")
crs(usObs) <- crs(s)

o <- over(usObs, s)
o$FIPS <- NA
o$FIPS <- paste(o$STATEFP,o$COUNTYFP,sep="")
usObs <- cbind(coordinates(usObs), usObs@data, o[,c("STATEFP", "NAME","FIPS")])
#write.csv(usObs, "processed_data/us_obs_counties.csv", row.names = F)

#FIPS hate staying put, fixing those
FIPSfix <- function(fipscol)
{
  fipscol <- as.character(fipscol)
  for (i in 1:length(fipscol)){
    if (nchar(fipscol[i]) < 5) {fipscol[i] <- paste(0,fipscol[i],sep="")}
    if (nchar(fipscol[i]) != 5) {errors <- c(errors,i)}
  }
  return(fipscol)
}

countyData$FIPS <- FIPSfix(countyData$FIPS)

#Check the ones that don't match
broken <- obs[!(obs$FIPS %in% countyData$FIPS),c("verbatimLocality","STATEFP","NAME","FIPS")] #pretty much all of these are water
#obs <- obs[-which(countyData$FIPS == "NANA"),] #remove the NANAs

#### Merge ####
obsByCounty <- base::merge(obs, countyData, by = "FIPS", all = F)
