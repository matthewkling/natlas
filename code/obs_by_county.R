#Working with observations at county level

obs <- read.csv("processed_data/mammals.csv", header = T)
countyData <- read.csv("processed_data/county_info.csv", header = T)
countyData$FIPS <- as.factor(countyData$FIPS)

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
