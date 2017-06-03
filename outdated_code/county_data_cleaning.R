#Cleaning county data
#Output includes population and land/water area (data also exist and could be obtained for demographic, economic, and educational characteristics)

setwd("~/Desktop/taxorama")

#FIPS repair function
FIPSfix <- function(fipscol)
{
  fipscol <- as.character(fipscol)
  for (i in 1:length(fipscol)){
    if (nchar(fipscol[i]) < 5) {fipscol[i] <- paste(0,fipscol[i],sep="")}
    if (nchar(fipscol[i]) != 5) {errors <- c(errors,i)}
  }
  return(fipscol)
}

#### Population ####
#data downloaded from https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk

rawPop <- read.csv("raw_data/census_data/PEP_2015_PEPANNRES_with_ann.csv",header = T, skip = 1); popData <- rawPop #I prefer to be able to look back at the original read-in, changes are mader to popData

#abbreviate column names
names(popData) <- c("Id", "FIPS", "countyState", "census2010", "EstimatesBase2010", "popEst2010", "popEst2011", "popEst2012", "popEst2013", "popEst2014", "popEst2015") 

popData$FIPS <- FIPSfix(popData$FIPS) #function to correct FIPS missing leading 0 


#### Area ####
#data downloaded from https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=DEC_10_SF1_G001&prodType=table (use options to add all counties to the geography, then transpose rows and columns)
#seven hundred pages of help/documentation available here: http://www.census.gov/prod/cen2010/doc/sf1.pdf
#This dataset includes population
# County shapefiles also have this information

rawGeo <- read.csv("raw_data/county_information/DEC_10_SF1_G001.csv",header = T, skip = 1)
geoData <- subset(rawGeo,select = c(
  "Id2", #FIPS code
  "Geography", #county and state names
  "AREA.CHARACTERISTICS...Area..Land.", #county land area, in square meters
  "AREA.CHARACTERISTICS...Area..Water.", #county water area, in square meters (there is not a total county area column)
  "AREA.CHARACTERISTICS...Population.Count..100.." #population
))

names(geoData) <- c("FIPS", "countyState", "areaLand", "areaWater", "population2010")

geoData$FIPS <- FIPSfix(geoData$FIPS) #function to correct FIPS missing leading 0 

#### Check data ####
#Check non-matching entries
geoData[!(geoData$FIPS %in% popData$FIPS),]
popData[!(popData$FIPS %in% geoData$FIPS),]

#Wade Hampton Census Area, Alaska (FIPS 02270) became Kusilvak Census Area, Alaska (FIPS 02158)
idx.Kusilvak <- which(geoData$FIPS=="02270")
geoData$FIPS[idx.Kusilvak] <- "02158"
#geoData$countyState[idx.Kusilvak] <- "Kusilvak Census Area, Alaska" #would have to create new factor level, will let merge handle this

#Shannon County, South Dakota (FIPS 46113) became Oglala Lakota County, South Dakota (FIPS 46102) in 2015
idx.Shannon <- which(geoData$FIPS=="46113")
geoData$FIPS[idx.Shannon] <- "46102"
#geoData$countyState[idx.Shannon] <- "Oglala Lakota County, South Dakota" #would have to create new factor level, will let merge handle this

#Bedford City (FIPS 51515) is now included in Bedford County (51019). NOTE: the census 2010 value in the pop document is pre-merge, which disappears ~6k people. (But the estimate is them added together, go figure)
#Have to add together original city and county values to get new area values, will be calling on unchanged table.
raw.City <- which(rawGeo$Id2== "51515")
raw.County <- which(rawGeo$Id2== "51019")
land = rawGeo$AREA.CHARACTERISTICS...Area..Land.[raw.City] + rawGeo$AREA.CHARACTERISTICS...Area..Land.[raw.County]
water = rawGeo$AREA.CHARACTERISTICS...Area..Water.[raw.City] + rawGeo$AREA.CHARACTERISTICS...Area..Water.[raw.County]
pop = rawGeo$AREA.CHARACTERISTICS...Population.Count..100..[raw.City] + rawGeo$AREA.CHARACTERISTICS...Population.Count..100..[raw.County]

#Change county data
idx.County <- which(geoData$FIPS== "51019")
geoData$areaLand[idx.County] <- land
geoData$areaWater[idx.County] <- water
geoData$population2010[idx.County] <- pop

#Remove city
geoData <- geoData[-which(geoData$FIPS== "51515"),]


#### Merge and export ####
geoDataMerge <- geoData[,c("FIPS", "areaLand", "areaWater")]
popDataMerge <- popData[, c("FIPS", "countyState", "census2010", "popEst2010", "popEst2015")]
countyData <- merge(popDataMerge,geoDataMerge,by = c("FIPS", "FIPS"))      
countyData$FIPS <- as.factor(countyData$FIPS)

write.csv(countyData, "processed_data/county_info.csv",row.names = F)


#### Additional available data ####
#General demographic
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=DEC_10_DP_DPDP1&src=pt

#General economic
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_DP03&src=pt

#Educational attainment
#https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_15_5YR_S1501&src=pt