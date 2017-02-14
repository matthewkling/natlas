#Checking out the iNat data
library(maps)
library(rgdal)
library(dplyr)
library(raster)

setwd("/Users/lauraalexander/taxorama")


#### From complete dataset ####
  # Data from http://www.inaturalist.org/observations/gbif-observations-dwca.zip
allObs <- read.csv("raw_data/observations.csv",header = T)
usObs <- subset(allObs,countryCode == "US")

usObs <- usObs[,c(
  "id", "recordedBy", 
  "eventDate", "eventTime",             
  "decimalLatitude", "decimalLongitude", "verbatimLocality", "coordinateUncertaintyInMeters", "informationWithheld", "establishmentMeans",
  "identificationID", "taxonID","scientificName","taxonRank",
  "kingdom", "phylum", "class","order","family","genus"
)]

usObs <- usObs[-which(!is.finite(usObs$decimalLatitude)),]

#### Add counties ####
  # This is borrowed from Matt's reptiles code, thanks Matt!

# load counties shapefile
s <- readOGR("raw_data/cb_2015_us_county_5m", "cb_2015_us_county_5m")
coordinates(usObs) <- c("decimalLongitude","decimalLatitude")
crs(usObs) <- crs(s)

o <- over(usObs, s)
o$FIPS <- NA
o$FIPS <- paste(o$STATEFP,o$COUNTYFP,sep="")

usObs <- cbind(coordinates(usObs), usObs@data, o[,c("STATEFP", "NAME","FIPS")])
write.csv(usObs, "processed_data/us_obs.csv", row.names = F)



#### Split to groups #####
amphibians <- subset(usObs, class == "Amphibia"); write.csv(amphibians, "processed_data/amphibians.csv", row.names = F)
arachnids <- subset(usObs, class == "Arachnida"); write.csv(arachnids, "processed_data/arachnids.csv", row.names = F)
birds <- subset(usObs, class == "Aves"); write.csv(birds, "processed_data/birds.csv", row.names = F)
rayfish <- subset(usObs, class == "Actinopterygii"); write.csv(rayfish, "processed_data/rayfish.csv", row.names = F)
insects <- subset(usObs, class == "Insecta"); write.csv(insects, "processed_data/insects.csv", row.names = F)
mammals <- subset(usObs, class == "Mammalia"); write.csv(mammals, "processed_data/mammals.csv", row.names = F)
plants <- subset(usObs, kingdom == "Plantae"); write.csv(plants, "processed_data/plants.csv", row.names = F)
reptiles <- subset(usObs, class == "Reptilia"); write.csv(reptiles, "processed_data/reptiles.csv", row.names = F)

#### Map for the curious ####
# map("world", c("USA","hawaii"), xlim=c(-179,-67), ylim=c(19,71), interior = TRUE) #Includes Alaska and Hawaii

# #map("state", boundary = FALSE, col="black", add = TRUE, fill=FALSE, lwd = .4)
# 
# map("state", boundary = T, col="black", add = F, fill=FALSE, lwd = .4)
# #map("county", boundary = FALSE, col="grey90", add = TRUE, fill=FALSE)
# 
# points(amphibians$decimalLongitude, amphibians$decimalLatitude , col = "blue", pch = ".")
# points(arachnids$decimalLongitude, arachnids$decimalLatitude , col = "darkorange", pch = ".")
# points(birds$decimalLongitude, birds$decimalLatitude , col = "violet", pch = ".")
# points(insects$decimalLongitude, insects$decimalLatitude , col = "green", pch = ".")
# points(mammals$decimalLongitude, mammals$decimalLatitude , col = "red", pch = ".")
# points(plants$decimalLongitude, plants$decimalLatitude , col = "darkgreen", pch = ".")
# points(rayfish$decimalLongitude, rayfish$decimalLatitude , col = "cyan", pch = ".")
# points(reptiles$decimalLongitude, reptiles$decimalLatitude , col = "brown", pch = ".")





