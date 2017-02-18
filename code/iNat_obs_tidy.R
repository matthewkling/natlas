#Checking out the iNat data
library(maps)
library(rgdal)
library(dplyr)
library(raster)
library(vioplot)
#library(maptools)

#setwd("/Users/lauraalexander/taxorama")
#setwd("/Users/lauraalexander/Desktop/taxorama")
setwd("~/Desktop/partners/taxorama") #feel shame

#### Download complete dataset ####
    # Data suggested by Ken-ichi, found at http://www.inaturalist.org/observations/gbif-observations-dwca.zip

# download.file("http://www.inaturalist.org/observations/gbif-observations-dwca.zip", "raw_data/all_inat_obs")
#     unzip("raw_data/all_inat_obs", files = "observations.csv", exdir = "raw_data")
#     file.remove("raw_data/all_inat_obs")


#### Read in data, select columns ####
allObs <- read.csv("raw_data/observations.csv", header = T)
usObs <- subset(allObs,countryCode == "US")

usObs <- usObs[-which(usObs$establishmentMeans == "cultivated"),]
usObs <- usObs[-which(!is.finite(usObs$decimalLatitude)),]
usObs <- usObs[-which(usObs$informationWithheld == "Coordinates hidden at the request of the observer"),]
#usObs <- usObs[-which(usObs$informationWithheld == "Coordinate uncertainty increased by 10000m at the request of the observer"),] #6 miles uncertainty added; between threatened taxa and observer requests, ~8% of points are altered that way. Remove?
#usObs <- usObs[-which(usObs$informationWithheld == "Coordinate uncertainty increased by 10000m to protect threatened taxon"),]

usObs <- usObs[,c(
  "id", "recordedBy", 
  "eventDate", "eventTime",             
  "decimalLatitude", "decimalLongitude", "verbatimLocality", "coordinateUncertaintyInMeters", 
  "informationWithheld", 
  #"establishmentMeans", # all cultivated have been removed, no need to pull this column
  "identificationID", "taxonID","scientificName","taxonRank",
  "kingdom", "phylum", "class","order","family","genus"
)]

safeus <- usObs
usObs$eventDate <- as.Date(usObs$eventDate)

#### Add counties, if in a park ####
  # This is borrowed from Matt's reptiles code, thanks Matt!

# load counties shapefile
s <- readOGR("raw_data/cb_2015_us_county_5m", "cb_2015_us_county_5m")
coordinates(usObs) <- c("decimalLongitude","decimalLatitude")
crs(usObs) <- crs(s)

o <- over(usObs, s)
o$FIPS <- NA
o$FIPS <- paste(o$STATEFP,o$COUNTYFP,sep="")
usObs <- cbind(coordinates(usObs), usObs@data, o[,c("STATEFP", "NAME","FIPS")])
#write.csv(usObs, "processed_data/us_obs.csv", row.names = F)

# Add park boundary info
parks <- readOGR("raw_data/park_boundaries","nps_boundary")
coordinates(usObs) <- c("decimalLongitude","decimalLatitude")
crs(usObs) <- crs(parks)

p <- over(usObs, parks)
parkInfo <- p[,c("UNIT_CODE","UNIT_NAME","UNIT_TYPE")]
names(parkInfo)[which(names(parkInfo) == "UNIT_CODE")] <- "PARK_CODE"
names(parkInfo)[which(names(parkInfo) == "UNIT_NAME")] <- "PARK_NAME"
names(parkInfo)[which(names(parkInfo) == "UNIT_TYPE")] <- "PARK_TYPE"
parkInfo$IN_PARK <- NA
parkInfo$IN_PARK <- !is.na(parkInfo$PARK_CODE)

usObs <- cbind(coordinates(usObs), usObs@data, parkInfo)
write.csv(usObs, "processed_data/us_obs.csv", row.names = F)

#### Split to groups #####
amphibians <- subset(usObs, class == "Amphibia"); 
arachnids <- subset(usObs, class == "Arachnida"); 
birds <- subset(usObs, class == "Aves"); 
rayfish <- subset(usObs, class == "Actinopterygii"); 
insects <- subset(usObs, class == "Insecta"); 
mammals <- subset(usObs, class == "Mammalia"); 
plants <- subset(usObs, kingdom == "Plantae"); 
reptiles <- subset(usObs, class == "Reptilia"); 

# #Write files
# write.csv(amphibians, "processed_data/amphibians.csv", row.names = F)
# write.csv(arachnids, "processed_data/arachnids.csv", row.names = F)
# write.csv(birds, "processed_data/birds.csv", row.names = F)
# write.csv(rayfish, "processed_data/rayfish.csv", row.names = F)
# write.csv(insects, "processed_data/insects.csv", row.names = F)
# write.csv(mammals, "processed_data/mammals.csv", row.names = F)
# write.csv(plants, "processed_data/plants.csv", row.names = F)
# write.csv(reptiles, "processed_data/reptiles.csv", row.names = F)



#### US Users ####
user_id <- unique(usObs$recordedBy)
userData <- data.frame(user_id)

userData$total_observations <- NA
userData$active_counties <- NA
userData$active_states <- NA
userData$proportion_obs_park  <- NA

userData$species_observed <- NA
userData$genus_observed <- NA
userData$family_observed <- NA
userData$order_observed <- NA
userData$class_observed <- NA
userData$phylum_observed <- NA
userData$kingdom_observed <- NA
userData$first_date <- as.Date(NA)
userData$last_date <- as.Date(NA)
userData$activity_period_days <- NA


for (i in 967:length(userData$user_id)){
  #for (i in 1:10){
  userObs <- usObs[which(usObs$recordedBy==userData$user_id[i]),]
  # userData$total_observations[i] <- length(unique(userObs$id))
  # userData$active_counties[i] <- length(unique(userObs$FIPS))
  # userData$active_states[i] <- length(unique(userObs$STATEFP))
  # 
  # userData$proportion_obs_park[i] <- sum(userObs$IN_PARK == T)/length(unique(userObs$id)) #identification ID vs id????????? Only use those with identification id?
  # 
  # 
  # userData$species_observed[i]  <- length(unique(userObs$scientificName))
  # userData$genus_observed[i]  <- length(unique(userObs$genus))
  # userData$family_observed[i]  <- length(unique(userObs$family))
  # userData$order_observed[i]  <- length(unique(userObs$order))
  # userData$class_observed[i]  <- length(unique(userObs$class))
  # userData$phylum_observed[i]  <- length(unique(userObs$phylum))
  # userData$kingdom_observed[i]  <- length(unique(userObs$kingdom))
  # userData$first_event_date[i]  <- as.Date(min(userObs$eventDate))
  userData$first_id_date[i]  <- as.Date(min(userObs$idDate))
  # userData$last_date[i]  <- as.Date(max(userObs$eventDate))
  userData$activity_period_days[i]  <- max(userObs$eventDate) - min(userObs$eventDate)
  # 
  # percentthrough <- 100*i/length(userData$user_id)
  # print(paste(percentthrough, "%"))
}

 


#### Map for the curious ####

map(parks, col = "darkgreen", boundary = F, xlim=c(-179,-67), add = T, interior = TRUE)

map("world", c("USA","hawaii"), xlim=c(-179,-67), ylim=c(19,71), interior = TRUE) #Includes Alaska and Hawaii



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
# points(rayfish$decimalLongitude, rayfish$decsimalLatitude , col = "cyan", pch = ".")
# points(reptiles$decimalLongitude, reptiles$decimalLatitude , col = "brown", pch = ".")


userData <- userData[-which(userData$total_observations > 10000),]
realUserData <- userData[-which(userData$total_observations <= 10),]
absurdUserData <-userData[which(userData$total_observations > 1000),]
undergrads <- userData[which(userData$total_observations <= 10),]
