#Checking out the iNat data
# library(maps)
library(rgdal)
# library(dplyr)
library(raster)


setwd("/Users/lauraalexander/Desktop/taxorama") 
#First run data_download.R to get all the datasets used in this code

#### Read in data, select columns ####
allObs <- read.csv("raw_data/observations.csv", header = TRUE) #read in complete iNat dataset
usObsRaw <- subset(allObs,countryCode == "US") #select only observations from inside US

usObs <- usObsRaw #means that if we make an error while removing columns or other data, we don't have to read in all the data over again.

usObs <- usObs[-which(usObs$establishmentMeans == "cultivated"),] #so leaves observations that are wild
usObs <- usObs[-which(!is.finite(usObs$decimalLatitude)),]  #removes observations with bad coordinates
usObs <- usObs[-which(usObs$informationWithheld == "Coordinates hidden at the request of the observer"),]
#usObs <- usObs[-which(usObs$informationWithheld == "Coordinate uncertainty increased by 10000m at the request of the observer"),] #6 miles uncertainty added; between threatened taxa and observer requests, ~8% of points are altered that way. Remove?
#usObs <- usObs[-which(usObs$informationWithheld == "Coordinate uncertainty increased by 10000m to protect threatened taxon"),]

usObs <- usObs[,c( #selecting the columns of interest for us; if you want to see complete column options, run names(usObsRaw) in console
  "id", "recordedBy", 
  "eventDate", "eventTime", "dateIdentified",             
  "decimalLatitude", "decimalLongitude", "verbatimLocality", "coordinateUncertaintyInMeters", 
  "informationWithheld", 
  #"establishmentMeans", # all cultivated have been removed, no need to pull this column
  "identificationID", "taxonID","scientificName","taxonRank",
  "kingdom", "phylum", "class","order","family","genus"
)]

#changes date from factor or whatever to be read as dates
usObs$eventDate <- as.Date(usObs$eventDate)
usObs$dateIdentified <- as.Date(usObs$dateIdentified)

cleanObs <- usObs[-which(usObs$eventDate < "2007-01-01"),] #removes pre-2007 observations 
write.csv(cleanObs, "processed_data/clean_us_obs.csv", row.names = F) #writes out files 

#### Add park info; for counties, see obs_by_county.R ####
obs <- cleanObs
#obs <- read.csv("processed_data/clean_us_obs.csv", header = T) #if we have already run the top bit, we can just read in the output; I also like this as a check that the top bit worked. It's more time consuming than reassigning cleanObs (or just using cleanObs, but eh.)
parks <- readOGR("raw_data/park_boundaries","nps_boundary")
coordinates(obs) <- c("decimalLongitude","decimalLatitude") #tell R that these are coordinates
crs(obs) <- crs(parks)

p <- over(obs, parks) #checks which points are in the park
parkInfo <- p[,c("UNIT_CODE","UNIT_NAME","UNIT_TYPE")] #pulls information about the park polygons points fall within
names(parkInfo)[which(names(parkInfo) == "UNIT_CODE")] <- "PARK_CODE"
names(parkInfo)[which(names(parkInfo) == "UNIT_NAME")] <- "PARK_NAME"
names(parkInfo)[which(names(parkInfo) == "UNIT_TYPE")] <- "PARK_TYPE"
parkInfo$IN_PARK <- NA #makes a column full of NA, so we can make a column about if they are or aren't in parks
parkInfo$IN_PARK <- !is.na(parkInfo$PARK_CODE) #if a point was in a park, it got the associated park code, if not the PARK_CODE column is NA. This column will be true if the PARK_CODE value is not NA, ie if it is in a park- this was more useful when we were working with the whole dataset, but makes it easy/transparent to pull out only observations made inside of a park.

obs <- cbind(coordinates(obs), obs@data, parkInfo) #adds the park information to the observations dataframe

parkObs <- obs[which(obs$IN_PARK == T),] #pulls all observations made within park
write.csv(parkObs, "processed_data/park_obs.csv", row.names = F) #writes file of park observations.

#### US user data ####
allUS <- cleanObs
#allUS <- read.csv("processed_data/clean_us_obs.csv", header = T) #if we have already run the top bit, we can just read in the output

  user_id <- unique(allUS$recordedBy)
  userData <- data.frame(user_id)
  
  userData$total_observations <- NA
  #userData$active_counties <- NA #no longer including these
  #userData$active_states <- NA #no longer including these
  #userData$proportion_obs_park  <- NA
  
  userData$species_observed <- NA
  userData$genus_observed <- NA
  userData$family_observed <- NA
  userData$order_observed <- NA
  userData$class_observed <- NA
  userData$phylum_observed <- NA
  userData$kingdom_observed <- NA
  
  userData$first_event_date <- as.Date(NA)
  userData$first_id_date <- as.Date(NA)
  userData$last_event_date <- as.Date(NA)
  userData$activity_period <- NA
  
  userLen <- length(userData$user_id)
  
for (i in 1:userLen){
  # #for (i in 1:10){
  userObs <- allUS[which(allUS$recordedBy==userData$user_id[i]),]
  userData$total_observations[i] <- length(unique(userObs$id))
  #userData$active_counties[i] <- length(unique(userObs$FIPS))
  #userData$active_states[i] <- length(unique(userObs$STATEFP))
  #userData$proportion_obs_park[i] <- sum(userObs$IN_PARK == T)/length(userObs$IN_PARK) #identification ID vs id????????? Only use those with identification id?

  userData$species_observed[i]  <- length(unique(userObs$scientificName))
  userData$genus_observed[i]  <- length(unique(userObs$genus))
  userData$family_observed[i]  <- length(unique(userObs$family))
  userData$order_observed[i]  <- length(unique(userObs$order))
  userData$class_observed[i]  <- length(unique(userObs$class))
  userData$phylum_observed[i]  <- length(unique(userObs$phylum))
  userData$kingdom_observed[i]  <- length(unique(userObs$kingdom))

  userData$first_event_date[i]  <- as.Date(min(userObs$eventDate))
  userData$first_id_date[i]  <- as.Date(min(userObs$dateIdentified))
  userData$last_event_date[i]  <- as.Date(max(userObs$eventDate))

  if(i %% 100 == 0){print(paste(100 * i/userLen, "%")); print(summary(userData$total_observations))}
}

write.csv(userData, "processed_data/us_users.csv",row.names = F)

users$activity_period <- users$first_event_date - users$last_event_date
