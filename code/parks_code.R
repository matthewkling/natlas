setwd("~/taxorama")
library(lubridate)
library(dplyr)
library(jsonlite)
library(rgdal)

park_id = "PORE" #Point Reyes is park of interest

#### Shapefile ####
# allParkShape <- readOGR("raw_data/park_boundaries","nps_boundary")
# parkShape <- allParkShape[which(allParkShape$UNIT_CODE == park_id),]
# shapePath <- paste0("processed_data/",park_id,"_data/",park_id,"_shape")
# writeOGR(obj = parkShape,dsn = shapePath, layer = park_id ,driver = "ESRI Shapefile") #there's a carto driver that wasn't working for me, unsure what makes a carto shapefile different.

#### Park Observations ####
parkObservations = function(park_id,allParkObs){
parkObs <- allParkObs[which(allParkObs$PARK_CODE == park_id),]

#add dates
parkObs$eventDate <- as.Date(parkObs$eventDate)
parkObs$dateIdentified <- as.Date(parkObs$dateIdentified)

#correct species names
parkObs$speciesFixed <- parkObs$scientificName
parkObs$speciesFixed[which(parkObs$taxonRank == "hybrid")] <- NA
parkObs$speciesFixed <- gsub("([A-z]+) ([A-z]+) ([A-z]+)",'\\1 \\2', parkObs$speciesFixed)
parkObs$speciesFixed[which(parkObs$speciesFixed == "Agaricus xanthoderma 'xanthodermus']")] <- "Agaricus xanthodermus" #specific error in PORE observations

#add category
parkObs$category <- NA
levels(parkObs$category) <- c("Mammal","Bird", "Herp", "Plant", "Fish",  "Invertebrate", "Fungi")

parkObs$category[which(parkObs$kingdom == "Plantae")] <- "Plant"
parkObs$category[which(parkObs$kingdom == "Fungi")] <- "Fungi"
parkObs$category[which(parkObs$kingdom == "Animalia")] <- "Invertebrate"; parkObs$category[which(parkObs$phylum == "Chordata")] <- NA #tags all non-vertebrate animals as invertebrates
parkObs$category[which(parkObs$class == "Aves")] <- "Bird"
parkObs$category[which(parkObs$class == "Mammalia")] <- "Mammal"
parkObs$category[which(parkObs$class == "Reptilia")] <- "Herp"
parkObs$category[which(parkObs$class == "Amphibia")] <- "Herp"
parkObs$category[which(parkObs$class == "Chondrichthyes")] <- "Fish" #cartelaginous fishes
parkObs$category[which(parkObs$class == "Actinopterygii")] <- "Fish" #bony fishes

#Write out the observation data
fullObs <- parkObs
filePathFull = paste0("processed_data/",park_id,"_data/",park_id,"_obs_FULL.csv")
write.csv(fullObs,filePathFull,row.names = F)

cleanObs <- parkObs[,c("decimalLongitude","decimalLatitude","category","family","speciesFixed","year","month")]
filePathClean = paste0("processed_data/",park_id,"_data/",park_id,"_obs_tidy.csv")
write.csv(cleanObs,filePathClean,row.names = F)

return(parkObs)
}

#only runs function if the park data hasn't been processed (park_obs.csv is in gitignore)
if (file.exists(paste0("processed_data/",park_id,"_data/",park_id,"_obs_tidy.csv"))  == F | file.exists(paste0("processed_data/",park_id,"_data/",park_id,"_obs_FULL.csv"))  == F) {
allParkObs <- read.csv("processed_data/park_obs.csv",header = T)
parkObs <- parkObservations(park_id, allParkObs)
}

#### species stats #####
# if above function has already been run, park obs can be retreived from file
if(exists("parkObs") == F) parkObs <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_obs_FULL.csv"), header = T)

inatStats <- function(parkObs)
{
speciesFixed <- unique(parkObs$speciesFixed)
inatSpecies <- data.frame(speciesFixed)

inatSpecies$total_obs <- NA
inatSpecies$user_obs <- NA
inatSpecies[c("kingdom","phylum","class","order","family","genus","category")] <- NA
inatSpecies[c(paste("m",1:12,sep=""))]<- NA
inatSpecies[c(paste("y",2007:2017,sep=""))] <- NA
numSp <- length(inatSpecies$speciesFixed)

for (i in 1:numSp){
spObs <- parkObs[which(parkObs$speciesFixed == inatSpecies$speciesFixed[i]),]
inatSpecies$total_obs[i] = length(spObs$speciesFixed)
inatSpecies$user_obs[i] = length(unique(spObs$recordedBy))
inatSpecies[i,"kingdom"] <- as.character(spObs[1,"kingdom"])
inatSpecies[i,"phylum"] <- as.character(spObs[1,"phylum"])
inatSpecies[i,"class"] <- as.character(spObs[1,"class"])
inatSpecies[i,"order"] <- as.character(spObs[1,"order"])
inatSpecies[i,"family"] <- as.character(spObs[1,"family"])
inatSpecies[i,"genus"] <- as.character(spObs[1,"genus"])
inatSpecies[i,"category"] <- as.character(spObs[1,"category"])

for (j in 1:12)
{
  inatSpecies[i,paste("m",j,sep="")] <- length(which(spObs$month == j))
}

for (k in 2007:2017)
{
  inatSpecies[i,paste("y",k,sep="")] <- length(which(spObs$year == k))
}

#print(paste("percent done =", 100*i/numSp) )
}
return(inatSpecies)
}

inatSpecies <- inatStats(parkObs)

#### comparing to NPS species list ####
  # species list downloaded from NPSpecies, converted to .csv by excel

NPSinatSpecies <- function(inatSpecies,park_id){
parkSpeciesFilepath <- paste0("raw_data/NPSpecies_",park_id,".csv")
parkSpeciesData <- read.csv(parkSpeciesFilepath,header =T)

parkSpeciesFixed <- parkSpeciesData[,c("Taxon.Record.Status","Scientific.Name","Common.Names","Synonyms","Park.Accepted", "Record.Status","Occurrence")]
parkSpeciesFixed <- parkSpeciesFixed[-(grep("([A-z]+) X ([A-z]+)",parkSpeciesFixed$Scientific.Name)),] #removing hybrids
parkSpeciesFixed$speciesFixed <- parkSpeciesFixed$Scientific.Name
parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+) var.*",'\\1 \\2', parkSpeciesFixed$speciesFixed)
parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+) ssp.*",'\\1 \\2', parkSpeciesFixed$speciesFixed)

parkSpeciesFixed <- parkSpeciesFixed[!duplicated(parkSpeciesFixed$speciesFixed),] #removes duplicates due to subspecies/varieties

totalSpecies <- merge(inatSpecies,parkSpeciesFixed,by = "speciesFixed", all = T)
totalSpecies <- totalSpecies[-which(totalSpecies$Occurrence == "Not In Park" & is.na(totalSpecies$total_obs)),]

writeSpecies <- totalSpecies[,-which(names(totalSpecies) %in% c("Scientific.Name","Taxon.Record.Status","Synonyms","Park.Accepted","Record.Status"))]

speciesListPath <- paste0("processed_data/",park_id,"_data/",park_id,"_species.csv")
write.csv(writeSpecies,speciesListPath,row.names = F)

return(writeSpecies)
}

speciesList <- NPSinatSpecies(inatSpecies,park_id = park_id)
