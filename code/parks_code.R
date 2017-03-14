setwd("~/Desktop/taxorama")
library(lubridate)

allParkObs <- read.csv("processed_data/park_obs.csv",header = T)

#Point Reyes is park of interest
parkObs <- allParkObs[which(allParkObs$PARK_CODE == "PORE"),]

parkObs$eventDate <- as.Date(parkObs$eventDate)
parkObs$dateIdentified <- as.Date(parkObs$dateIdentified)
parkObs$year <- year(parkObs$eventDate)
parkObs$month <- month(parkObs$eventDate)

#### species statistics  #####
#correct species names
parkObs$speciesFixed <- parkObs$scientificName
parkObs$speciesFixed[which(parkObs$taxonRank == "hybrid")] <- NA
parkObs$speciesFixed <- gsub("([A-z]+) ([A-z]+) ([A-z]+)",'\\1 \\2', parkObs$speciesFixed)
parkObs$speciesFixed[which(parkObs$speciesFixed == "Agaricus xanthoderma 'xanthodermus']")] <- "Agaricus xanthodermus"

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
cleanObs <- parkObs[,c("decimalLongitude","decimalLatitude","coordinateUncertaintyInMeters","kingdom","phylum","class","order","family","genus","year","month","speciesFixed","category"  )]
write.csv(cleanObs,"processed_data/PORE_obs.csv",row.names = F)

#### species stats #####
speciesFixed <- unique(parkObs$speciesFixed)
inatSpecies <- data.frame(speciesFixed)

inatSpecies$total_obs <- NA
inatSpecies$user_obs <- NA
inatSpecies[c("kingdom","phylum","class","order","family","genus","category")] <- NA
# inatSpecies[c(paste("m",1:12,sep=""))]<- NA
# inatSpecies[c(paste("y",2007:2017,sep=""))] <- NA
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

print(paste("percent done =", 100*i/numSp) )
}

#### comparing to NPS species list ####
  # species list downloaded from NPSpecies, converted to .csv by excel, 
parkSpeciesData <- read.csv("raw_data/NPSpecies_PORE.csv",header =T)


parkSpeciesFixed <- parkSpeciesData[,c("Taxon.Record.Status","Scientific.Name","Common.Names","Synonyms","Park.Accepted", "Record.Status","Occurrence")]
parkSpeciesFixed <- parkSpeciesFixed[-(grep("([A-z]+) X ([A-z]+)",parkSpeciesFixed$Scientific.Name)),] #removing hybrids
parkSpeciesFixed$speciesFixed <- parkSpeciesFixed$Scientific.Name
parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+) var.*",'\\1 \\2', parkSpeciesFixed$speciesFixed)
parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+) ssp.*",'\\1 \\2', parkSpeciesFixed$speciesFixed)
#parkSpeciesFixed$scientificName <- gsub("([A-z]+) ([A-z]+) ([A-z]+)",'\\1 \\2', parkSpeciesFixed$scientificName)

# park <- unique(parkSpeciesFixed$scientificName); parkOnly <- sort(park[-which(park %in% inat)])
# inat <- unique(parkObs$speciesFixed); inatOnly <- sort(inat[-which(inat %in% park)])

totalSpecies <- merge(inatSpecies,parkSpeciesFixed,by = "speciesFixed", all = T)
totalSpecies <- totalSpecies[-which(totalSpecies$total_obs == NA & totalSpecies$Occurrence == "Not In Park")]

write.csv(totalSpecies,"processed_data/PORE_species.csv",row.names = F)



