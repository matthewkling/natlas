setwd("~/Desktop/taxorama")
library(lubridate)
library(dplyr)
library(jsonlite)
library(rgdal)

park_id = "PORE" #Point Reyes is park of interest

#### Shapefile ####

allParkShape <- readOGR("raw_data/park_boundaries","nps_boundary")
parkShape <- allParkShape[which(allParkShape$UNIT_CODE == park_id),]
shapePath <- paste0("processed_data/",park_id,"_data/",park_id,"_shape")
writeOGR(obj = parkShape,dsn = shapePath, layer = park_id ,driver = "ESRI Shapefile") #there's a carto driver that wasn't working for me, unsure what makes a carto shapefile different.

#### Park Observations ####
parkObservations = function(park_id,allParkObs){
parkObs <- allParkObs[which(allParkObs$PARK_CODE == park_id),]

#add dates
parkObs$eventDate <- as.Date(parkObs$eventDate)
parkObs$dateIdentified <- as.Date(parkObs$dateIdentified)
parkObs$year <- year(parkObs$eventDate)
parkObs$month <- month(parkObs$eventDate)

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

##### Convert species list to JSON ####
#pulls from file if skipping higher up functions
if (exists("speciesList") == F) speciesList <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_species.csv"), header = T)

toCharacter <- function(data,colums)
{
  for (i in colums)
  {
    data[,which(names(data)==i)] <- as.character(data[,which(names(data)==i)])
  }
  return(data)
}

columns_defactor <- c("kingdom","phylum","class","order","family","genus","category","speciesFixed")
testing <- speciesList[1:10,]



f <- toCharacter(testing,columns_defactor)

f <- f %>%
  mutate(species=regexpr(" ", speciesFixed),
         species=substr(speciesFixed, species+1, nchar(speciesFixed))) %>%
  group_by(category, order, family, genus, species) %>%
  arrange(category, order, family, genus, species) %>%
  as.data.frame() 

d = f
p <- lapply(1:nrow(d), function(i) data.frame(name=d$speciesFixed[i],
                                              level="speciesFixed",
                                              species=d$species[i],
                                              total_obs=d$total_obs[i],
                                              user_obs=d$user_obs[i],
                                              # m1=d$m1[i],
                                              # m2=d$m2[i],
                                              # m3=d$m3[i],
                                              # m4=d$m4[i],
                                              # m5=d$m5[i],
                                              # m6=d$m6[i],
                                              # m7=d$m7[i],
                                              # m8=d$m8[i],
                                              # m9=d$m9[i],
                                              # m10=d$m10[i],
                                              # m11=d$m11[i],
                                              # m12=d$m12[i],
                                              # y2007=d$y2007[i],
                                              # y2008=d$y2008[i],
                                              # y2009=d$y2009[i],
                                              # y2010=d$y2010[i],
                                              # y2011=d$y2011[i],
                                              # y2012=d$y2012[i],
                                              # y2013=d$y2013[i],
                                              # y2014=d$y2014[i],
                                              # y2015=d$y2015[i],
                                              # y2016=d$y2016[i],
                                              # y2017=d$y2017[i],
                                              # npsOccurence = d$Occurrence[i],
                                              commonName = d$Common.Names[i],
                                              n_species=1))


group_taxa <- function(data, groupings, level){
  parents <- unique(groupings[,1])
  lapply(parents, function(x){
    print(x)
    children <- which(parents==x)
    list(name=x, level=level, children=data[children])
  })
}

# apply over all taxonomic levels, generating tree-like list
levels <- c("category","class", "family", "genus", "speciesFixed")
levels <- c("family","genus", "speciesFixed")
levels <- c("family","genus", "speciesFixed")
for(level in rev(levels)[2:length(levels)]){
  if(level=="speciesFixed") next()
  child_level <- levels[match(level, levels)+1]
  p <- group_taxa(p, d[,c(level, child_level)], level)
}

### convert to JSON format

# serialize to json
json <- toJSON(p)
jsonn <- json

jsonn <- gsub(',\\{\\}', '', jsonn)

# remove brackets from taxon names to match target format
jsonn <- gsub('name":\\[', 'name":', jsonn)
jsonn <- gsub('\\],"level', ',"level', jsonn)
jsonn <- gsub('level":\\[', 'level":', jsonn)
jsonn <- gsub('\\],"children', ',"children', jsonn)

jsonn <- gsub('\\[\\[', '[', jsonn)
jsonn <- gsub('\\]\\]', '\\]', jsonn)
jsonn <- gsub('\\],\\[', ',', jsonn)

# remove outermost brackets to match target format
jsonn <- substr(jsonn, 2, nchar(jsonn)-1)

# format and export
jsonn <- prettify(jsonn)
jsonPath <- paste0("processed_data/",park_id,"_data/",park_id,"_taxonomy.json")
write(jsonn, jsonPath)
