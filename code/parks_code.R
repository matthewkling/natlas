setwd("~/taxorama")
library(lubridate)
library(dplyr)
library(jsonlite)
library(rgdal)
library(spocc)
library(taxize)

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
  parkObs$speciesFixed[which(parkObs$taxonRank == "hybrid")] <- NA #set their names to NA
  parkObs <- parkObs[-which(parkObs$taxonRank == "hybrid"),] #went on ahead and removed hybrids
  parkObs$speciesFixed <- gsub("([A-z]+) ([A-z]+) ([A-z]+)",'\\1 \\2', parkObs$speciesFixed)
  parkObs$speciesFixed[which(parkObs$speciesFixed == "Agaricus xanthoderma 'xanthodermus']")] <- "Agaricus xanthodermus" #specific error in PORE observations
  
  names(parkObs)[which(names(parkObs)== "taxonID")] <- "inat.taxonID"
  
  #add category
  parkObs$category <- NA
  levels(parkObs$category) <- c("Mammal","Bird", "Herp", "Plant", "Fish",  "Invertebrate", "Fungi")
  
  parkObs$category[which(parkObs$kingdom == "Plantae")] <- "Plant"
  parkObs$category[which(parkObs$kingdom == "Fungi")] <- "Fungi"
  parkObs$category[which(parkObs$kingdom == "Animalia")] <- "Invertebrate"; parkObs$category[which(parkObs$phylum == "Chordata")] <- NA #tags all animals as invertebrates, then returns chordates to NA. Issues with chordate inverts to be dealt with in a few lines.
  parkObs$category[which(parkObs$class == "Aves")] <- "Bird"
  parkObs$category[which(parkObs$class == "Mammalia")] <- "Mammal"
  parkObs$category[which(parkObs$class == "Reptilia")] <- "Herp"
  parkObs$category[which(parkObs$class == "Amphibia")] <- "Herp"
  parkObs$category[which(parkObs$class == "Chondrichthyes")] <- "Fish" #cartelaginous fishes
  parkObs$category[which(parkObs$class == "Actinopterygii")] <- "Fish" #bony fishes
  parkObs$category[which(parkObs$class == "Myxini")] <- "Fish" #hagfishes
  parkObs$category[which(parkObs$class == "Hyperoartia")] <- "Fish" #lamprays
  parkObs$category[which(parkObs$class == "Sarcopterygii")] <- "Fish" #lobe-finned fish
  parkObs$category[which(parkObs$kingdom == "Chromista")] <- "Microbe" #will be dropped
  parkObs$category[which(parkObs$kingdom == "Archaea")] <- "Microbe"
  parkObs$category[which(parkObs$kingdom == "Bacteria")] <- "Microbe"
  parkObs$category[which(parkObs$kingdom == "Protozoa")] <- "Microbe"
  
  fix.idx <- which(is.na(parkObs$category))
  for (i in fix.idx){
    tax <- classification(parkObs$class[i], db = 'itis')
    if (tax[[1]]$name[which(tax[[1]]$rank == "phylum")] == "Chordata" && tax[[1]]$name[which(tax[[1]]$rank == "subphylum")] !="Vertebrata") {parkObs$category[i] <- "Invertebrate"}
  }
  
  #Write out the observation data
  allObs <- parkObs
  filePathFull = paste0("processed_data/",park_id,"_data/",park_id,"_obs_FULL.csv")
  write.csv(allObs,filePathFull,row.names = F)
  
  cleanObs <- parkObs[,c("decimalLongitude","decimalLatitude","category","family","speciesFixed","year","month","inat.taxonID")]
  filePathClean = paste0("processed_data/",park_id,"_data/",park_id,"_obs_tidy.csv")
  write.csv(cleanObs,filePathClean,row.names = F)
  
  return(parkObs)
}

#only runs function if the park data hasn't been processed (park_obs.csv is in gitignore)
if (file.exists(paste0("processed_data/",park_id,"_data/",park_id,"_obs_tidy.csv"))  == F | file.exists(paste0("processed_data/",park_id,"_data/",park_id,"_obs_FULL.csv"))  == F) {
  allParkObs <- read.csv("processed_data/park_obs.csv",header = T, stringsAsFactors = FALSE)
  parkObs <- parkObservations(park_id, allParkObs)
}

#### species stats #####
# if above function was run in a previous session, park obs can be retreived from file
if(exists("parkObs") == F) parkObs <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_obs_FULL.csv"), header = T)

inatStats <- function(parkObs)
{
  speciesFixed <- unique(parkObs$speciesFixed)
  inatSpecies <- data.frame(speciesFixed)
  
  inatSpecies$total_obs <- NA
  inatSpecies$user_obs <- NA
  inatSpecies[c("kingdom","phylum","class","order","family","genus","category","inat.taxonID")] <- NA
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
    inatSpecies[i,"inat.taxonID"] <- spObs[1,"inat.taxonID"]
    
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
  return(inatSpecies)
}

inatSpecies <- inatStats(parkObs)

#### comparing to NPS species list ####
  # species list downloaded from NPSpecies, converted to .csv by excel

NPSinatSpecies <- function(inatSpecies,park_id){
  parkSpeciesFilepath <- paste0("raw_data/NPSpecies_",park_id,".csv")
  parkSpeciesData <- read.csv(parkSpeciesFilepath,header =T, stringsAsFactors = FALSE)
  
  #select columns
  parkSpeciesFixed <- parkSpeciesData[,c("Taxon.Record.Status","Scientific.Name","Common.Names","Synonyms","Park.Accepted", "Record.Status","Occurrence","Category")]
  
  #fix names, remove hybrids and duplicates
  parkSpeciesFixed <- parkSpeciesFixed[-(grep("([A-z]+) X ([A-z]+)",parkSpeciesFixed$Scientific.Name)),] #removing hybrids
  parkSpeciesFixed$speciesFixed <- parkSpeciesFixed$Scientific.Name
  parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+) var.*",'\\1 \\2', parkSpeciesFixed$speciesFixed)
  parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+)-([A-z]+) var.*",'\\1 \\2-\\3', parkSpeciesFixed$speciesFixed) #hyphenated species names that are also hybrids (WHO PUTS HYPHENS IN SPECIES NAMES???)
  parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+) ssp.*",'\\1 \\2', parkSpeciesFixed$speciesFixed)
  parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+) ([A-z]+).*",'\\1 \\2', parkSpeciesFixed$speciesFixed)
  parkSpeciesFixed <- parkSpeciesFixed[!duplicated(parkSpeciesFixed$speciesFixed),] #removes duplicates due to subspecies/varieties
  
  #Change NPS categories
  parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Slug/Snail","Insect","Other Non-vertebrates","Crab/Lobster/Shrimp"))] <- "Invertebrate"
  parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Reptile","Amphibian"))] <- "Herp"
  parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Vascular Plant","Non-vascular Plant"))] <- "Plant"
  parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Chromista","Bacteria","Protozoa","Archaea"))] <- "Microbe"
  
  #merge dataframes
  totalSpecies <- merge(inatSpecies,parkSpeciesFixed,by = "speciesFixed", all = T)
  totalSpecies$speciesFixed <- as.character(totalSpecies$speciesFixed)
  totalSpecies <- totalSpecies[order(totalSpecies$speciesFixed),]  #makes alphabetical by species name
  
  #fix missing information
  category.idx <- which(is.na(totalSpecies$category))
  totalSpecies$category[category.idx] <- totalSpecies$Category[category.idx] #puts park species categories into same column as iNat
  
  #allObsSpecies <- read.csv("processed_data/iNatSpecies.csv", header = TRUE, stringsAsFactors = FALSE)
  need.tax <- which(is.na(totalSpecies$kingdom)); still.need.tax <- NULL; multiple.tax <- NULL
  for (s in need.tax){
    sp.name <- totalSpecies$speciesFixed[s]
    all.matches <- which(allObsSpecies$scientificName == sp.name)
    if (length(all.matches) > 1){multiple.tax <- c(multiple.tax, s)} #this might be of some interest
    if (length(all.matches) > 0){
      totalSpecies[s, c("kingdom","phylum","class","order","family","genus", "inat.taxonID")] <- allObsSpecies[all.matches[1],c("kingdom","phylum","class","order","family","genus", "taxonID")]
    }  else {(still.need.tax <- c(still.need.tax, s))} 
    print(paste0(100*which(need.tax == s)/length(need.tax), "% through iNat"))
  }
  
  #totalSpecies[still.need.tax, c("speciesFixed","total_obs","user_obs","kingdom","phylum","class","order","family","genus","category","inat.taxonID","Taxon.Record.Status","Scientific.Name","Common.Names","Synonyms","Park.Accepted")]
  
  #Using taxize
  ranks <- c("kingdom","phylum", "class","order","family","genus") #if you use itis as your db, they list plant divisions instead of phyla (in loop there's a line that's commented out if using gif)
  need.tax <- which(is.na(totalSpecies$kingdom)); still.need.tax <- NULL
  all.tax <- classification(totalSpecies$speciesFixed[need.tax],'gbif', rows = 1) #will always choose first result, that way it doesn't have to be babysat. On the other hand this could compeletly fuck up if there are conflicts with genus names.
  for (s in need.tax){
    sp.name <- totalSpecies$speciesFixed[s]
    
    if(dim(all.tax[[sp.name]])[1] < 5){still.need.tax <- c(still.need.tax, s)} else{ #if gbif didn't find the taxonomic information, it skips instead of throwing an error
      
      for (r in ranks){
        totalSpecies[[r]][s] <-  all.tax[[sp.name]]$name[which(all.tax[[sp.name]]$rank == r)]
      }
      #if (totalSpecies$kingdom[s] == "Plantae") {totalSpecies$phylum[s] <- all.tax[[sp.name]]$name[which(all.tax[[sp.name]]$rank == "division")]} else {totalSpecies$phylum[s] <- all.tax[[sp.name]]$name[which(all.tax[[sp.name]]$rank == "phylum")]} # needed if you use itis as the source
    }
    print(paste0(100*which(need.tax == s)/length(need.tax), "% through taxize"))
  }
  
  #remove unwanted columns and rows
  writeSpecies <- totalSpecies[-which(totalSpecies$Occurrence == "Not In Park" & is.na(totalSpecies$total_obs)),] #removes species everyone agrees are not present
  writeSpecies <- writeSpecies[,-which(names(totalSpecies) %in% c("Scientific.Name","Taxon.Record.Status","Synonyms","Park.Accepted","Record.Status"))]
  
  if (length(still.need.tax) > 0) {print(paste0(length(still.need.tax), " taxa still unaccounted for."))}
  
  speciesListPath <- paste0("processed_data/",park_id,"_data/",park_id,"_species.csv")
  write.csv(writeSpecies,speciesListPath,row.names = F)
  
  return(writeSpecies)
}

speciesList <- NPSinatSpecies(inatSpecies,park_id = park_id)

#### Common names for the taxonomic groups####

id.species <- speciesList[,c("speciesFixed", "kingdom","phylum","class","order","family","genus","inat.taxonID", "Common.Names")]
ranks <- c("kingdom","phylum", "class","order","family","genus", "species")

for (r in ranks){
  id.species[paste0(r,".common")] <- ""
  id.species[paste0(r,".science")] <- ""
}
id.species$species.ID <- ""
id.species$inat.iconic <- ""

#id.species <- id.species[1:50,] #testing

have.iNat.ID <- which(id.species$inat.taxonID != "")

for (s in have.iNat.ID)
{
  #inat_url <- paste0("https://www.inaturalist.org/taxa/", id.species$inat.taxonID[s], "#taxonomy-tab")
  inat_url <- paste0("https://www.inaturalist.org/taxa/", id.species$inat.taxonID[s])
  
  all.lines <- readLines(inat_url)
  #all.lines <- readLines("http://www.inaturalist.org/taxa/1-Animalia") #useful for testing
  
  tax.idx <- NULL
  for (i in 1:length(all.lines)){
    if (gregexpr("preferred_common_name",all.lines[i])[[1]][1] != -1) {tax.idx = c(tax.idx, i)}
  }
  
  tax.lines <- all.lines[tax.idx]
  n <- strsplit(tax.lines, "\\}\\,\\{\\\"observations_count")
  #retreived.names <- data.frame(scientificName = character(length(n[[1]])), commonName = character(length(n[[1]])), rank = character(length(n[[1]])), stringsAsFactors = FALSE)
  for (k in 1:length(n[[1]]))
  {
    taxon.line.dirty <- n[[1]][k]
    taxon.line <- gsub("\"","",taxon.line.dirty)
    rank <- sub(".*,rank:(.*?),.*", "\\1", taxon.line) ; rank
    if (rank %in% ranks){ #this way names are only retrieved for major groupings
    
      sci.name.clean <- sub(".*,name:(.*?),.*", "\\1", taxon.line)
      
      if (gregexpr("preferred_common_name",taxon.line)[[1]][1] != -1) {
        common.name <- sub(".*,preferred_common_name:(.*?)", "\\2", taxon.line)
        if (gregexpr("\\}|\\]",common.name)[[1]][1] != -1) {
          common.name <- gsub(".*,preferred_common_name:(.*?)\\}.*", "\\1", taxon.line)
        }
        if (gregexpr(",conservation_status:",common.name)[[1]][1] != -1) {
          common.name <- sub(".*,preferred_common_name:(.*?),conservation_status:.*", "\\1", taxon.line)
        }
      } else common.name <- ""
      
    if (rank == "species")
    {
      id.species[s, "species.ID"] <- sub(".*,id:(.*?),.*", "\\1", taxon.line)
      id.species[s, "inat.iconic"] <- sub(".*,iconic_taxon_name:(.*?),.*", "\\1", taxon.line)
    }
    
    id.species[s, paste0(rank, ".common")] <- common.name
    id.species[s, paste0(rank, ".science")] <- sci.name.clean
    } 
  }
  print(paste0(100*which(have.iNat.ID == s)/length(have.iNat.ID), "% done"))
}
save.id.species <- id.species


####try again####

id.species <- speciesList[,c("speciesFixed", "kingdom","phylum","class","order","family","genus","inat.taxonID", "Common.Names")]
ranks <- c("kingdom","phylum", "class","order","family","genus", "species")

for (r in ranks){
  id.species[paste0(r,".common")] <- ""
  id.species[paste0(r,".science")] <- ""
}
id.species$species.ID <- ""
id.species$inat.iconic <- ""

id.species <- id.species[1:50,] #testing

have.iNat.ID <- which(id.species$inat.taxonID != "")
check.iNat.ID <- NULL
for (s in have.iNat.ID)
{
  #inat_url <- paste0("https://www.inaturalist.org/taxa/", id.species$inat.taxonID[s], "#taxonomy-tab")
  inat_url <- paste0("https://www.inaturalist.org/taxa/", id.species$inat.taxonID[s])
  
  all.lines <- readLines(inat_url)
  #all.lines <- readLines("http://www.inaturalist.org/taxa/1-Animalia") #useful for testing
  
  tax.idx <- NULL
  for (i in 1:length(all.lines)){
    if (gregexpr("preferred_common_name",all.lines[i])[[1]][1] != -1) {tax.idx = c(tax.idx, i)}
  }
  
  tax.lines <- all.lines[tax.idx]
  n <- strsplit(tax.lines, "\\}\\,\\{\\\"observations_count")
  #retreived.names <- data.frame(scientificName = character(length(n[[1]])), commonName = character(length(n[[1]])), rank = character(length(n[[1]])), stringsAsFactors = FALSE)
  for (k in 1:length(n[[1]]))
  {
    taxon.line <- n[[1]][k]
    rank <- sub(".*\"rank\":\"(.*?)\".*", "\\1", taxon.line) ; rank
    if (rank %in% ranks){ #this way names are only retrieved for major groupings
      
      if (gregexpr("preferred_common_name",taxon.line)[[1]][1] != -1) {
        common.name <- sub(".*\"preferred_common_name\":\"(.*?)\".*", "\\1", taxon.line)
      } else {common.name <- ""}
      
      sci.name <- sub(".*\"name\":\"(.*?)\".*", "\\1", taxon.line)
      
      id.species[s, paste0(rank, ".common")] <- common.name
      id.species[s, paste0(rank, ".science")] <- sci.name
      
      if (rank == "species")
      {
        id.species[s, "species.ID"] <- sub(".*\"id\":(.*?),.*", "\\1", taxon.line)
        id.species[s, "inat.iconic"] <- sub(".*\"iconic_taxon_name\":\"(.*?)\".*", "\\1", taxon.line)
      }
    } 
  }
  print(paste0(100*which(have.iNat.ID == s)/length(have.iNat.ID), "% done"))
}



