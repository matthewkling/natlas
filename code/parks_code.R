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
cleanParkObservations = function(park_id,allParkObs){
  parkObs <- allParkObs[which(allParkObs$PARK_CODE == park_id),]
  
  #add dates
  parkObs$eventDate <- as.Date(parkObs$eventDate)
  parkObs$dateIdentified <- as.Date(parkObs$dateIdentified)
  
  #correct species names
  parkObs$speciesFixed <- parkObs$scientificName
  parkObs$speciesFixed[which(parkObs$speciesFixed == "Agaricus xanthoderma [as 'xanthodermus']")] <- "Agaricus xanthodermus"
  parkObs$speciesFixed[which(parkObs$taxonRank == "hybrid")] <- NA #set their names to NA
  parkObs <- parkObs[-which(parkObs$taxonRank == "hybrid"),] #went on ahead and removed hybrids
  parkObs$speciesFixed <- gsub("([A-z]+) ([A-z]+) ([A-z]+)",'\\1 \\2', parkObs$speciesFixed)

  names(parkObs)[which(names(parkObs)== "taxonID")] <- "original.inat.taxonID"
  
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
  
  cleanObs <- parkObs[,c("decimalLongitude","decimalLatitude","category","kingdom","phylum","class","order","family","genus","speciesFixed","year","month","original.inat.taxonID")]
  filePathClean = paste0("processed_data/",park_id,"_data/",park_id,"_obs_tidy.csv")
  write.csv(cleanObs,filePathClean,row.names = F)
  
  return(parkObs)
  #return(cleanObs)
}

#only runs function if the park data hasn't been processed (park_obs.csv is in gitignore)
if (file.exists(paste0("processed_data/",park_id,"_data/",park_id,"_obs_tidy.csv"))  == F | file.exists(paste0("processed_data/",park_id,"_data/",park_id,"_obs_FULL.csv"))  == F) {
  allParkObs <- read.csv("processed_data/park_obs.csv",header = T, stringsAsFactors = FALSE)
  parkObs <- cleanParkObservations(park_id, allParkObs)
}

#### species stats #####
# Should change this to taxon stats
# if above function was run in a previous session, park obs can be retreived from file
if(exists("parkObs") == F) parkObs <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_obs_FULL.csv"), header = T, stringsAsFactors =  FALSE)

inatStats <- function(parkObs)
{
  speciesFixed <- unique(parkObs$speciesFixed)
  inatSpecies <- data.frame(speciesFixed)
  
  inatSpecies$total_obs <- NA
  inatSpecies$user_obs <- NA
  inatSpecies[c("kingdom","phylum","class","order","family","genus","category","original.inat.taxonID")] <- NA
  inatSpecies[c(paste("m",1:12,sep=""))]<- NA
  inatSpecies[c(paste("y",2007:max(parkObs$year),sep=""))] <- NA
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
    inatSpecies[i,"original.inat.taxonID"] <- spObs[1,"original.inat.taxonID"]
    
    for (j in 1:12)
    {
      inatSpecies[i,paste("m",j,sep="")] <- length(which(spObs$month == j))
    }
    
    for (k in 2007:max(parkObs$year))
    {
      inatSpecies[i,paste("y",k,sep="")] <- length(which(spObs$year == k))
    }
    
    print(paste0(100*i/numSp, "% species stats calculated") )
  }
  return(inatSpecies)
}

inatSpecies <- inatStats(parkObs[1:500,])


####Functions for finding information####
compareWithObservations <- function(workingSpeciesList, column_to_match, column_to_create_index){
  totalSpecies <- workingSpeciesList
  
  #Use scientific name to find matches in iNat observed species
  allObsSpecies <- read.csv("processed_data/iNatSpecies.csv", header = TRUE, stringsAsFactors = FALSE)
  need.tax <- which(is.na(totalSpecies[,column_to_create_index])); still.need.tax <- NULL; multiple.tax <- NULL
  for (s in need.tax){
    sp.name <- totalSpecies[s, column_to_match]
    all.matches <- which(allObsSpecies$scientificName == sp.name)
    if (length(all.matches) > 1){multiple.tax <- c(multiple.tax, s)} #this might be of some interest
    if (length(all.matches) > 0){
      totalSpecies[s, c("kingdom","phylum","class","order","family","genus", "matched.inat.ID")] <- allObsSpecies[all.matches[1],c("kingdom","phylum","class","order","family","genus", "taxonID")]
    }  else {(still.need.tax <- c(still.need.tax, s))} 
    print(paste0(100*which(need.tax == s)/length(need.tax), "% through matching iNat observations"))
  }
  print(paste0((length(need.tax)-length(still.need.tax)), " records matched to ", column_to_match, " column, ", length(still.need.tax), " records still needed, ", length(multiple.tax), " returned multiple records."))
  return(totalSpecies)
}

taxizeFindTaxonomy <- function(workingSpeciesList, column_to_match, column_to_create_index, taxize_database = "gbif"){
  totalSpecies <- workingSpeciesList
  #Using taxize to find taxonomic heirarchy for species not in the iNat observations
  #if you use itis as your db, they list plant divisions instead of phyla (in loop there's a line that's commented out if using gif)
  totalSpecies$species <- ""
  need.tax <- which(is.na(totalSpecies[,column_to_create_index])); still.need.tax <- NULL
  all.tax <- classification(totalSpecies[need.tax, column_to_match],db = taxize_database, rows = 1) #will always choose first result, that way it doesn't have to be babysat. On the other hand this could compeletly fuck up if there are conflicts with genus names or something.
  for (s in need.tax){
    sp.name <- totalSpecies[s, column_to_match]
    if(is.null(all.tax[[sp.name]])[1]){still.need.tax <- c(still.need.tax, s)} else{ #if gbif didn't find the taxonomic information, it skips instead of throwing an error
      for (r in ranks_to_species){
        if (length(which(all.tax[[sp.name]]$rank == r)) > 0) #if taxonomy is matched, fills in the names
        {totalSpecies[[r]][s] <-  all.tax[[sp.name]]$name[which(all.tax[[sp.name]]$rank == r)]}
        
        if (length(which(all.tax[[sp.name]]$rank == "genus")) > 0 # if there's a genus name but no species name, makes the name from taxize the genus (park list includes some genus-only) 
            #&& length(which(all.tax[[sp.name]]$rank == "species")) == 0 #checks there's no species listed
                            && length(grep(" ", sp.name)) == 0) #checks that the name being matched doesn't have a space/is actually supposed to be a genus (in case some species returned the genus but no species entry)
        {totalSpecies[["species"]][s] <-  all.tax[[sp.name]]$name[which(all.tax[[sp.name]]$rank == "genus")]}
      }
      #if (totalSpecies$kingdom[s] == "Plantae") {totalSpecies$phylum[s] <- all.tax[[sp.name]]$name[which(all.tax[[sp.name]]$rank == "division")]} else {totalSpecies$phylum[s] <- all.tax[[sp.name]]$name[which(all.tax[[sp.name]]$rank == "phylum")]} # needed if you use itis as the source
    }
    print(paste0(100*which(need.tax == s)/length(need.tax), "% through taxize taxonomy"))
  }
  print(paste0((length(need.tax)-length(still.need.tax)), " taxonomies found using taxize database ",taxize_database,", ", length(still.need.tax), " records not found"))
  
  names(totalSpecies)[which(names(totalSpecies)=="species")] <- "taxizeName"
  
  
  return(totalSpecies)
}

scrapeiNat <- function(workingSpeciesList, primary_column_to_match, secondary_column_to_match,column_to_create_index, column_for_retreived_IDs, consensus_name_column)
{
  speciesList <- workingSpeciesList
  
  need.id.idx <- which(is.na(speciesList[, column_to_create_index])); still.need.id <- NULL
  
  for (i in need.id.idx){
    tax.name <- speciesList[i, primary_column_to_match]
    search.term <- gsub(" ", "+", tax.name)
    search.results <- readLines(paste0("https://www.inaturalist.org/taxa/search?utf8=%E2%9C%93&q=",search.term))
    found.term <- gsub(" ", "-", tax.name)
    search.to.find <- paste0("<a href=\"/taxa/([0-9]*)-", found.term, "\">")

    alternative.name <- speciesList[i, secondary_column_to_match]
    alternative.find <- gsub(" ", "-", alternative.name)
    alternative.search.find <- paste0("<a href=\"/taxa/([0-9]*)-", alternative.find, "\">")

    any.search.result <- "<a href=\"/taxa/([0-9]*)-"
    
    results.idx = NULL
    for (l in 1:length(search.results)){
      if(gregexpr(search.to.find,search.results[l])[[1]][1] != -1 | gregexpr(alternative.search.find,search.results[l])[[1]][1] != -1) {results.idx = c(results.idx, l)}
    }
    results.idx
    
    if (is.null(results.idx) == FALSE){
      results <- search.results[results.idx[1]]
      id.number <- gsub(".*<a href=\"/taxa/([0-9]*)-.*", "\\1", results)
      speciesList[i, column_for_retreived_IDs] <- id.number
      
      iNat.name.dirty <- gsub(".*<a href=\"/taxa/([0-9]*)-(.*)\"><.*", "\\2", results)
      iNat.name <- sub("-", " ", iNat.name.dirty)
      #speciesList$scrapediNatName[i] <- iNat.name
      
    } else still.need.id <- c(still.need.id, i)
    print(paste0(100*which(need.id.idx == i)/length(need.id.idx), "% through retreiving iNat ID numbers using NPS names"))
    
  }
  print(paste0((length(need.id.idx)-length(still.need.id)), " iNat IDs matched on NPS name, ", length(still.need.id), " not found"))
  
  # id.not.found <- NULL
  # for (j in still.need.id){
  #   tax.name <- speciesList[j, secondary_column_to_match]
  #   search.term <- gsub(" ", "+", tax.name)
  #   search.results <- readLines(paste0("https://www.inaturalist.org/taxa/search?utf8=%E2%9C%93&q=",search.term))
  #   found.term <- gsub(" ", "-", tax.name)
  #   search.to.find <- paste0("<a href=\"/taxa/([0-9]*)-", found.term, "\">")
  #   
  #   alternative.name <- speciesList[j, primary_column_to_match]
  #   alternative.find <- gsub(" ", "-", alternative.name)
  #   alternative.search.find <- paste0("<a href=\"/taxa/([0-9]*)-", alternative.find, "\">")
  #   
  #   results.idx = NULL
  #   for (l in 1:length(search.results)){
  #     if(gregexpr(search.to.find,search.results[l])[[1]][1] != -1 | gregexpr(alternative.search.find,search.results[l])[[1]][1] != -1) {results.idx = c(results.idx, l)}
  #   }
  #   results.idx
  #   
  #   if (is.null(results.idx) == FALSE){
  #     results <- search.results[results.idx[1]]
  #     id.number <- gsub(".*<a href=\"/taxa/([0-9]*)-.*", "\\1", results)
  #     speciesList[j, column_for_retreived_IDs] <- id.number
  #     
  #     iNat.name.dirty <- gsub(".*<a href=\"/taxa/([0-9]*)-(.*)\"><.*", "\\2", results)
  #     iNat.name <- sub("-", " ", iNat.name.dirty)
  #     speciesList$scrapediNatName[j] <- iNat.name
  #     
  #   } else id.not.found <- c(id.not.found, j)
  #   
  #   print(paste0(100*which(still.need.id == j)/length(still.need.id), "% through retreiving iNat ID numbers using taxize"))
  # }
  # 
  # # if (is.null(results.idx)){ #if using taxize name doesn't work
  # #   possible.result.idx = NULL
  # #   for (l in 1:length(search.results)){
  # #     if(gregexpr(any.search.result,search.results[l])[[1]][1] != -1) {possible.result.idx = c(possible.result.idx, l)}
  # #   }
  # #   possible.result.idx
  # #   
  # #   possible.result <- search.results[possible.result.idx]
  # #   possible.tax.link <- gsub(".*<a href=\"(.*)\">.*", "\\1", possible.result)
  # #   check.URL <- paste0("https://www.inaturalist.org", possible.tax.link) 
  # #   check.page <- readLines(check.URL)
  # #   
  # #   alt.name.idx = NULL
  # #   for (l in 1:length(check.page)){
  # #     if(gregexpr("Scientific Names",check.page[l])[[1]][1] != -1) {alt.name.idx = c(alt.name.idx, l)}
  # #   }
  # #   alt.name.idx
  # #   
  # # }
  # 
  print(paste0((length(need.id.idx)-length(still.need.id)), " iNat IDs matched on NPS name, "))#,(length(still.need.id)-length(id.not.found)), " iNat IDs matched on taxize name, ", length(id.not.found), " not found"))
  
  return(speciesList)
}


#### comparing to NPS species list ####
  # species list downloaded from NPSpecies, converted to .csv by excel
ranks_to_genus <- ranks <- c("kingdom","phylum", "class","order","family","genus")
ranks_to_species <- c("kingdom","phylum", "class","order","family","genus", "species")

NPScleanup <- function(park_id){
  # read in NPS species list
  parkSpeciesFilepath <- paste0("raw_data/NPSpecies_",park_id,".csv")
  parkSpeciesData <- read.csv(parkSpeciesFilepath,header =T, stringsAsFactors = FALSE)
  
  # select columns
  parkSpeciesFixed <- parkSpeciesData[,c("Taxon.Record.Status","Scientific.Name","Common.Names","Synonyms","Park.Accepted", "Record.Status","Occurrence","Category")]

  # fix names, remove hybrids and duplicates
  parkSpeciesFixed <- parkSpeciesFixed[-(grep("([A-z]+) X ([A-z]+)",parkSpeciesFixed$Scientific.Name)),] #removing hybrids
  parkSpeciesFixed$speciesTidy <- parkSpeciesFixed$Scientific.Name
  parkSpeciesFixed$speciesTidy <- gsub("([A-z]+) ([A-z]+) var.*",'\\1 \\2', parkSpeciesFixed$speciesTidy)
  parkSpeciesFixed$speciesTidy <- gsub("([A-z]+) ([A-z]+)-([A-z]+) var.*",'\\1 \\2-\\3', parkSpeciesFixed$speciesTidy) #hyphenated species names that are also hybrids (WHO PUTS HYPHENS IN SPECIES NAMES??? Too many people it turns out.)
  parkSpeciesFixed$speciesTidy <- gsub("([A-z]+) ([A-z]+) ssp.*",'\\1 \\2', parkSpeciesFixed$speciesTidy)
  parkSpeciesFixed$speciesTidy <- gsub("([A-z]+) ([A-z]+) ([A-z]+).*",'\\1 \\2', parkSpeciesFixed$speciesTidy)
  parkSpeciesFixed <- parkSpeciesFixed[!duplicated(parkSpeciesFixed$speciesTidy),] #removes duplicates due to subspecies/varieties

  parkSpeciesFixed <- parkSpeciesFixed[order(parkSpeciesFixed$speciesTidy),]  #makes alphabetical by name
  
  # Change NPS categories to match our categories
  parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Slug/Snail","Insect","Other Non-vertebrates","Crab/Lobster/Shrimp"))] <- "Invertebrate"
  parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Reptile","Amphibian"))] <- "Herp"
  parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Vascular Plant","Non-vascular Plant"))] <- "Plant"
  parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Chromista","Bacteria","Protozoa","Archaea"))] <- "Microbe"

  # Compare against all iNat observation
  parkSpeciesFixed[,ranks_to_genus] <- NA
  parkSpeciesFixed$matched.inat.ID <-NA
  
  parkSpeciesFixed <- compareWithObservations(parkSpeciesFixed, "speciesTidy", column_to_create_index = "kingdom")
  parkSpeciesFixed <- taxizeFindTaxonomy(parkSpeciesFixed, "speciesTidy", column_to_create_index = "kingdom")
  parkSpeciesFixed <- compareWithObservations(parkSpeciesFixed, "taxizeName", column_to_create_index = "matched.inat.ID")
  parkSpeciesFixed <- scrapeiNat(parkSpeciesFixed, primary_column_to_match = "speciesTidy", secondary_column_to_match = "taxizeName", column_to_create_index = "matched.inat.ID", column_for_retreived_IDs = "matched.inat.ID", consensus_name_column = "resolvedNames")
  parkSpeciesFixed <- taxizeFindTaxonomy(parkSpeciesFixed, column_to_match =  "speciesTidy", column_to_create_index = "matched.inat.ID", taxize_database = "ncbi")
  
  
  parkSpeciesFixed$resolvedNames <- parkSpeciesFixed$speciesTidy
  taxize.idx <- which(parkSpeciesFixed$taxizeName != "")
  parkSpeciesFixed$resolvedNames[taxize.idx] <- parkSpeciesFixed$taxizeName[taxize.idx]
  
  return(parkSpeciesFixed)
}

parkList <- NPScleanup(park_id = park_id)


#####




####Complete find information loop####
findMissingInformation <- function(rawSpeciesList)
{
  workingSpeciesList <- rawSpeciesList
  
  workingSpeciesList <- compareWithObservations(workingSpeciesList)
  workingSpeciesList <- taxizeFindTaxonomy(workingSpeciesList)
  
  workingSpeciesList <- scrapeiNatIDs(workingSpeciesList)
  
  #remove unwanted columns and rows
  writeSpecies <- workingSpeciesList[-which(workingSpeciesList$Occurrence == "Not In Park" & is.na(workingSpeciesList$total_obs)),] #removes species everyone agrees are not present
  writeSpecies <- writeSpecies[,-which(names(workingSpeciesList) %in% c("Scientific.Name","Taxon.Record.Status","Synonyms","Park.Accepted","Record.Status"))]
  
  #if (length(still.need.tax) > 0) {print(paste0(length(still.need.tax), " taxa still unaccounted for."))}
  
  speciesListPath <- paste0("processed_data/",park_id,"_data/",park_id,"_species.csv")
  #write.csv(writeSpecies,speciesListPath,row.names = F)
  
  return(writeSpecies)
}

workingSpeciesList <- findMissingInformation(rawSpeciesList)

####Find common names functions####
findUsingiNat <- function(id.species, ranks)
{
  have.iNat.ID <- which(id.species$inat.taxonID != "")
  
  for (s in have.iNat.ID)
  {
    inat_url <- paste0("https://www.inaturalist.org/taxa/", id.species$inat.taxonID[s])
    all.lines <- readLines(inat_url)
    
    tax.idx <- NULL
    for (i in 1:length(all.lines)){ #checks that there is a preferred common name
      if (gregexpr("preferred_common_name",all.lines[i])[[1]][1] != -1) {tax.idx = c(tax.idx, i)}
    }
    
    tax.lines <- all.lines[tax.idx]
    n <- strsplit(tax.lines, "\\}\\,\\{\\\"observations_count") #breaks up taxonomy into components
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
          if (gregexpr(",([A-z]*):",common.name)[[1]][1] != -1) {
            common.name <- sub(".*,preferred_common_name:(.*?),([A-z]*):.*", "\\1", taxon.line)
          }
          # if (gregexpr(",conservation_status:",common.name)[[1]][1] != -1) {
          #   common.name <- sub(".*,preferred_common_name:(.*?),conservation_status:.*", "\\1", taxon.line)
          # }
          # if (gregexpr(",ancestors:",common.name)[[1]][1] != -1) {
          #   common.name <- sub(".*,ancestors:(.*?),conservation_status:.*", "\\1", taxon.line)
          # }
        }
        
        if (rank == "species")
        {
          id.species[s, "species.ID"] <- sub(".*,id:(.*?),.*", "\\1", taxon.line)
          id.species[s, "inat.iconic"] <- sub(".*,iconic_taxon_name:(.*?),.*", "\\1", taxon.line)
          id.species[s, "source"] <- "iNat"
        }
        
        id.species[s, paste0(rank, ".common")] <- common.name
        id.species[s, paste0(rank, ".science")] <- sci.name.clean
      } 
    }
    print(paste0(100*which(have.iNat.ID == s)/length(have.iNat.ID), "% done finding common names with iNat IDs"))
  }
  return(id.species)
}



####Find common names loop####

findCommonNames <- function(compiledSpeciesList, ranks = c("kingdom","phylum", "class","order","family","genus", "species")){
  id.species <- compiledSpeciesList[,c("speciesFixed", "kingdom","phylum","class","order","family","genus","species","inat.taxonID", "Common.Names")]

  for (r in ranks){
    id.species[paste0(r,".common")] <- ""
    id.species[paste0(r,".science")] <- ""
  }
  id.species$species.ID <- ""
  id.species$inat.iconic <- ""
  id.species$name.source <- ""

  
  id.species.working <- findUsingiNat(id.species = id.species, ranks = ranks)
  
  return(id.species.working)
}

speciesNamed <- findCommonNames(compiledSpeciesList = workingSpeciesList)

# ####Stats by level above species####
# inatStats <- function(observationData, rankToFindStats, ranks = c("kingdom","phylum","class","order","family","genus", "species"))
# {
#   unique.taxa <- unique(observationData[rankToFindStats])
#   inatTaxa <- data.frame(unique.taxa, stringsAsFactors = FALSE)
#   
#   ranks.to.use <- ranks[1:which(ranks == rankToFindStats)]
#   
#   inatTaxa$total_obs <- NA
#   inatTaxa$user_obs <- NA
#   
#   inatTaxa[c(ranks.to.use,"category","inat.taxonID")] <- NA
#   inatTaxa[rankToFindStats] <- unique.taxa #gets messed up by above line
#   inatTaxa[c(paste("m",1:12,sep=""))]<- NA
#   inatTaxa[c(paste("y",2007:max(observationData$year),sep=""))] <- NA
#   numTaxa <- length(inatTaxa[,rankToFindStats])
#   
#   
#   for (i in 1:numTaxa){
#     obs.of.taxon <- observationData[which(observationData[rankToFindStats] == inatTaxa[i,rankToFindStats]),]
#     
#     #total/user observations
#     inatTaxa$total_obs[i] = length(obs.of.taxon$speciesFixed)
#     inatTaxa$user_obs[i] = length(unique(obs.of.taxon$recordedBy))
#     
#     #build hierarchy
#     for (r in ranks.to.use)
#     {
#       inatTaxa[i,r] <- obs.of.taxon[1,r]
#     }
#     inatTaxa[i,"category"] <- obs.of.taxon[1,"category"]
#     
#     #Find the iNat taxon id and common name: better done in a different loop?
#     
#     #stats for times
#     for (j in 1:12)
#     {
#       inatTaxa[i,paste("m",j,sep="")] <- length(which(obs.of.taxon$month == j))
#     }
#     
#     for (k in 2007:max(observationData$year))
#     {
#       inatTaxa[i,paste("y",k,sep="")] <- length(which(obs.of.taxon$year == k))
#     }
#     
#     print(paste0(100*i/numTaxa, "% ", rankToFindStats," stats calculated") )
#   }
#   return(inatTaxa)
# }
# 
# # for (taxon.level in (ranksAboveSpecies = ranks = c("kingdom","phylum","class","order","family","genus"))){
# # paste0(taxon.level,"iNatStats") <- inatStats(observationData, rankToFindStats = taxon.level) #paste0 doesn't work here for naming the output- maybe I'll have to make then fill a list of data frames
# #   
# # }


#####Mergeing clean lists###
# #original function
# NPSinatSpeciesMerge <- function(inatSpecies,park_id, cleanParkSpecies){
#   parkSpeciesFilepath <- paste0("raw_data/NPSpecies_",park_id,".csv")
#   parkSpeciesData <- read.csv(parkSpeciesFilepath,header =T, stringsAsFactors = FALSE)
#   
#   #select columns
#   parkSpeciesFixed <- parkSpeciesData[,c("Taxon.Record.Status","Scientific.Name","Common.Names","Synonyms","Park.Accepted", "Record.Status","Occurrence","Category")]
#   
#   #fix names, remove hybrids and duplicates
#   parkSpeciesFixed <- parkSpeciesFixed[-(grep("([A-z]+) X ([A-z]+)",parkSpeciesFixed$Scientific.Name)),] #removing hybrids
#   parkSpeciesFixed$speciesFixed <- parkSpeciesFixed$Scientific.Name
#   parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+) var.*",'\\1 \\2', parkSpeciesFixed$speciesFixed)
#   parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+)-([A-z]+) var.*",'\\1 \\2-\\3', parkSpeciesFixed$speciesFixed) #hyphenated species names that are also hybrids (WHO PUTS HYPHENS IN SPECIES NAMES??? Too many people it turns out.)
#   parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+) ssp.*",'\\1 \\2', parkSpeciesFixed$speciesFixed)
#   parkSpeciesFixed$speciesFixed <- gsub("([A-z]+) ([A-z]+) ([A-z]+).*",'\\1 \\2', parkSpeciesFixed$speciesFixed)
#   parkSpeciesFixed <- parkSpeciesFixed[!duplicated(parkSpeciesFixed$speciesFixed),] #removes duplicates due to subspecies/varieties
#   
#   #Change NPS categories
#   parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Slug/Snail","Insect","Other Non-vertebrates","Crab/Lobster/Shrimp"))] <- "Invertebrate"
#   parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Reptile","Amphibian"))] <- "Herp"
#   parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Vascular Plant","Non-vascular Plant"))] <- "Plant"
#   parkSpeciesFixed$Category[which(parkSpeciesFixed$Category %in% c("Chromista","Bacteria","Protozoa","Archaea"))] <- "Microbe"
#   
#   # #merge dataframes
#   # totalSpecies <- merge(inatSpecies,parkSpeciesFixed,by = "speciesFixed", all = T)
#   # totalSpecies$speciesFixed <- as.character(totalSpecies$speciesFixed)
#   # totalSpecies <- totalSpecies[order(totalSpecies$speciesFixed),]  #makes alphabetical by species name
#   # 
#   # category.idx <- which(is.na(totalSpecies$category))
#   # totalSpecies$category[category.idx] <- totalSpecies$Category[category.idx] #puts park species categories into same column as iNat
#   # 
#   # return(totalSpecies)
# }
# 
# rawSpeciesList <- NPSinatSpeciesMerge(inatSpecies,park_id = park_id)
