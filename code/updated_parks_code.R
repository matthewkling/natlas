library(dplyr)

####Functions####
makeParkShapefile <-function(park_id){
  allParkShape <- readOGR("raw_data/park_boundaries","nps_boundary")
  parkShape <- allParkShape[which(allParkShape$UNIT_CODE == park_id),]
  if(dir.exists(paste0("processed_data/",park_id,"_data/")) == FALSE) dir.create(paste0("processed_data/",park_id,"_data/"))
  shapePath <- paste0("processed_data/",park_id,"_data/",park_id,"_shape")
  writeOGR(obj = parkShape,dsn = shapePath, layer = park_id ,driver = "ESRI Shapefile") #there's a carto driver that wasn't working for me, unsure what makes a carto shapefile different.
}

addCategories <- function(species.data,kingdom.column = "kingdom", class.column = "class"){
  species.data$category <- NA
  levels(species.data$category) <- c("Mammal","Bird", "Herp", "Plant", "Fish",  "Invertebrate", "Fungi")
  
  species.data$category[which(species.data[,kingdom.column] == "Plantae")] <- "Plant"
  species.data$category[which(species.data[,kingdom.column] == "Fungi")] <- "Fungi"
  species.data$category[which(species.data[,kingdom.column] == "Animalia")] <- "Invertebrate"; species.data$category[which(species.data$phylum == "Chordata")] <- NA #tags all animals as invertebrates, then returns chordates to NA. Issues with chordate inverts to be dealt with in a few lines- they aren't left as NAs.
  
  species.data$category[which(species.data[,class.column] == "Aves")] <- "Bird"
  species.data$category[which(species.data[,class.column] == "Mammalia")] <- "Mammal"
  species.data$category[which(species.data[,class.column] == "Reptilia")] <- "Herp"
  species.data$category[which(species.data[,class.column] == "Amphibia")] <- "Herp"
  species.data$category[which(species.data[,class.column] == "Chondrichthyes")] <- "Fish" #cartelaginous fishes
  species.data$category[which(species.data[,class.column] == "Elasmobranchii")] <- "Fish" #cartelaginous fishes
  species.data$category[which(species.data[,class.column] == "Actinopterygii")] <- "Fish" #bony fishes
  species.data$category[which(species.data[,class.column] == "Myxini")] <- "Fish" #hagfishes
  species.data$category[which(species.data[,class.column] == "Hyperoartia")] <- "Fish" #lamprays
  species.data$category[which(species.data[,class.column] == "Sarcopterygii")] <- "Fish" #lobe-finned fish
  species.data$category[which(species.data[,kingdom.column] == "Chromista")] <- "Plants" #this includes the kelps, so plants made marginally more sense than microbes?
  species.data$category[which(species.data[,kingdom.column] == "Archaea")] <- "Microbe"
  species.data$category[which(species.data[,kingdom.column] == "Bacteria")] <- "Microbe"
  species.data$category[which(species.data[,kingdom.column] == "Protozoa")] <- "Microbe"
  
  species.data$category[which(species.data[,class.column] == "Ascidiacea")] <- "Invertebrate"
  species.data$category[which(species.data[,class.column] == "Thaliacea")] <- "Invertebrate"
  
  fix.idx <- which(is.na(species.data$category))
  for (to.fix in unique(species.data[,class.column][fix.idx])){
    fix.loc <- which(species.data[,class.column] == to.fix)
    tax <- classification(to.fix, db = 'itis')
    if (tax[[1]]$name[which(tax[[1]]$rank == "phylum")] == "Chordata" && tax[[1]]$name[which(tax[[1]]$rank == "subphylum")] !="Vertebrata") {
      species.data$category[fix.loc] <- "Invertebrate"
    }
  }
  fix.idx <- which(is.na(species.data$category))
  
  if (length(fix.idx) >1) print(paste0(length(fix.idx), " entries do not have categories!"))
  
  return(species.data)
}

matchToiNatParkList <- function(parkData,ranks_to_match){
  #read in list that already exists
  extant.lists <- list.files(path = "processed_data",pattern = "(.*)_all_parks_inat_species_list.csv")
  most.recent.file <- extant.lists[which.max(gsub("_all_parks_inat_species_list.csv","", extant.lists))]
  species.list <- read.csv(paste0("processed_data/",most.recent.file), stringsAsFactors = FALSE)
  
  wanted.columns <- c(paste0(ranks_to_match, ".inat.ID"),
                      paste0(ranks_to_match, ".science"),
                      paste0(ranks_to_match, ".common"))
  
  parkData[,wanted.columns] <- ""
  
  for(idx in 1:length(parkData$original.inat.taxonID))
  {
    species.row <- which(species.list$taxonID == parkData$original.inat.taxonID[idx])
    if(length(species.row) > 0){
      parkData[idx,wanted.columns] <- species.list[species.row[1],wanted.columns]
    }
    if (length(species.row) == 0) print(paste0("There's a problem with row ", idx))
  }
  return(parkData)
}

inatStatsByPark <- function(matchedParkObs)
{
  inatSpecies <- distinct(matchedParkObs, species.science ,genus.science, .keep_all = TRUE)
  inatSpecies <- inatSpecies[,-which(names(inatSpecies) %in% c("decimalLongitude","decimalLatitude","id","recordedBy","userNumber" ,"eventDate","eventTime","dateIdentified","verbatimLocality","coordinateUncertaintyInMeters","informationWithheld","identificationID","year","month","hour", "PARK_CODE","PARK_NAME","PARK_TYPE","IN_PARK"))]
  
  inatSpecies$total_obs <- NA
  inatSpecies$user_obs <- NA
  inatSpecies[c(paste("m",1:12,sep=""))]<- NA
  inatSpecies[c(paste("y",2007:max(matchedParkObs$year),sep=""))] <- NA
  numSp <- length(inatSpecies$speciesFixed)
  
  for (i in 1:numSp){
    spObs <- matchedParkObs[which(matchedParkObs$species.science == inatSpecies$species.science[i] & matchedParkObs$genus.science == inatSpecies$genus.science[i]),]
    inatSpecies$total_obs[i] = length(spObs$speciesFixed)
    inatSpecies$user_obs[i] = length(unique(spObs$userNumber))
    
    for (j in 1:12)
    {
      inatSpecies[i,paste("m",j,sep="")] <- length(which(spObs$month == j))
    }
    
    for (k in 2007:max(matchedParkObs$year))
    {
      inatSpecies[i,paste("y",k,sep="")] <- length(which(spObs$year == k))
    }
    if (i %% 100 == 1 | i == length(inatSpecies$total_obs)) print(paste0(floor(100*i/numSp), "% species stats calculated") )
  }
  return(inatSpecies)
}

matchNPSpecies <- function(parkStats, park_id,ranks_to_match = ranks_to_species){
  NPSpecies <-  read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_clean_NPSpecies.csv"), header = TRUE, stringsAsFactors = FALSE)
  NPSpecies <- NPSpecies[-which(NPSpecies$other.rank == "hybrid"),]
  
  wanted.columns <- c(paste0(ranks_to_match, ".inat.ID"),
                      paste0(ranks_to_match, ".science"),
                      paste0(ranks_to_match, ".common"),
                      "category")
  
  NPSpecies$category <- as.character(NPSpecies$category)
  parkStats$category <- as.character(parkStats$category)
  
  #corrections for all 
  parkStats$phylum.common[which(parkStats$phylum.common == "Molluscs")] <- "Mollusks"
  
  
  #specific corrections
  if(park_id == "PORE"){
    # NPSpecies[which(NPSpecies$species.science == "Calandrinia ciliata"),"species.common"] <- parkStats[which(parkStats$species.science == "Calandrinia ciliata"),"species.common"]
    # NPSpecies[which(NPSpecies$species.science == "Calandrinia ciliata"),"species.inat.ID"] <- parkStats[which(parkStats$species.science == "Calandrinia ciliata"),"species.inat.ID"] 
    
    NPSpecies[which(NPSpecies$species.science == "Calandrinia ciliata"),"species.common"] <- "Fringed redmaids" #explicit correction prevents error
    NPSpecies[which(NPSpecies$species.science == "Calandrinia ciliata"),"species.inat.ID"] <- 489239
    
    NPSpecies[which(NPSpecies$family.common == "Mallows"),"order.common"] <- ""
    NPSpecies[which(NPSpecies$family.common == "Mallows"),"family.common"] <- "Mallows and allies"
  }
  
  if(park_id == "SAMO"){
    NPSpecies[which(NPSpecies$family.science == "Plantaginaceae"),"family.common"] <- "Plantain family"
  }
  
  if(park_id == "GOGA"){
    NPSpecies[which(NPSpecies$species.science == "Nucella emarginata"),"species.common"] <- parkStats[which(parkStats$species.science == "Nucella emarginata"),"species.common"]
    NPSpecies[which(NPSpecies$species.science == "Nucella emarginata"),"species.inat.ID"] <- parkStats[which(parkStats$species.science == "Nucella emarginata"),"species.inat.ID"]
    
    NPSpecies[which(NPSpecies$species.science == "Hermissenda crassicornis"),"species.common"] <- parkStats[which(parkStats$species.science == "Hermissenda crassicornis"),"species.common"]
    NPSpecies[which(NPSpecies$species.science == "Hermissenda crassicornis"),"species.inat.ID"] <- parkStats[which(parkStats$species.science == "Hermissenda crassicornis"),"species.inat.ID"]
    
    NPSpecies[which(NPSpecies$species.science == "Aeolidia papillosa"),"species.common"] <- parkStats[which(parkStats$species.science == "Aeolidia papillosa"),"species.common"]
    NPSpecies[which(NPSpecies$species.science == "Aeolidia papillosa"),"species.inat.ID"] <- parkStats[which(parkStats$species.science == "Aeolidia papillosa"),"species.inat.ID"]
    
    NPSpecies[which(NPSpecies$species.science == "Calandrinia ciliata"),"species.common"] <- parkStats[which(parkStats$species.science == "Calandrinia ciliata"),"species.common"]
    NPSpecies[which(NPSpecies$species.science == "Calandrinia ciliata"),"species.inat.ID"] <- parkStats[which(parkStats$species.science == "Calandrinia ciliata"),"species.inat.ID"]
    
    NPSpecies[which(NPSpecies$family.science == "Plantaginaceae"),"family.common"] <- "Plantain family"
  }
  
  mergedSpecies <- merge.data.frame(parkStats, NPSpecies, by = wanted.columns, all = TRUE, suffixes = c("",".NPS"))

  ##To troubleshoot multiples:
  multiples <- which(duplicated(mergedSpecies[,c("species.science", "genus.science")]))
  if (length(multiples) > 0){
    for (m in multiples){ #check each multiple
      entries <- mergedSpecies[which(mergedSpecies$species.science == mergedSpecies$species.science[m]),]
      # which(entries[1,wanted.columns] != entries[2,wanted.columns]) #find the difference
      NPSpecies$species.inat.ID[which(NPSpecies$species.science == mergedSpecies$species.science[m])] <-  parkStats$species.inat.ID[which(parkStats$species.science == mergedSpecies$species.science[m])]
      
      # if(entries$family.science[1] == "Malvaceae"){
      #   NPSpecies$order.common[which(NPSpecies$species.science == mergedSpecies$species.science[m])] <-  parkStats$order.common[which(parkStats$species.science == mergedSpecies$species.science[m])]
      #   NPSpecies$family.common[which(NPSpecies$species.science == mergedSpecies$species.science[m])] <-  parkStats$family.common[which(parkStats$species.science == mergedSpecies$species.science[m])]
      # }
      
      #automatic fix for if common names differ- defaults to what iNat has
      if(identical(entries$species.inat.ID,entries$species.inat.ID) == TRUE & identical(entries$species.science,entries$species.science) == TRUE){
        NPSpecies[which(NPSpecies$species.science == mergedSpecies$species.science[m]),paste0(ranks_to_match, ".common")] <-  parkStats[which(parkStats$species.science == mergedSpecies$species.science[m]),paste0(ranks_to_match, ".common")]
      }
    }
    mergedSpecies <- merge.data.frame(parkStats, NPSpecies, by = wanted.columns, all = TRUE, suffixes = c("",".NPS"))
  }

  #check if multiples still exist, if they do but species/IDs match up, assign to iNat values
  multiples <- which(duplicated(mergedSpecies[,c("species.science", "genus.science")]))
  if (length(multiples) > 0){
    for (m in multiples){ #check each multiple
      entries <- mergedSpecies[which(mergedSpecies$species.science == mergedSpecies$species.science[m]),]
      
      
      #if all the species IDs match up, uses iNat entries for all the values.
      iNat.ent <- which(!is.na(entries$original.inat.taxonID))
      NPS.idx <- which(NPSpecies$species.science == mergedSpecies$species.science[m])
      if (identical(entries$species.inat.ID,entries$species.inat.ID) == TRUE 
          & identical(entries$species.science,entries$species.science) == TRUE 
          & identical(as.character(entries$original.inat.taxonID[iNat.ent]), entries$species.inat.ID[-iNat.ent]) == TRUE) {
       
         NPSpecies[NPS.idx,wanted.columns] <- entries[iNat.ent,wanted.columns]
      
         }
    }
    mergedSpecies <- merge.data.frame(parkStats, NPSpecies, by = wanted.columns, all = TRUE, suffixes = c("",".NPS"))
  }
  
  
  
  multiples <- which(duplicated(mergedSpecies[,c("species.science", "genus.science")]))
  if(length(multiples > 0)){print(paste0("There are multiples for ", park_id))}
  
  
  
  
  
  mergedSpecies <- mergedSpecies[-which(mergedSpecies$Occurrence == "Not In Park" & is.na(mergedSpecies$total_obs)),] #remove species agreed to not be in park
  
  #add speciesFixed where needed
  has.species <- which(is.na(mergedSpecies$speciesFixed) & mergedSpecies$species.science != "")
  mergedSpecies$speciesFixed[has.species] <- mergedSpecies$species.science[has.species]
  
  use.genus <- which(is.na(mergedSpecies$speciesFixed) & mergedSpecies$species.science == "" & mergedSpecies$genus.science != "")
  mergedSpecies$speciesFixed[use.genus] <- mergedSpecies$species.science[use.genus]
  
  if(length(which(is.na(mergedSpecies$speciesFixed))) != 0) print("Some speciesFixed are NA")
  
  
  filePathFull = paste0("processed_data/",park_id,"_data/",park_id,"_species_list_updated.csv")
  write.csv(mergedSpecies, filePathFull, row.names = FALSE)
  
  return(mergedSpecies)
}
####Cleaning function####
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
  parkObs$speciesFixed <- gsub("([A-z]+) ([A-z]+) ([A-z]+)",'\\1 \\2', parkObs$speciesFixed) #takes only the first parts of names
  
  names(parkObs)[which(names(parkObs)== "taxonID")] <- "original.inat.taxonID"
  
  parkObs <- addCategories(parkObs)
  
  #Write out the observation data
  allObs <- parkObs
  if(dir.exists(paste0("processed_data/",park_id,"_data/")) == FALSE) dir.create(paste0("processed_data/",park_id,"_data/"))
  filePathFull = paste0("processed_data/",park_id,"_data/",park_id,"_obs_FULL_updated.csv")
  write.csv(allObs,filePathFull,row.names = F)
  
  cleanObs <- parkObs[,c("decimalLongitude","decimalLatitude",
                         "category","kingdom","phylum","class","order","family","genus","speciesFixed",
                         "year","month","hour",
                         "userNumber", "original.inat.taxonID", "id")]
  filePathClean = paste0("processed_data/",park_id,"_data/",park_id,"_obs_tidy_updated.csv")
  write.csv(cleanObs,filePathClean,row.names = F)
  
  return(parkObs)
  #return(cleanObs)
}

####Full loop####
processPark <- function(allParkObs, park_id){
  if(file.exists(paste0("processed_data/",park_id,"_data/",park_id,"_shape")) == FALSE) makeParkShapefile(park_id)
  parkObs <- cleanParkObservations(park_id = park_id, allParkObs = allParkObs)
  matchedObs <- matchToiNatParkList(parkObs, ranks_to_match = c("kingdom","phylum","class","order","family","genus","species"))
  stats <- inatStatsByPark(matchedParkObs = matchedObs)
  park_list <- matchNPSpecies(stats,park_id = park_id)
  print(paste0(park_id, " data processed!"))
}

####Running functions####
allParkObs.all <- read.csv("processed_data/park_obs.csv",header = T, stringsAsFactors = FALSE)
allParkObs <- allParkObs.all[-which(is.na(allParkObs.all$eventDate)),]
ranks_to_species <- c("kingdom","phylum","class","order","family","genus","species")

parks <- c("PORE", #Point Reyes, ~5.5k observations
           "SAMO", #Santa Monica Mountains, ~11k obsevations
           "GOGA" #Golden Gate National Recreation Area (GOGA), ~14k observations
)

for(park in parks){
  processPark(allParkObs = allParkObs, park_id = park)
}
