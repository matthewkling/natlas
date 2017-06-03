setwd("~/Desktop/clean/natlas")
library(beepr)
library(taxize)
library(rgdal)
library(raster)

####Download functions####
downloadObservations <- function(){
  download.file("http://www.inaturalist.org/observations/gbif-observations-dwca.zip", "raw_data/all_inat_obs")
  unzip("raw_data/all_inat_obs", files = "observations.csv", exdir = "raw_data")
  file.remove("raw_data/all_inat_obs")
  beep(sound = 2)
  }
  
#Download National Park Boundaries#
  # Found at https://catalog.data.gov/dataset/national-park-boundariesf0a4c
  # Metadata at https://catalog.data.gov/harvest/object/d66865e2-e232-4da9-b399-1da516e1b73c/html
  downloadParks <- function(){
  download.file("http://gstore.unm.edu/apps/rgis/datasets/7bbe8af5-029b-4adf-b06c-134f0dd57226/nps_boundary.original.zip", "raw_data/park_boundaries_download")
  unzip("raw_data/park_boundaries_download", exdir = "raw_data/park_boundaries")
  file.remove("raw_data/park_boundaries_download")
}

####Tidy data functions####
tidyData <- function(){
  allObs <- read.csv("raw_data/observations.csv", header = TRUE, stringsAsFactors = FALSE)#; beep(sound = 2) #read in complete iNat dataset
  
  print("All obs read in")
  
  #create list of all unique species/genera in the dataset
  allObsTaxa <- allObs[,c("taxonID","scientificName","taxonRank","kingdom","phylum","class","order","family","genus")]
  allObsTaxa <- distinct(allObsTaxa)
  write.csv(allObsTaxa, "processed_data/iNatObservedTaxa.csv", row.names = FALSE)
  
  allObsSpecies <- allObs[which(allObs$taxonRank == "species" |allObs$taxonRank == "genus"),c("taxonID","scientificName","taxonRank","kingdom","phylum","class","order","family","genus")]
  allObsSpecies <- distinct(allObsSpecies)
  write.csv(allObsSpecies, "processed_data/iNatSpecies.csv", row.names = FALSE)
  
  #Work with US observations
  usObsRaw <- subset(allObs,countryCode == "US") #select only observations from inside US
  usObs <- usObsRaw #so that if we make an error while removing columns or other data, we don't have to read in all the data over again.
  
  print("US data subsetted")
  
  usObs <- usObs[-which(usObs$establishmentMeans == "cultivated"),] #so leaves observations that are wild
  usObs <- usObs[-which(!is.finite(usObs$decimalLatitude)),]  #removes observations with bad coordinates
  usObs <- usObs[-which(usObs$informationWithheld == "Coordinates hidden at the request of the observer"),]
  #usObs <- usObs[-which(usObs$informationWithheld == "Coordinate uncertainty increased by 10000m at the request of the observer"),] #6 miles uncertainty added; between threatened taxa and observer requests, ~8% of points are altered that way. Remove?
  #usObs <- usObs[-which(usObs$informationWithheld == "Coordinate uncertainty increased by 10000m to protect threatened taxon"),]
  
  usObs$userNumber <- as.numeric(factor(usObs$recordedBy,levels=unique(usObs$recordedBy)))
  
  usObs <- usObs[,c( #selecting the columns of interest for us; if you want to see complete column options, run names(usObsRaw) in console
    "id", "recordedBy", "userNumber",
    "eventDate", "eventTime", "dateIdentified",             
    "decimalLatitude", "decimalLongitude", "verbatimLocality", "coordinateUncertaintyInMeters", 
    "informationWithheld", 
    #"establishmentMeans", # all cultivated have been removed, no need to pull this column
    "identificationID", "taxonID","scientificName","taxonRank",
    "kingdom", "phylum", "class","order","family","genus"
  )]
  
  #changes date columns to be readable as dates; adds month and year columns for US data
  usObs$eventDate <- as.Date(usObs$eventDate)
  usObs$dateIdentified <- as.Date(usObs$dateIdentified)
  usObs$year <- year(usObs$eventDate)
  usObs$month <- month(usObs$eventDate)
  usObs$hour <- gsub("([0-9].*):([0-9].*):.*", "\\1", usObs$eventTime)
  
  cleanObs <- usObs[-which(usObs$eventDate < "2007-01-01"),];  cleanObs <- cleanObs[-which(is.na(usObs$eventDate)),]#removes pre-2007 observations, observations with no assocated event date.
  write.csv(cleanObs, "processed_data/clean_us_obs.csv", row.names = F) #writes out files 
  
  print("Clean US obs written out")
  
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
  
  print("Park file written")
}


####species list base functions####
trimiNatObservations <- function(iNat.obs){
  allSpecies <- iNat.obs[,c("taxonID","scientificName","taxonRank","kingdom","phylum","class","order","family","genus")]
  allSpecies <- unique(allSpecies)
  allSpecies <- allSpecies[order(allSpecies$kingdom, allSpecies$scientificName),]
  allSpecies$taxonRank <- factor(allSpecies$taxonRank)
  
  return(allSpecies)
}

make.iNat.scrape.df <- function(ranks_to_scrape){
  scrape.colnames <- c(paste0(ranks_to_scrape, ".inat.ID"),
                       paste0(ranks_to_scrape, ".science"),
                       paste0(ranks_to_scrape, ".common"))
  
  return.df <- as.data.frame(matrix(data = "", nrow = 1, ncol = length(ranks_to_scrape)*3, 
                                    dimnames = list(c(),scrape.colnames)),
                             stringsAsFactors = FALSE)
  return(return.df)
}

scrape.iNat.ID <- function(scrape.ID, ranks_to_scrape = c("kingdom","phylum","class","order","family","genus","species"), return.df){
  inat_url <- paste0("https://www.inaturalist.org/taxa/", scrape.ID)
  all.lines <- readLines(inat_url)
  tax.idx <- NULL
  scraped.tax <- return.df
  
  for (i in 1:length(all.lines)){
    #if (gregexpr("preferred_common_name",all.lines[i])[[1]][1] != -1) {tax.idx = c(tax.idx, i)} 
    if (gregexpr("taxon: \\{\"total_results\"",all.lines[i])[[1]][1] != -1) {tax.idx = c(tax.idx, i)} #not all have preferred common names
  }
  
  tax.lines <- all.lines[tax.idx]
  n <- strsplit(tax.lines, "\\{\\\"observations_count") #breaks up taxonomy into components
  
  for(rank in ranks_to_scrape){
    #rank <- ranks_to_scrape[1]
    rank.line.idx <- grep(paste0("\"rank\":\"", rank, "\""),n[[1]])
    
    if(length(rank.line.idx) == 0) print(paste0("No ", rank, " found for ", scrape.ID))
    else{
      
      if(length(rank.line.idx) > 1) {
        rank.line.idx <- rank.line.idx[1] 
        #print(paste0("Multiple lines at ", rank, " level for ", scrape.ID))
      }#there were errors
      rank.line <- n[[1]][rank.line.idx]
      
      #reset values
      inat.ID <- ""
      sci.name.clean <-""
      common.name <- ""
      
      #extract information
      inat.ID <- sub(".*,\"id\":([0-9].*?),.*", "\\1", rank.line)
      sci.name.clean <- sub(".*,\"name\":\"(.*?)\",.*", "\\1", rank.line)
      
      if (str_detect(rank.line, "preferred_common_name") == TRUE) {
        common.name <- gsub(".*,\"preferred_common_name\":\"([A-z| |-].*?)\".*", "\\1", rank.line)
      } else common.name <- ""
      
      #put information into df
      scraped.tax[1,paste0(rank, c(".inat.ID", ".science", ".common"))] <- c(inat.ID, sci.name.clean, common.name)
      
    }
  }
  return(scraped.tax)
}

addCategories <- function(species.data,kingdom.column = "kingdom.science", class.column = "class.science"){
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
  species.data$category[which(species.data[,class.column] == "Actinopterygii")] <- "Fish" #bony fishes
  species.data$category[which(species.data[,class.column] == "Myxini")] <- "Fish" #hagfishes
  species.data$category[which(species.data[,class.column] == "Hyperoartia")] <- "Fish" #lamprays
  species.data$category[which(species.data[,class.column] == "Sarcopterygii")] <- "Fish" #lobe-finned fish
  species.data$category[which(species.data[,kingdom.column] == "Chromista")] <- "Plants" #this includes the kelps, so plants made marginally more sense than microbes?
  species.data$category[which(species.data[,kingdom.column] == "Archaea")] <- "Microbe"
  species.data$category[which(species.data[,kingdom.column] == "Bacteria")] <- "Microbe"
  species.data$category[which(species.data[,kingdom.column] == "Protozoa")] <- "Microbe"
  
  
  fix.idx <- which(is.na(species.data$category))
  for (to.fix in unique(species.data[,class.column][fix.idx])){
    fix.loc <- which(species.data[,class.column] == to.fix)
    tax <- classification(to.fix, db = 'itis')
    if (tax[[1]]$name[which(tax[[1]]$rank == "phylum")] == "Chordata" && tax[[1]]$name[which(tax[[1]]$rank == "subphylum")] !="Vertebrata") {
      species.data$category[fix.loc] <- "Invertebrate"
    }
  }
  return(species.data)
}

#### species list completed functions ####
iNatScrapeAllObs <- function(observations,ranks_to_scrape = c("kingdom","phylum","class","order","family","genus","species")){
  species.dat = trimiNatObservations(observations)
  
  #set up dataframes for results
  scrape.colnames <- c(paste0(ranks_to_scrape, ".inat.ID"),
                       paste0(ranks_to_scrape, ".science"),
                       paste0(ranks_to_scrape, ".common"))
  
  species.dat[,scrape.colnames] <- ""
  df <- make.iNat.scrape.df(ranks_to_scrape)
  
  #scrape loop
  for(id.idx in 1:length(species.dat$taxonID)){
    scrape.ID <- species.dat$taxonID[id.idx]
    scraped <- scrape.iNat.ID(scrape.ID, return.df = df)
    species.dat[id.idx,names(scraped)] <- scraped
    print(paste0(100*id.idx/length(species.dat$taxonID), "%"))
  }
  
  species.dat <- addCategories(species.dat)
  beep(2)
  
  current.date <- gsub(" |-|:","",gsub(" PDT","",Sys.time()))
  filePathFull = paste0("processed_data/",current.date,"_all_parks_inat_species_list.csv")
  write.csv(clean.park.species, filePathFull, row.names = FALSE)
  
  return(species.dat)
}

iNatUpdateObs <- function(iNat.obs,ranks_to_scrape = c("kingdom","phylum","class","order","family","genus","species")){
  #read in, make list from observations
  obs.species <- trimiNatObservations(iNat.obs)
  
  #read in list that already exists
  extant.lists <- list.files(path = "processed_data",pattern = "(.*)_all_parks_inat_species_list.csv")
  most.recent.file <- extant.lists[which.max(gsub("_all_parks_inat_species_list.csv","", extant.lists))]
  old.list <- read.csv(paste0("processed_data/",most.recent.file))
  
  #find new taxon IDs
  new.IDs <- setdiff(obs.species$taxonID, old.list$taxonID)
  
  #set up dataframes for results
  scrape.colnames <- c(paste0(ranks_to_scrape, ".inat.ID"),
                       paste0(ranks_to_scrape, ".science"),
                       paste0(ranks_to_scrape, ".common"))
  
  df <- make.iNat.scrape.df(ranks_to_scrape)
  new.species <- df
  
  #scrape loop
  i <- 1
  for(ID in new.IDs){
    scraped <- scrape.iNat.ID(ID, return.df = df, ranks_to_scrape = ranks_to_scrape)
    new.species[i,] <- scraped
    print(paste0(100*i/length(new.IDs),"% new IDs retrieved"))
    i <- i + 1
  }
  if (new.species$kingdom.inat.ID != ""){
    new.species <- addCategories(new.species)
    to.add <- cbind(obs.species[which(obs.species$taxonID %in% new.IDs),],new.species)
    new.list <- rbind(old.list, to.add)
    #make sure higher-level taxa don't accidentally have lower-level info
    new.list[which(new.list$taxonRank == "class"), paste0(rep(c("order", "family", "genus","species"),each = 3), c(".inat.ID", ".science", ".common"))] <- ""
    new.list[which(new.list$taxonRank == "order"), paste0(rep(c("family", "genus","species"),each = 3), c(".inat.ID", ".science", ".common"))] <- ""
    new.list[which(new.list$taxonRank == "family"), paste0(rep(c("genus","species"),each = 3), c(".inat.ID", ".science", ".common"))] <- ""
    new.list[which(new.list$taxonRank == "subfamily"), paste0(rep(c("genus","species"),each = 3), c(".inat.ID", ".science", ".common"))] <- ""
    new.list[which(new.list$taxonRank == "tribe"), paste0(rep(c( "genus","species"),each = 3), c(".inat.ID", ".science", ".common"))] <- ""
    new.list[which(new.list$taxonRank == "genus"), paste0("species", c(".inat.ID", ".science", ".common"))] <- ""
    new.list[which(new.list$taxonRank == "hybrid"), paste0("species", c(".inat.ID", ".science", ".common"))] <- ""
    
    current.date <- gsub(" |-|:","",gsub(" PDT","",Sys.time()))
    filePathFull = paste0("processed_data/",current.date,"_all_parks_inat_species_list.csv")
    write.csv(new.list, filePathFull, row.names = FALSE)
  }
  if (new.species$kingdom.inat.ID != ""){
    new.list <- old.list
    print("List was already up-to-date")
    }
  
  beep(2)
  return(new.list)
}

####Update and first runs####
updateiNat <- function(){
  downloadObservations(); print("Observations downloaded")
  tidyData(); print("Observations tidied")
  
  all.park.observations <- read.csv("processed_data/park_obs.csv",header = T)
  update.scrape <- iNatUpdateObs(iNat.obs = all.park.observations,ranks_to_scrape = c("kingdom","phylum","class","order","family","genus","species")); print("Scraped species list updated")
  
  beep(1)
}

firstDownload <- function(){
  downloadObservations(); print("Observations downloaded")
  downloadParks(); print("Parks downloaded")
  tidyData(); print("Observations tidied")
  all.park.observations <- read.csv("processed_data/park_obs.csv",header = T)
  
  iNatScrapeAllObs(observations <- all.park.observations); print("Park species scraped")
  beep(1)
}
####Run function####
updateiNat()