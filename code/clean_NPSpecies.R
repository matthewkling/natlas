setwd("~/Desktop/clean/natlas")
library(stringr)
library(dplyr)
library(beepr)

#####Base functions####
tidyNPSpecies <- function(list.path){
  park_list_raw <-  read.csv(list.path, header = T)
  park_list <- park_list_raw[, c("Category", "Order","Family","Taxon.Code","TSN","Taxon.Record.Status", "Scientific.Name","Common.Names","Synonyms","Occurrence")]
  
  #detect and label hybrids, varieties; make their formatting like iNat's.
  park_list$other.rank <- ""; levels(park_list$other.rank) <- c("hybrid", "variety", "subspecies","genus")
  park_list$other.rank[which(str_detect(park_list$Scientific.Name," X "))] <- "hybrid" 
  park_list$Scientific.Name <- gsub(" X ", " x ", park_list$Scientific.Name)
  
  park_list$other.rank[which(str_detect(park_list$Scientific.Name," var. "))] <- "variety"
  park_list$Scientific.Name <- gsub(" var. ", " ", park_list$Scientific.Name)
  
  park_list$other.rank[which(str_detect(park_list$Scientific.Name," ssp. "))] <- "subspecies"
  park_list$Scientific.Name <- gsub(" ssp. ", " ", park_list$Scientific.Name)
  
  park_list$other.rank[grep(" ",park_list$Scientific.Name, invert = TRUE)] <- "genus" #entries with no spaces are assumed to be genus-level
  
  #More specific issues
  park_list$Scientific.Name <- gsub("Charadrius leshcenaultii", "Charadrius leschenaultii", park_list$Scientific.Name)
  park_list$Scientific.Name <- gsub("Plegadis chichi", "Plegadis chihi",park_list$Scientific.Name)
  
  return(park_list)
}

checkNPSbyiNat <- function(inatTaxaObs, tidied_park_list){
  if(length(which(names(tidied_park_list) == "matched.ID"))==0) tidied_park_list$matched.ID <- NA #adds ID column if one doesn't exist, leaves it alone if it does.
  
  #specific fix
  tidied_park_list$matched.ID[which(tidied_park_list$Scientific.Name == "Pseudognaphalium bioletti")] <- 58829
  
  #keep track
  unmatched <- NULL
  morethanone <- NULL
  syn.match <- NULL
  morethanonesyn <- NULL
  
  need.ID <- which(is.na(tidied_park_list$matched.ID))
  #for (t in 1:length(tidied_park_list$Scientific.Name)){ #this was whole length, going to use an index so function is more flexible
  for(t in need.ID){
    obs.idx <- which(inatTaxaObs$scientificName == tidied_park_list$Scientific.Name[t])
    
    if(length(obs.idx) == 1) tidied_park_list$matched.ID[t] <- inatTaxaObs$taxonID[obs.idx]
    if(length(obs.idx) == 0) 
    {
      if(tidied_park_list$Synonyms[t] == "") unmatched <- c(unmatched,t)
      else {
        syn.list <- strsplit(as.character(tidied_park_list$Synonyms[t]), ", ")[[1]]
        for (d in syn.list){
          syn.idx <- which(inatTaxaObs$scientificName == d)
          if(length(syn.idx) == 1) tidied_park_list$matched.ID[t] <- inatTaxaObs$taxonID[syn.idx]; syn.match <- c(syn.match, t)
          if(length(syn.idx) == 0) #unmatched <- c(unmatched,t)
            if(length(syn.idx) > 1 ) morethanonesyn <- c(morethanonesyn, t)
        }
      }
    }
    
    if(length(obs.idx) > 1 ) 
    {
      morethanone <- c(morethanone, t)
      tidied_park_list$matched.ID[t] <- max(inatTaxaObs$taxonID[obs.idx]) #picks the larger taxon ID number, this seems to happen from inactive taxa.
    }
    if (which(need.ID == t) %% 10 == 0| which(need.ID == t) == 1 | which(need.ID == t) == length(need.ID)) print(paste0(100*t/length(tidied_park_list$matched.ID),"% matched")) #if something breaks look here
  }
  print(paste0(100*length(which(is.na(tidied_park_list$matched.ID)))/length(tidied_park_list$matched.ID), "% without IDs")) 
  return(tidied_park_list)
}

subspeciesRemove <- function(tidied_park_list,inatTaxaObs){
  unmatched <- which(is.na(tidied_park_list$matched.ID))
  
  for (u in unmatched){
    if(tidied_park_list$other.rank[u] != "hybrid"){ #strips varaities and subspecies to species
      tidied_park_list$Scientific.Name[u] <- gsub("([A-z]+) ([A-z]+) .*",'\\1 \\2',tidied_park_list$Scientific.Name[u])
      obs.idx <- which(inatTaxaObs$scientificName == tidied_park_list$Scientific.Name[u])

      if(length(obs.idx) == 1) tidied_park_list$matched.ID[u] <- inatTaxaObs$taxonID[obs.idx]
      if(length(obs.idx) >1 ) tidied_park_list$matched.ID[u] <- max(inatTaxaObs$taxonID[obs.idx])
    }
  }
  print(paste0(100*length(which(is.na(tidied_park_list$matched.ID)))/length(tidied_park_list$matched.ID), "% without IDs"))
  return(tidied_park_list)
}

searchiNat <- function(tax.name){
  search.term <- gsub(" ", "+", tax.name)
  search.results <- readLines(paste0("https://www.inaturalist.org/taxa/search?utf8=%E2%9C%93&q=",search.term))
  found.name <- gsub(" ", "-", tax.name)
  search.name<- paste0("<a href=\"/taxa/([0-9]*)-", found.name, "\">")
  any.search.result <- "<a href=\"/taxa/([0-9]*)-"
  
  name.results.idx = NULL
  any.return.idx = NULL
  for (l in 1:length(search.results)){
    if(gregexpr(search.name,search.results[l])[[1]][1] != -1) {name.results.idx = c(name.results.idx, l)}
    if(gregexpr(any.search.result,search.results[l])[[1]][1] != -1) {any.return.idx = c(any.return.idx, l)}
  }
  
  IDs.found <- NULL
  for (r in any.return.idx){
    possible.ID <- gsub(".*<a href=\"/taxa/([0-9]*?)-.*", "\\1", search.results[r])
    IDs.found <- c(IDs.found,possible.ID)
  }
  return(IDs.found)
}

make.iNat.check.df <- function(ranks_to_scrape){
  scrape.colnames <- c(paste0(ranks_to_scrape, ".inat.ID"),
                       paste0(ranks_to_scrape, ".science"),
                       paste0(ranks_to_scrape, ".common"),
                       "name.in.page")
  
  return.df <- as.data.frame(matrix(data = "", nrow = 1, ncol = length(scrape.colnames), 
                                    dimnames = list(c(),scrape.colnames)),
                             stringsAsFactors = FALSE)
  return(return.df)
}

check.search.IDs <- function(tax.name, scrape.ID, ranks_to_scrape, return.df){
  inat_url <- paste0("https://www.inaturalist.org/taxa/", scrape.ID)
  all.lines <- readLines(inat_url)
  scraped.tax <- return.df
  
  if((length(which(str_detect(all.lines,tax.name) == TRUE)) > 0) == TRUE) scraped.tax$name.in.page <- TRUE
  if((length(which(str_detect(all.lines,tax.name) == TRUE)) > 0) == FALSE) scraped.tax$name.in.page <- FALSE

  tax.idx <- NULL
  
  for (i in 1:length(all.lines)){
    if (gregexpr("taxon: \\{\"total_results\"",all.lines[i])[[1]][1] != -1) {tax.idx = c(tax.idx, i)} #not all have preferred common names
  }
  
  tax.lines <- all.lines[tax.idx]
  n <- strsplit(tax.lines, "\\{\\\"observations_count") #breaks up taxonomy into components
  
  for(rank in ranks_to_scrape){
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

scrape.iNat.ID <- function(scrape.ID, ranks_to_scrape = c("kingdom","phylum","class","order","family","genus","species"), return.df){
  if(is.na(scrape.ID) == FALSE)
  {
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
}

addCategoriesNPSOLD <- function(species.data,kingdom.column = "kingdom.science", class.column = "class.science"){
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

addCategoriesNPS <- function(species.data,kingdom.column = "kingdom.science", class.column = "class.science"){
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
  
  #taxize was producing errors; changing NPS categories instead (it's also faster)
  fix.idx <- which(is.na(species.data$category))
  
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Vascular Plant" |
                                        species.data$Category[fix.idx] =="Non-vascular Plant" | 
                                        species.data$Category[fix.idx] == "Chromista")]] <- "Plant"
  
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Amphibian" |
                                        species.data$Category[fix.idx] == "Reptile")]] <- "Herp"
  
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Mammal")]] <- "Mammal"
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Bird")]] <- "Bird"
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Fungi")]] <- "Fungi"
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Fish")]] <- "Fish"
  
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Amphibian" |
                                        species.data$Category[fix.idx] == "Reptile")]] <- "Herp"
  
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Spider/Scorpion" |
                                        species.data$Category[fix.idx] =="Slug/Snail" | 
                                        species.data$Category[fix.idx] =="Insect" | 
                                        species.data$Category[fix.idx] =="Crab/Lobster/Shrimp" | 
                                        species.data$Category[fix.idx] == "Other Non-vertebrates")]] <- "Invertebrate"
  
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Vascular Plant" | species.data$Category[fix.idx] =="Non-vascular Plant" | species.data$Category[fix.idx] == "Chromista")]] <- "Plant"
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Vascular Plant" | species.data$Category[fix.idx] =="Non-vascular Plant" | species.data$Category[fix.idx] == "Chromista")]] <- "Plant"
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Vascular Plant" | species.data$Category[fix.idx] =="Non-vascular Plant" | species.data$Category[fix.idx] == "Chromista")]] <- "Plant"
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Vascular Plant" | species.data$Category[fix.idx] =="Non-vascular Plant" | species.data$Category[fix.idx] == "Chromista")]] <- "Plant"
  species.data$category[fix.idx[which(species.data$Category[fix.idx] == "Vascular Plant" | species.data$Category[fix.idx] =="Non-vascular Plant" | species.data$Category[fix.idx] == "Chromista")]] <- "Plant"
  
  return(species.data)
}

  
####Larger functions####
useiNat <- function(checked_park_list, ranks_to_scrape){
 
  #df for using iNat scrape
  df <- make.iNat.check.df(ranks_to_scrape = ranks_to_scrape)
  #set up dataframes for results
  scrape.colnames <- c(paste0(ranks_to_scrape, ".inat.ID"),
                       paste0(ranks_to_scrape, ".science"),
                       paste0(ranks_to_scrape, ".common"))
  
  checked_park_list[,c(scrape.colnames, "name.in.page")] <- "" #for retreived data
  
  #make index of rows that need to be searched for
  search.iNat.idx <- which(is.na(checked_park_list$matched.ID))
  
  for (beep in search.iNat.idx){
    tax.name <- checked_park_list[beep, "Scientific.Name"]
    IDs.to.check <- searchiNat(tax.name)
    inat.info <- df
    if(is.null(IDs.to.check) == FALSE){
      for (i in 1:length(IDs.to.check)){
        scrape.ID <- IDs.to.check[i]
        pages.checked <- check.search.IDs(tax.name = tax.name, scrape.ID <- scrape.ID, return.df = df, ranks_to_scrape = ranks_to_scrape)
        inat.info[i,] <- pages.checked
      }
      
      #select the ID
      if(length(which(inat.info$name.in.page == TRUE)) > 0) inat.info <- inat.info[which(inat.info$name.in.page == TRUE),] #if there are pages that mention the name of the taxon, selection will come from these
      if(length(which(inat.info$order.science == checked_park_list[beep,"Order"])) == 0) print(paste0(tax.name, " matched to ", IDs.to.check, " returns different order"))
      else {
        right.order <- inat.info[which(inat.info$order.science == checked_park_list[beep,"Order"]),]
        if(checked_park_list$other.rank[beep] == "genus") {
          found.ID  <- max(right.order$genus.inat.ID)
          checked_park_list[beep, names(right.order)] <- right.order[which(right.order$genus.inat.ID == max(right.order$genus.inat.ID))[1],]
        }
        
        else {
          found.ID  <- max(right.order$species.inat.ID)
          checked_park_list[beep, names(right.order)] <- right.order[which(right.order$genus.inat.ID == max(right.order$genus.inat.ID))[1],]}
      }
      checked_park_list$matched.ID[beep] <- found.ID
    }
    if (i %% 50 == 0| i == 1 | i == length(IDs.to.check)){print(paste0(100*(which(search.iNat.idx == beep)/length(search.iNat.idx)), "% through checking IDs"))}
  }
  return(checked_park_list)
}

matchToParkList <- function(checked.NPS,ranks_to_scrape){
  #read in list that already exists
  extant.lists <- list.files(path = "processed_data",pattern = "(.*)_all_parks_inat_species_list.csv")
  most.recent.file <- extant.lists[which.max(gsub("_all_parks_inat_species_list.csv","", extant.lists))]
  species.list <- read.csv(paste0("processed_data/",most.recent.file), stringsAsFactors = FALSE)
  
  wanted.columns <- c(paste0(ranks_to_scrape, ".inat.ID"),
                      paste0(ranks_to_scrape, ".science"),
                      paste0(ranks_to_scrape, ".common"),
                      "category", "taxonRank")
  
  checked.NPS$category <- as.character("")
  checked.NPS$taxonRank <- as.character("")
  
  for(idx in 1:length(checked.NPS$matched))
  {
    species.row <- which(species.list$taxonID == checked.NPS$matched.ID[idx])
    if(length(species.row) > 0){
      checked.NPS[idx,wanted.columns] <- species.list[species.row[1],wanted.columns]
    }
  }
  return(checked.NPS)
}

scrapeMissing <- function(processedNPS,ranks_to_scrape){
  missing.info <- which(processedNPS$kingdom.inat.ID == "")
  df <- make.iNat.check.df(ranks_to_scrape = ranks_to_scrape)
  
  for (m in missing.info){
    scraped <- scrape.iNat.ID(scrape.ID = processedNPS$matched.ID[m], return.df = df, ranks_to_scrape = ranks_to_scrape)
    processedNPS[m,names(scraped)] <- scraped
    if (which(missing.info == m) %% 10 == 0| which(missing.info == m) == 1 | which(missing.info == m) == length(missing.info)) print(paste0(100*which(missing.info == m)/length(missing.info), "% missing entries added"))
  }
  processedNPS <- addCategoriesNPS(processedNPS)
  
  processedNPS[which(processedNPS$other.rank == "genus"), paste0("species", c(".inat.ID", ".science", ".common"))] <- "" #fix accidental species grabs
  
  return(processedNPS)
}

writeCleanNPSlist <- function(processedList, park_id){
  cleanNPSlist <- distinct(processedList, species.science ,genus.science, .keep_all = TRUE)
  remove.columns <- which(names(cleanNPSlist) %in% c("Category","Order","Family","Taxon.Code","TSN","Taxon.Record.Status","name.in.page","Synonyms","Common.Names",  "taxonRank"))
  cleanNPSlist <- cleanNPSlist[, - remove.columns]
  
  filePath <- paste0("processed_data/",park_id,"_data/",park_id,"_clean_NPSpecies.csv")
  write.csv(cleanNPSlist, filePath, row.names = FALSE)
  return(cleanNPSlist)
}

#### Current test code###
NPS.lists <- list.files(path = "raw_data",pattern = "NPSpecies_(.*).csv")
allObsTaxa <- read.csv("processed_data/iNatObservedTaxa.csv", header = TRUE, stringsAsFactors = FALSE)
ranks_to_species <- c("kingdom","phylum","class","order","family","genus","species")

for(each in NPS.lists){
  park_id <- gsub('NPSpecies_(.*).csv','\\1',each)
  if(file.exists(paste0("processed_data/",park_id,"_data/",park_id,"_clean_NPSpecies.csv")) == FALSE){
    tidyNPS <- tidyNPSpecies(list.path = paste0("raw_data/", each))
    first.matched <- checkNPSbyiNat(tidied_park_list = tidyNPS, inatTaxaObs = allObsTaxa)
    ssp.rm <- subspeciesRemove(tidied_park_list = first.matched, inatTaxaObs = allObsTaxa)
    
    checked <- useiNat(checked_park_list = ssp.rm, ranks_to_scrape  = ranks_to_species); beep(2)
    matched <- matchToParkList(checked.NPS = checked, ranks_to_scrape  = ranks_to_species); beep(2)
    completed <- scrapeMissing(processedNPS = matched, ranks_to_scrape = ranks_to_species); beep(2)
    clean <- writeCleanNPSlist(completed,park_id)
    
    print(paste0(park_id, " NPSpecies has been tidied!"))
  } else print(paste0(park_id, " already has tidied file"))
}

