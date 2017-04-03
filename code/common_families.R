setwd("~/taxorama")
allParkObs <- read.csv("processed_data/park_obs.csv",header = T)
PORE <- read.csv("processed_data/PORE_data/PORE_species.csv",header=T)


library(tm)
library(stringi)
library(proxy)

####Wikispecies####
forWikispecies <- function(taxonList, taxonLevel){
  taxaNames <- data.frame(scientific = character(length(taxonList)), common = character(length(taxonList)), stringsAsFactors = FALSE)
  for (j in 1:length(taxonList))  
  {
    lines <- readLines(paste0("https://species.wikimedia.org/wiki/", taxonList[j]))
    
    nameline <- NULL
    for (i in 1:length(lines)){
      if (gregexpr("English:",lines[i])[[1]][1] != -1) {nameline = rbind(nameline, lines[i])}
    }
    if(is.null(nameline) == FALSE){
    name <- strsplit(nameline,"English:.*?;")[[1]][2]
    name <- strsplit(name,"<")[[1]][1]
    }
    else name <- ""
    taxaNames$scientific[j] <- taxonList[j]
    taxaNames$common[j] <- name
    print(paste0(100*j/length(taxonList),"% complete"))
  }
  names(taxaNames) <- c(paste0(taxonLevel,".scientific"), paste0(taxonLevel, ".common"))
  return(taxaNames)
}

phyla <- as.character(unique(sort(PORE$phylum)))
phylaNames <- forWikispecies(phyla, "phylum")
write.csv(phylaNames,"processed_data/POREwikispeciesphyla.csv", row.names = FALSE)

classes <- as.character(unique(sort(PORE$class)))
classNames <- forWikispecies(classes, "class")
write.csv(classNames,"processed_data/POREwikispeciesclasses.csv", row.names = FALSE)

orders <- as.character(unique(sort(PORE$order)))
orderNames <- forWikispecies(orders, "order")
write.csv(orderNames,"processed_data/POREwikispeciesorders.csv", row.names = FALSE)

families <- as.character(unique(sort(PORE$family)))
familyNames <- forWikispecies(families, "family")
write.csv(familyNames,"processed_data/POREwikispeciesfamilies.csv", row.names = FALSE)

genuses <- as.character(unique(sort(PORE$genus)))
genusNames <- forWikispecies(genuses, "genus")
write.csv(genusNames,"processed_data/POREwikispeciesgenuses.csv", row.names = FALSE)




# ####Animal phyla####
# animalPhylaURL <- "https://simple.wikipedia.org/wiki/List_of_animal_phyla?action=submit&veswitched=1" #from https://simple.wikipedia.org/wiki/List_of_animal_phyla#Sortable_table
# 
# 
# 
# ####Chordate orders####
# 
# 
# ####Birds families####
# forBirds <- function(birdsectionurl){
# b <- readLines(birdsectionurl)
# famlist <- NULL
# for (i in 1:length(b))
# {
#   if (gregexpr("idae",b[i])[[1]][1] != -1) {famlist = rbind(famlist, b[i])}
# }
# famlist <- gsub("\\[\\[File.*?\\]\\]","",famlist) #removes links to photos
# 
# retreivedNames <- data.frame(scientificFamily = character(length(famlist)), commonFamily = character(length(famlist)), stringsAsFactors = FALSE)
# for (j in 1:length(famlist))
#      {
#        line <- famlist[j]
#        adjline <- gsub("\\[|\\]|\\* |\\*", "", line)
#        retreivedNames[j, 1] <- strsplit(adjline, ": ")[[1]][1]
#        retreivedNames[j, 2] <- strsplit(adjline, ": ")[[1]][2]
#   }
# return(retreivedNames)
# }
# 
# p <- forBirds("https://en.wikipedia.org/w/index.php?title=List_of_birds&action=edit&section=2") #paleognathae 
# n <- forBirds("https://en.wikipedia.org/w/index.php?title=List_of_birds&action=edit&section=10") #neognathae
# 
# #corrections
# tapaculo <- which(n$scientificFamily == "Tapaculo|Rhinocryptidae tapaculos")
# n$scientificFamily[tapaculo] <- "Rhinocryptidae"; n$commonFamily[tapaculo] <- "tapaculos"
# n$commonFamily <- gsub("goose\\|","",n$commonFamily)
# n$commonFamily <- gsub("Guan \\(bird\\)\\|","",n$commonFamily)
# n$commonFamily <- gsub("whistler \\(bird\\)\\|","",n$commonFamily)
# n$commonFamily <- gsub("Bunting \\(bird\\)\\|","",n$commonFamily)
# n$commonFamily <- gsub("Batis \\(bird\\)\\|","",n$commonFamily)
# 
# birdfamilies <- rbind(p,n)
# write.csv(birdfamilies,"processed_data/bird_common_families.csv", row.names = FALSE)
# 
# 
# ####Mammal families####
# mammalurl <- "https://en.wikipedia.org/w/index.php?title=Mammal_classification&action=edit&section=1" #url for a subsection of the mammals
# forMammals <- function(mammalurl){
# m <- readLines(mammalurl)
# famlist <- NULL
# for (i in 1:length(m))
# {
#   if (gregexpr("Family \\[\\[",m[i])[[1]][1] != -1) {famlist = rbind(famlist, m[i])} #pulls lines that begin with "family
# }
# 
# retreivedNames <- data.frame(scientificFamily = character(length(famlist)), commonFamily = character(length(famlist)), stringsAsFactors = FALSE)
# for (j in 1:length(famlist))
# {
#   line <- famlist[j]
#   sci_section <- strsplit(line, ": ")[[1]][1]
#   sciname <- gsub("\\*|Family |\\[|\\]","",sci_section)
#   com_section <- strsplit(line, ": ")[[1]][2]
#   comname <- gsub( " *\\(.*?\\)\\,  *", "", com_section) #removes the "(# species)," parenthetical
#   comname <- gsub(" *\\(.*?\\) *","", comname) #removes the locations
#   retreivedNames[j, 1] <- sciname
#   retreivedNames[j, 2] <- comname
# }
# return(retreivedNames)
# }
# 
# mammalfamilies <- forMammals(mammalurl)
# write.csv(mammalfamilies,"processed_data/mammal_common_families.csv", row.names = FALSE)
# 
# ####Herp families####
# #reptiles
# anapsidurl <- "https://en.wikipedia.org/w/index.php?title=List_of_reptiles&action=edit&section=2"
# diapsidurl <- "https://en.wikipedia.org/w/index.php?title=List_of_reptiles&action=edit&section=3"
# 
# forReptiles <- function(reptileurl){
# r <- readLines(reptileurl)
# 
# famlist <- NULL
# for (i in 1:length(r))
# {
#   if (gregexpr("Family \\[\\[",r[i])[[1]][1] != -1) {famlist = rbind(famlist, r[i])} #pulls lines that begin with "family
# }
# 
# retreivedNames <- data.frame(scientificFamily = character(length(famlist)), commonFamily = character(length(famlist)), stringsAsFactors = FALSE)
# for (j in 1:length(famlist))
# {
#   line <- famlist[j]
#   sci_section <- strsplit(line, " - ")[[1]][1]
#   sciname <- gsub("\\*|Family |\\[|\\]|:","",sci_section)
#   com_section <- strsplit(line, " - ")[[1]][2]
#   comname <- gsub( "\\[|\\]", "", com_section) #removes the brackets
#   retreivedNames[j, 1] <- sciname
#   retreivedNames[j, 2] <- comname
# }
# return(retreivedNames)
# }
# 
# turtleFamilies <- forReptiles(anapsidurl)
# #corrections
# turtleFamilies$commonFamily[which(turtleFamilies$scientificFamily == "Chelydridae")] <- "snapping turtles" #the scraped name is "common snapping turtles and alligator snapping turtle"
# turtleFamilies <- turtleFamilies[-which(turtleFamilies$scientificFamily == "Sanke"),] #not a turtle familiy, some Wikipedia error
# turtleFamilies <- rbind(turtleFamilies,c("Platysternidae", "Big-headed turtle")) #excluded from the list. Not actually in California  but as long as I noticed the error why not fix it.
# 
# diapsidFamilies <- forReptiles(diapsidurl)
# #fixing errors
# diapsidFamilies$commonFamily[which(diapsidFamilies$scientificFamily == "Sphenodontidae")] <- "tuataras"
# diapsidFamilies$commonFamily[which(diapsidFamilies$scientificFamily == "Agamidae")] <- "agamas"
# diapsidFamilies$commonFamily[which(diapsidFamilies$scientificFamily == "Iguanidae ")] <- "iguanas and relatives"
# diapsidFamilies$scientificFamily[which(diapsidFamilies$scientificFamily == "Iguanidae ")] <- "Iguanidae" #trailing space removed
# diapsidFamilies$commonFamily[which(diapsidFamilies$scientificFamily == "Teiidae")] <- "whiptails and tegus"
# diapsidFamilies$commonFamily[which(diapsidFamilies$scientificFamily == "Boidae")] <- "boas and anacondas"
# 
# #amphibians
# salamanderURL <- "https://en.wikipedia.org/w/index.php?title=Salamander&action=edit&section=14" #edit page for https://en.wikipedia.org/wiki/Salamander#Taxonomy
# frogURL <- "https://en.wikipedia.org/w/index.php?title=List_of_Anuran_families&action=edit&section=2" #edit page for https://en.wikipedia.org/wiki/List_of_Anuran_families#Families
# 
# forFrogs <- function(frogURL)
#   {
# a <- readLines(frogURL)
# 
# famlist <- NULL
# for (i in 1:length(a))
# {
#   if (gregexpr("*idae\\]\\]",a[i])[[1]][1] != -1) {famlist = rbind(famlist, a[i])} #pulls lines that begin with "family
# }
# 
# retreivedNames <- data.frame(scientificFamily = character(length(famlist)), commonFamily = character(length(famlist)), stringsAsFactors = FALSE)
# for (j in 1:length(famlist))
# {
#   line <- famlist[j]
#   sci_section <- strsplit(line, "\\|\\|")[[1]][1]
#   sci_section <- regmatches(sci_section, gregexpr("\\[\\[.*?\\]\\]", sci_section))[[1]][1]
#   sciname <- gsub( "\\[|\\]", "", sci_section)
#   comname<- strsplit(line, "\\|\\|")[[1]][3]
#   retreivedNames[j, 1] <- sciname
#   retreivedNames[j, 2] <- comname
# }
# return(retreivedNames)
# }
# 
# 
# forSalamanders <- function(salamanderURL)
# {
#   a <- readLines(salamanderURL)
#   
#   famlist <- NULL
#   for (i in 1:length(a))
#   {
#     if (gregexpr("*idae\\]\\]",a[i])[[1]][1] != -1) {famlist = rbind(famlist, a[i])} #pulls lines that begin with "family
#   }
#   
#   retreivedNames <- data.frame(scientificFamily = character(length(famlist)), commonFamily = character(length(famlist)), stringsAsFactors = FALSE)
#   for (j in 1:length(famlist))
#   {
#     line <- famlist[j]
#     sci_section <- strsplit(line, "\\|\\|")[[1]][1]
#     sci_section <- regmatches(sci_section, gregexpr("\\[\\[.*?\\]\\]", sci_section))[[1]][1]
#     sciname <- gsub( "\\[|\\]", "", sci_section)
#     comname<- strsplit(line, "\\|\\|")[[1]][2]
#     retreivedNames[j, 1] <- sciname
#     retreivedNames[j, 2] <- comname
#   }
#   return(retreivedNames)
# }
# 
# caecilianURL <- "https://en.wikipedia.org/w/index.php?title=List_of_amphibians&action=edit&section=10" #edit page for https://en.wikipedia.org/wiki/List_of_amphibians#Order_Gymnophiona:_Caecilian
# forCae <- function(caecilianURL){
#   c <- readLines(caecilianURL)
#   famlist <- NULL
#   for (i in 1:length(m))
#   {
#     if (gregexpr("Family \\[\\[",c[i])[[1]][1] != -1) {famlist = rbind(famlist,c[i])} #pulls lines that begin with "family
#   }
#   
#   retreivedNames <- data.frame(scientificFamily = character(length(famlist)), commonFamily = character(length(famlist)), stringsAsFactors = FALSE)
#   for (j in 1:length(famlist))
#   {
#     line <- famlist[j]
#     sci_section <- strsplit(line, " - ")[[1]][1]
#     sciname <- gsub("\\*|Family |\\[|\\]","",sci_section)
#     com_section <- strsplit(line, " - ")[[1]][2]
#     comname <- strsplit(com_section, ", [0-9]")[[1]][1]
#     retreivedNames[j, 1] <- sciname
#     retreivedNames[j, 2] <- comname
#   }
#   return(retreivedNames)
# }
# 
# frogs <- forFrogs(frogURL)
# sal <- forSalamanders(salamanderURL)
# cae <- forCae(caecilianURL)
# 
# #fixing
# frogs$commonFamily[which(frogs$commonFamily == "-")] <- NA
# 
# #bind all herps
# herpFamilies <- rbind(turtleFamilies,diapsidFamilies,frogs,sal,cae)
# write.csv(herpFamilies,"processed_data/herp_common_families.csv", row.names = FALSE)
# 
# 
# #### Where the mess starts in earnest####
# ####Plants####
# #I guess start with the list of tree families, and the list of most-speciose angiosperms.
# gymnospermTreeURL <- "https://en.wikipedia.org/w/index.php?title=List_of_trees_and_shrubs_by_taxonomic_family&action=edit&section=1" #edit page for https://en.wikipedia.org/wiki/List_of_trees_and_shrubs_by_taxonomic_family#Gymnosperms
# angiospermTreeURL <- "https://en.wikipedia.org/w/index.php?title=List_of_trees_and_shrubs_by_taxonomic_family&action=edit&section=13" #edit page for https://en.wikipedia.org/wiki/List_of_trees_and_shrubs_by_taxonomic_family#Angiosperms
# commonFloweringURL <- "https://en.wikipedia.org/w/index.php?title=Flowering_plant&action=edit&section=9" #edit page for https://en.wikipedia.org/wiki/Flowering_plant#Flowering_plant_diversity
# 
# forTrees <- function(treeURL)
# {
#   t <- readLines(treeURL)
#   famlist <- NULL
#   for (i in 1:length(t))
#   {
#     if (gregexpr("====\\[\\[",t[i])[[1]][1] != -1) {famlist = rbind(famlist, t[i])} #pulls lines that begin with "family
#   }
#   
#   retreivedNames <- data.frame(scientificFamily = character(length(famlist)), commonFamily = character(length(famlist)), stringsAsFactors = FALSE)
#   for (j in 1:length(famlist))
#   {
#     line <- famlist[j]
#     sci_section <- strsplit(line, ": ")[[1]][1]
#     sciname <- gsub("\\[|\\]|=","",sci_section)
#     com_section <- strsplit(line, ": ")[[1]][2]
#     comname <- gsub( "=", "", com_section)
#     retreivedNames[j, 1] <- sciname
#     retreivedNames[j, 2] <- comname
#   } 
#   return(retreivedNames)
# }
# 
# gymnTrees <- forTrees(gymnospermTreeURL)
# angioTrees <- forTrees(angiospermTreeURL)
# 
# forDiverseAngio<- function(commonFloweringURL)
# {
#   f <- readLines(commonFloweringURL)
#   famlist <- NULL
#   for (i in 1:length(f))
#   {
#     if (gregexpr("aceae\\]\\]",f[i])[[1]][1] != -1) {famlist = rbind(famlist, f[i])} 
#   }
#   
#   retreivedNames <- data.frame(scientificFamily = character(length(famlist)), commonFamily = character(length(famlist)), stringsAsFactors = FALSE)
#   for (j in 1:length(famlist))
#   {
#     line <- famlist[j]
#     line <- strsplit(line, ": ")[[1]][1]
#     line <- gsub("\\]\\] or.*?\\(", "\\]\\] \\(", line)
#     sci_section <- strsplit(line,"\\]\\] \\(")[[1]][1]
#     sciname <- gsub("\\[|\\]|#| ","",sci_section)
#     
#     com_section <- strsplit(line,"\\]\\] \\(")[[1]][2]
#     com_section <- gsub(".*?\\|", "", com_section)
#     comname <- gsub( "\\[|\\]|\\(|\\)", "", com_section)
#     retreivedNames[j, 1] <- sciname
#     retreivedNames[j, 2] <- comname
#   } 
#   return(retreivedNames)
# }
# 
# diverseAngio <- forDiverseAngio(commonFloweringURL)
# 
# 
# 
# 
# 
# ####Animal phyla and classes####
# animalURL <- "https://en.wikipedia.org/w/index.php?title=List_of_animal_classes&action=edit&section="
# #Also list of chordate orders https://en.wikipedia.org/wiki/List_of_chordate_orders 
# 
# #animalClasses <- function(animalURL){ #https://en.wikipedia.org/wiki/List_of_animal_classes
# for (z in 1:50) #about the number of sections in the article
# {
#   group <- readLines(paste0(animalURL,z))
#   phylum <- 
# }
# 
# classlist <- NULL
# for (k in 1:length(inv))
# {
#   if (gregexpr("\\*\\[\\[",inv[k])[[1]][1] != -1) {classlist = rbind(classlist, inv[k])}
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# }
# #####
# <- "http://calphotos.berkeley.edu/fauna/com-Invertebrate-Insect.html"
# 
# insecturl <- "http://nrs.ucdavis.edu/mcl/spcecies/insect_list.pdf"
# insects <- parkObs[which(parkObs$class == "Insecta"),]
# 
# listInsectOrders <- as.character(sort(unique(insects$order)))
# #forInsectOrders <-
# 
# insectorders <- readLines(insecturl) #https://en.wikipedia.org/w/index.php?title=Template:Orders_of_Insects&action=edit
# famlist = NULL
# for (j in 1:length(listInsectOrders))
# {
#   for (i in 1:length(insectorders))
# {
#   if (gregexpr(listInsectOrders[j],insectorders[i])[[1]][1] != -1) {famlist = rbind(famlist, insectorders[i])} #pulls lines that contain an insect order name
#   }
# }
# 
# 
# #Angiosperms: https://en.wikipedia.org/w/index.php?title=Flowering_plant&printable=yes#Flowering_plant_diversity
# 
# 
# 
# 
# #birds: 
# 
# 
# #birds: Neognathae from https://en.wikipedia.org/w/index.php?title=List_of_birds&action=edit&section=10
# 
# n <- readLines("raw_data/families/Neognathae.txt")
# 
# pos = substr('*tidae', n)
# 
# 
# 
# 
# for (j in 1:length(famlist))
# {
#   
# }
# try <- famlist[1:10]
# 
# listfix <- gsub("\\[|\\]|\\*", "", famlist)
# 
# scientific <- 
#   
# 
# 
# 
# tryfix <- listfix[1:10]
# strsplit(" [A-z]idae:", tryfix)
# 


