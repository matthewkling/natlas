
setwd("~/Desktop/taxorama")

parkObs <- read.csv("processed_data/park_obs.csv",header = T)
parkObs$eventDate <- as.Date(parkObs$eventDate)
parkObs$dateIdentified <- as.Date(parkObs$dateIdentified)

#Adding NPS taxon category, I hope correctly? Their categories are:
# Mammal
# Bird
# Reptile
# Amphibian
# Fish
# Vascular Plant
# Non-vascular Plant
# Crab/Lobster/Shrimp
# Slug/Snail
# Insect
# Other Non-vertebrates
# Fungi
# Chromista
# Bacteria

parkObs$NPScategory <- 0
levels(parkObs$NPScategory) <- c("Mammal","Bird", "Reptile", "Amphibian", "Fish", "Vascular Plant", "Non-vascular Plant", "Crab/Lobster/Shrimp", "Slug/Snail", "Insect", "Other Non-vertebrates", "Fungi", "Chromista", "Bacteria")

parkObs$NPScategory[which(parkObs$class == "Mammalia")] <- "Mammal"
parkObs$NPScategory[which(parkObs$class == "Aves")] <- "Bird"
parkObs$NPScategory[which(parkObs$class == "Reptilia")] <- "Reptile"
parkObs$NPScategory[which(parkObs$class == "Amphibia")] <- "Amphibian"
parkObs$NPScategory[which(parkObs$class == "Insecta")] <- "Insect"
parkObs$NPScategory[which(parkObs$class == "Gastropoda")] <- "Slug/Snail"

parkObs$NPScategory[which(parkObs$order == "Decapoda")] <- "Crab/Lobster/Shrimp"

parkObs$NPScategory[which(parkObs$kingdom == "Fungi")] <- "Fungi"
parkObs$NPScategory[which(parkObs$kingdom == "Bacteria")] <- "Bacteria"
parkObs$NPScategory[which(parkObs$kingdom == "Chromista")] <- "Chromista"

parkObs$NPScategory[which(parkObs$phylum == "Anthocerotophyta")] <- "Non-vascular Plant"
parkObs$NPScategory[which(parkObs$phylum == "Bryophyta")] <- "Non-vascular Plant"
parkObs$NPScategory[which(parkObs$phylum == "Rhodophyta")] <- "Non-vascular Plant"
parkObs$NPScategory[which(parkObs$phylum == "Tracheophyta")] <- "Vascular Plant"





parkObs$NPScategory[which(parkObs$class == "")] <- "Fish"

parkObs$NPScategory[which(parkObs$class == "")] <- "Other Non-vertebrates"

#Overall park users
user_id <- unique(parkObs$recordedBy)

parkUsers <- data.frame(user_id)
parkUsers$total_observations <- NA
parkUsers$active_parks[i]  <- NA
parkUsers$active_states <- NA

parkUsers$species_observed <- NA
parkUsers$genus_observed <- NA
parkUsers$family_observed <- NA
parkUsers$order_observed <- NA
parkUsers$class_observed <- NA
parkUsers$phylum_observed <- NA
parkUsers$kingdom_observed <- NA

parkUsers$first_event_date <- as.Date(NA)
parkUsers$first_id_date <- as.Date(NA)
parkUsers$last_event_date <- as.Date(NA)
parkUsers$activity_period <- NA
userLen <- length(parkUsers$user_id)

for (i in 1:userLen){
  #for (i in 1:10){
  userObs <- parkObs[which(parkObs$recordedBy==parkUsers$user_id[i]),]
  parkUsers$total_observations[i] <- length(unique(userObs$id))
  parkUsers$active_parks[i] <- length(unique(userObs$PARK_CODE))
  parkUsers$active_states[i] <- length(unique(userObs$STATEFP))
  
  parkUsers$species_observed[i]  <- length(unique(userObs$scientificName))
  parkUsers$genus_observed[i]  <- length(unique(userObs$genus))
  parkUsers$family_observed[i]  <- length(unique(userObs$family))
  parkUsers$order_observed[i]  <- length(unique(userObs$order))
  parkUsers$class_observed[i]  <- length(unique(userObs$class))
  parkUsers$phylum_observed[i]  <- length(unique(userObs$phylum))
  parkUsers$kingdom_observed[i]  <- length(unique(userObs$kingdom))
  parkUsers$first_event_date[i]  <- min(userObs$eventDate)
  parkUsers$first_id_date[i]  <- min(userObs$dateIdentified)
  parkUsers$last_event_date[i]  <- max(userObs$eventDate)
  
  percentthrough <- 100*i/userLen
  
  if(i %% 10 == 0){print(paste(percentthrough, "%")); print(summary(parkUsers$total_observations))}
}

#Individual parks; general labels as NPS or Inat for easy park switching


#PORElist <- read.xlsx2("raw_data/park_species_lists/NPSpecies_FullListWithDetails_PORE_20170227115931.xlsx", sheetName = "Sheet1", header = T, startRow=1) #grrrr couldn't get to work, saved as CSV for short-term workaround
fullNPSlist <- read.csv("raw_data/park_species_lists/PORE.csv",header = T)
NPSlist <- fullPORElist[,c(
  "Park.Code",
  "Category","Order","Family",
  "Scientific.Name","Common.Names","Synonyms",
  "Taxon.Record.Status","Park.Accepted","Record.Status","Occurrence",
  "Occurrence.Tags","Nativeness", "Abundance", "NPS.Tags"
)]
inatObs <- parkObs[which(parkObs$PARK_CODE == "PORE"),]


inatSpecies <-  unique(inatObs$scientificName)

inatTax <- data.frame(inatSpecies)
inatTax$kingdom <- NA
inatTax$phylum <- NA
inatTax$class <- NA
inatTax$order <- NA
inatTax$family <- NA
inatTax$genus <- NA
numSp <- length(inatSpecies)

for (i in 1:numSp){
  inatTax$kingdom[i] <- as.character(inatObs$kingdom[match(inatTax$inatSpecies[i],inatObs$scientificName)])
  inatTax$phylum[i] <- as.character(inatObs$phylum[match(inatTax$inatSpecies[i],inatObs$scientificName)])
  inatTax$class[i] <- as.character(inatObs$class[match(inatTax$inatSpecies[i],inatObs$scientificName)])
  inatTax$order[i] <- as.character(inatObs$order[match(inatTax$inatSpecies[i],inatObs$scientificName)])
  inatTax$family[i] <- as.character(inatObs$family[match(inatTax$inatSpecies[i],inatObs$scientificName)])
  inatTax$genus[i] <- as.character(inatObs$genus[match(inatTax$inatSpecies[i],inatObs$scientificName)])
}

summary(NPSlist$Category)
summary(factor(inatTax$class))

#assign categories that match park designation
inatTax$Category <- NA
inatTax$Category[which(inatTax$class == )]

#bay area map
parkShapes <- readOGR("raw_data/park_boundaries","nps_boundary")
map('county', 'california',xlim=c(-123.5,-121.5), ylim=c(37,38.5))
map(parkShapes, col = "darkgreen", boundary = F, xlim=c(-179,-67), add = T, interior = TRUE)
