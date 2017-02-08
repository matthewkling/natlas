#Checking out the iNat data
library(maps)


####Clean####

cleanTaxon <- function(taxonData){
  
taxonObs <- taxonData[,c("id",
                           "scientific_name", "common_name",
                           "user_id", "user_login",
                           "place_guess","latitude","longitude", "positional_accuracy",
                           "observed_on","time_observed_at","time_zone","geoprivacy",
                           "num_identification_agreements", "num_identification_disagreements", "out_of_range","captive_cultivated",
                           "taxon_kingdom_name", "taxon_phylum_name", "taxon_class_name", "taxon_order_name","taxon_family_name","taxon_genus_name"
                           )]

#data to remove?

  
}


#### Taxa ####
molluskRaw <- read.csv("raw_data/Mollusks.csv",header = T)
mammalRaw <- read.csv("raw_data/Mammals.csv",header = T)

amphibianRaw <- read.csv("raw_data/Amphibians.csv",header = T)

#### I like maps ####
map('county', col = "gray90"); map('state',add = T)
points(taxonObs$longitude, taxonObs$latitude , col = "red", pch = ".")
