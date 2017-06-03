
# get common names for each species, and export as a csv for use elsewhere

library(dplyr)
library(taxize)
setwd("~/documents/inviz/taxorama")

# load species list data
park_id = "PORE" #Point Reyes is park of interest
spp <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_species.csv"), header=T, stringsAsFactors=F)
spp <- unique(spp$speciesFixed)


#### SPECIES COMMON NAMES

# query common names online. warning: this taks a LONG time.
comm_ncbi <- lapply(f$speciesFixed, function(x) try(sci2comm(x, db="ncbi")))
comm_itis <- lapply(f$speciesFixed, function(x) try(sci2comm(x, db="itis")))

comm_eol <- lapply(f$speciesFixed, function(x) try(sci2comm(x, db="eol")))
saveRDS(comm_eol, "raw_data/PORE_common_names_EOL.rds")

# cleanup. many species have multiple common names; select just the first one
n <- lapply(comm, function(x) ifelse(class(x)=="try-error", "unknown", x))
n <- sapply(n, function(x) x[[1]]) 
n <- data.frame(binomial=spp, common_name=n)

# save results


#### FAMILIES

