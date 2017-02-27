# this script processes iNat observations into a taxonomic tree in json format

library(rgdal)
library(dplyr)
library(raster)
library(data.table)
library(jsonlite)


setwd("~/documents/inviz/taxorama/d3_sandbox/taxonomy")

# load inat records
#d <- read.csv("observations-15832.csv", stringsAsFactors=F)
f <- fread("~/documents/inviz/gbif-observations-dwca/observations.csv") %>%
      filter(is.finite(decimalLatitude),
             decimalLongitude < -115, ##### temporary -- limit taxa to west coast
            countryCode=="US")

# taxonomic data frame
f <- f %>%
      filter(taxonRank=="species",
             kingdom %in% c("Animalia", "Fungi", "Plantae")) %>%
      mutate(species=regexpr(" ", scientificName),
             species=substr(scientificName, species+1, nchar(scientificName))) %>%
      group_by(kingdom, phylum, class, order, family, genus, species) %>%
      summarize(n_records=n(),
                n_users=length(unique(recordedBy))) %>%
      arrange(kingdom, phylum, class, order, family, genus, species) %>%
      as.data.frame() 

# target json structure: https://bl.ocks.org/mbostock/5944371
# or http://bl.ocks.org/vgrocha/1580af34e56ee6224d33
# at any level, each taxon is a list of two elements: a name, and a list of children.


# for expediency during testing
d <- filter(f, class=="Aves")

# leaf-level nodes have data on species and frequency
p <- lapply(1:nrow(d), function(i) data.frame(name=d$species[i],
                                              level="species",
                                              n_records=d$n_records[i],
                                              n_users=d$n_users[i],
                                              n_species=1))

# function aggregates taxa list into parent clades
group_taxa <- function(data, groupings, level){
      parents <- unique(groupings[,1])
      lapply(parents, function(x){
            children <- which(groupings[,1]==x)
            list(name=x, level=level, children=data[children])
      })
}

# apply over all taxonomic levels, generating tree-like list
levels <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
for(level in rev(levels)[2:length(levels)]){
      if(level=="species") next()
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
write(jsonn, "taxonomy.json")
