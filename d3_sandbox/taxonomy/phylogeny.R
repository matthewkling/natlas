
library(rgdal)
library(dplyr)
library(raster)
library(data.table)
library(jsonlite)


setwd("~/documents/inviz/taxorama/d3_sandbox/taxonomy")

# load inat records
#d <- read.csv("observations-15832.csv", stringsAsFactors=F)
d <- fread("~/documents/inviz/gbif-observations-dwca/observations.csv")
d <- filter(d, 
            is.finite(decimalLatitude),
            countryCode=="US")

# taxonomic data frame
d <- d %>%
      filter(taxonRank=="species",
             kingdom %in% c("Animalia", "Fungi", "Plantae")) %>%
      mutate(species=regexpr(" ", scientificName),
             species=substr(scientificName, species+1, nchar(scientificName))) %>%
      group_by(kingdom, phylum, class, order, family, genus, species) %>%
      summarize(n_records=n()) %>%
      arrange(kingdom, phylum, class, order, family, genus, species) %>%
      as.data.frame() 

# target json structure: https://bl.ocks.org/mbostock/5944371
# at any level, each taxon is a list of two elements: a name, and a list of children.


# for expediency during testing
#d <- filter(d, family=="Accipitridae")
d <- filter(d, class=="Aves")

# leaf-level nodes have data on species and frequency
p <- lapply(1:nrow(d), function(i) data.frame(name=d$species[i], size=d$n_records[i]))

# function aggregates taxa list into parent clades
group_taxa <- function(data, groupings){
      parents <- unique(groupings[,1])
      lapply(parents, function(x){
            children <- which(groupings[,1]==x)
            list(name=x, children=data[children])
      })
}

# apply over all taxonomic levels, generating tree-like list
levels <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
for(level in rev(levels)[2:length(levels)]){
      if(level=="species") next()
      child_level <- levels[match(level, levels)+1]
      p <- group_taxa(p, d[,c(level, child_level)])
}



### convert to JSON format

# serialize to json
json <- toJSON(p)  

# remove brackets from taxon names to match target format
jsonn <- gsub('name":\\[', 'name":', json)
jsonn <- gsub('\\],"children', ',"children', jsonn)

jsonn <- gsub('\\[\\[', '[', jsonn)
jsonn <- gsub('\\]\\]', '\\]', jsonn)
jsonn <- gsub('\\],\\[', ',', jsonn)

# remove outermost brackets to match target format
jsonn <- substr(jsonn, 2, nchar(jsonn)-1)

# format and export
jsonn <- prettify(jsonn)
write(jsonn, "taxonomy.json")
