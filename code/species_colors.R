
# generate color values for each species 

library(dplyr)
library(cluster)
library(vegan)
library(colormap)

setwd("~/documents/inviz/taxorama")

# load data
park_id = "PORE" #Point Reyes is park of interest
spp <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_species.csv"), header=T, stringsAsFactors=F)

f <- spp %>%
      mutate(species=speciesFixed,
             category=tolower(category),
             root="life") %>%
      distinct() %>%
      group_by(category, family, species) %>%
      arrange(category, family, species) %>%
      as.data.frame() 

h <- select(f, kingdom:genus, species) %>%
      na.omit() %>%
      mutate_each(funs(factor))
# consider adding a n_records metric to gradate within genus

# distances
dm <- daisy(h, metric="gower")

# ordination
ord <- metaMDS(as.dist(dm), k=3)

# colors
col <- colors3d(ord$points, trans="ecdf")

# export
d <- data.frame(binomial=h$species, hex=col)
write.csv(d, paste0("processed_data/",park_id,"_data/",park_id,"_species_colors.csv"), row.names=F)
