
# generate color values for each species via ordination

library(dplyr)
library(cluster)
library(vegan)
library(colormap)

setwd("~/documents/inviz/taxorama")

for(park_id in c("PORE", "GOGA")){
      # load data
      spp <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_species_list.csv"), header=T, stringsAsFactors=F)
      
      f <- spp %>%
            mutate(species=speciesFixed,
                   category=tolower(category),
                   root="life") %>%
            distinct() %>%
            group_by(kingdom, phylum, class, order, family, genus) %>%
            mutate(spnum = scales::rescale(rank(total_obs))) %>%
            as.data.frame() 
      h <- select(f, kingdom:genus, species) %>%
            na.omit() %>%
            mutate_each(funs(factor))
      binomials <- h$species
      
      # distances
      dm <- daisy(h, metric="gower")
      
      # ordination
      ord <- metaMDS(as.dist(dm), k=3, trymax=0)
      
      # colors
      col_ecdf <- colors3d(ord$points, trans="ecdf")
      col_fit <- colors3d(ord$points, trans="fit")
      
      # export
      d <- data.frame(binomial=binomials, hex_ecdf=col_ecdf, hex_fit=col_fit)
      write.csv(d, paste0("processed_data/",park_id,"_data/",park_id,"_species_colors.csv"), row.names=F)
}



