
library(dplyr)
library(jsonlite)
#setwd("..")
setwd("~/documents/inviz/taxorama")

# load data
park_id = "PORE" #Point Reyes is park of interest
spp <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_species_list.csv"), header=T, stringsAsFactors=F)

f <- spp %>%
      mutate(species=speciesFixed,
             category=tolower(category),
             root="life") %>%
      distinct() %>%
      arrange(category, family, species) %>%
      as.data.frame() 



# add colors
colors <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_species_colors.csv"), stringsAsFactors=F) %>%
      mutate(species=binomial) %>% select(-binomial)
f <- left_join(f, colors)
f$hex <- f$hex_ecdf # hex_fit or hex_ecdf
f$hex[is.na(f$hex)] <- "#000000"

d <- f
d <- filter(d, 
            total_obs>0, 
            !is.na(category),
            nchar(class)>0)
d <- d[!duplicated(d$speciesFixed),]

# generate list of leaf-level species datasets
leaves <- lapply(1:nrow(d), function(i) data.frame(level="species",
                                                   name=d$speciesFixed[i],
                                                   common=ifelse(!is.na(d$genus.common[i]), 
                                                                 d$species.common[i], 
                                                                 "Common name unknown"),
                                                   inat_id=d$species.inat.ID[i],
                                                   hex=d$hex[i],
                                                   nspp=1,
                                                   n_records=d$total_obs[i],
                                                   n_users=d$user_obs[i],
                                                   n_species=1))

# function aggregates taxa list into parent clades
group_taxa <- function(data, groupings, level, max_col){
      parents <- as.vector(unique(groupings[,1]))
      lapply(parents, function(x){
            
            kids <- which(groupings[,1]==x)
            kids <- data[kids]
            kids <- kids[!sapply(kids, is.null)]
            
            weights_spp <- sapply(kids, function(x) x$nspp)
            weights_obs <- sapply(kids, function(x) x$nspp)
            
            color <- sapply(kids, function(x) x$hex) %>% col2rgb()
            color <- sapply(1:3, function(x) weighted.mean(color[x,], weights_spp))
            
            common <- d[d[,level] == x, paste0(level, ".common")][1]
            if(is.null(common)) common <- x
            
            inat_id <- d[d[,level] == x, paste0(level, ".inat.ID")][1]
            if(is.null(inat_id)) inat_id <- "NA"
            
            list(name=x,
                 common=common,
                 inat_id=inat_id,
                 level=level, 
                 hex=rgb(color[1], color[2], color[3], maxColorValue=max_col), # mcv/255 ratio controls fade to black
                 nspp=sum(weights_spp),
                 nobs=sum(weights_obs),
                 children=kids)
      })
}

hierarchies <- list(linnean=c("root", "kingdom", "phylum", "class", "order", "family", "genus", "species"),
                    simple=c("root", "category", "family", "species"))

for(hierarchy in names(hierarchies)){
      # construct hierarchy
      levels <- hierarchies[[hierarchy]]
      p <- leaves
      for(level in rev(levels)[2:length(levels)]){
            child_level <- levels[match(level, levels)+1]
            groupings <- distinct(d[,c(level, child_level)])
            groupings <- groupings[apply(groupings, 1, function(x) min(nchar(x))>0),]
            p <- group_taxa(p, groupings, level,
                            max_col=switch(hierarchy, "linnean"=400, "simple"=800))
      }
      
      
      
      ### convert to JSON format
      
      # serialize to json
      json <- toJSON(p, auto_unbox=T)
      jsonn <- json
      
      # remove brackets from level names to match target format
      jsonn <- gsub('\\[\\[', '[', jsonn)
      jsonn <- gsub('\\]\\]', '\\]', jsonn)
      jsonn <- gsub('\\],\\[', ',', jsonn)
      
      jsonn <- prettify(jsonn)
      
      # remove outermost brackets to match target format
      jsonn <- stringr::str_trim(jsonn)
      jsonn <- substr(jsonn, 2, nchar(jsonn)-1)
      jsonn <- stringr::str_trim(jsonn)
      
      #write(jsonn, paste0("processed_data/",park_id,"_data/",park_id,"_taxonomy_", hierarchy, ".json"))
      write(jsonn, paste0("d3_master/taxonomy_pore_", hierarchy, ".json"))
}

