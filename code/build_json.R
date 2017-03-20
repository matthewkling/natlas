
library(dplyr)
library(jsonlite)
#setwd("..")
setwd("~/documents/inviz/taxorama")

# load data
park_id = "PORE" #Point Reyes is park of interest
spp <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_species.csv"), header=T, stringsAsFactors=F)


f <- spp %>%
      mutate(species=speciesFixed,
             category=tolower(category),
             root="life") %>%
      distinct() %>%
      #group_by(category, family, species) %>%
      arrange(category, family, species) %>%
      as.data.frame() 

# add species-level common names from our dictionary


# TODO: add family-level common names


# add colors
colors <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_species_colors.csv"), stringsAsFactors=F) %>%
      mutate(species=binomial) %>% select(-binomial)
f <- left_join(f, colors)
f$hex <- f$hex_ecdf # hex_fit or hex_ecdf
f$hex[is.na(f$hex)] <- "#000000"

d <- f
#d <- ungroup(f) %>% group_by(category) %>% sample_n(5) %>% as.data.frame()
d <- filter(d, 
            total_obs>0, 
            !is.na(category),
            nchar(class)>0)
#d$Common.Names <- gsub("'", "", d$Common.Names)

# generate list of leaf-level species datasets
leaves <- lapply(1:nrow(d), function(i) data.frame(level="species",
                                                   name=d$Common.Names[i],
                                                   binomial=d$speciesFixed[i],
                                                   common_name=ifelse(!is.na(d$Common.Names[i]), d$Common.Names[i], "[common name unknown]"),
                                                   hex=d$hex[i],
                                                   nspp=1,
                                                   n_records=d$total_obs[i],
                                                   n_users=d$user_obs[i],
                                                   n_species=1))

# m1=d$m1[i],
# m2=d$m2[i],
# m3=d$m3[i],
# m4=d$m4[i],
# m5=d$m5[i],
# m6=d$m6[i],
# m7=d$m7[i],
# m8=d$m8[i],
# m9=d$m9[i],
# m10=d$m10[i],
# m11=d$m11[i],
# m12=d$m12[i],
# y2007=d$y2007[i],
# y2008=d$y2008[i],
# y2009=d$y2009[i],
# y2010=d$y2010[i],
# y2011=d$y2011[i],
# y2012=d$y2012[i],
# y2013=d$y2013[i],
# y2014=d$y2014[i],
# y2015=d$y2015[i],
# y2016=d$y2016[i],
# y2017=d$y2017[i],
# npsOccurence = d$Occurrence[i]))



# function aggregates taxa list into parent clades
group_taxa <- function(data, groupings, level, max_col){
      parents <- as.vector(unique(groupings[,1]))
      lapply(parents, function(x){
            
            kids <- which(groupings[,1]==x)
            kids <- data[kids]
            kids <- kids[!sapply(kids, is.null)]
            
            weights <- sapply(kids, function(x) x$nspp)
            
            color <- sapply(kids, function(x) x$hex) %>% col2rgb()
            color <- sapply(1:3, function(x) weighted.mean(color[x,], weights))
            
            list(name=x, 
                 level=level, 
                 hex=rgb(color[1], color[2], color[3], maxColorValue=max_col), # mcv/255 ratio controls fade to black
                 nspp=sum(weights),
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
      
      write(jsonn, paste0("processed_data/",park_id,"_data/",park_id,"_taxonomy_", hierarchy, ".json"))
      write(jsonn, paste0("d3_sandbox/taxonomy/taxonomy_pore_", hierarchy, ".json"))
}

