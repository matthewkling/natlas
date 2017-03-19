
library(dplyr)
library(jsonlite)
#setwd("..")
setwd("~/documents/inviz/taxorama")

# load data
park_id = "PORE" #Point Reyes is park of interest
spp <- read.csv(paste0("processed_data/",park_id,"_data/",park_id,"_species.csv"), header=T, stringsAsFactors=F)


f <- spp %>%
      #mutate(species=regexpr(" ", speciesFixed),
      #       species=substr(speciesFixed, species+1, nchar(speciesFixed))) %>%
      mutate(species=speciesFixed,
             category=tolower(category)) %>%
      group_by(category, family, species) %>%
      arrange(category, family, species) %>%
      as.data.frame() 

d <- f
#d <- ungroup(f) %>% group_by(category) %>% sample_n(10) %>% as.data.frame()
d <- filter(d, total_obs>0, !is.na(category))
d$Common.Names <- gsub("'", "", d$Common.Names)

# generate list of leaf-level species datasets
leaves <- lapply(1:nrow(d), function(i) data.frame(level="species",
                                                   name=d$speciesFixed[i],
                                                   common_name=ifelse(!is.na(d$Common.Names[i]), d$Common.Names[i], "godzilla"),
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
group_taxa <- function(data, groupings, level){
      parents <- as.vector(unique(groupings[,1]))
      lapply(parents, function(x){
            children <- which(groupings[,1]==x)
            list(level=level, name=x, children=data[children])
      })
}

# apply over all levels of hierarchy, generating tree-like list
levels <- c("category", "family", "species")
p <- leaves
for(level in rev(levels)[2:length(levels)]){
      child_level <- levels[match(level, levels)+1]
      p <- group_taxa(p, distinct(d[,c(level, child_level)]), level)
}

### convert to JSON format

# serialize to json
json <- toJSON(p, auto_unbox=T)
jsonn <- json

# remove empty lists
#jsonn <- gsub(',\\{\\}', '', jsonn)

# remove brackets from level names to match target format
jsonn <- gsub('\\[\\[', '[', jsonn)
jsonn <- gsub('\\]\\]', '\\]', jsonn)
jsonn <- gsub('\\],\\[', ',', jsonn)

jsonn <- prettify(jsonn)

# remove outermost brackets to match target format
#jsonn <- stringr::str_trim(jsonn)
#jsonn <- substr(jsonn, 4, nchar(jsonn)-3)
#jsonn <- stringr::str_trim(jsonn)

jsonPath <- paste0("processed_data/",park_id,"_data/",park_id,"_taxonomy.json")
write(jsonn, jsonPath)
jsonPath <- "d3_sandbox/taxonomy/taxonomy_pore.json"
write(jsonn, jsonPath)
