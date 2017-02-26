
library(rgdal)
library(dplyr)
library(raster)
library(data.table)


setwd("~/documents/d3/e1")


# load counties shapefile and crop to US48
s <- readOGR("cb_2015_us_county_5m", "cb_2015_us_county_5m")
e <- extent(-126, -66, 25, 49)
s <- crop(s, e)

# load inat records
#d <- read.csv("observations-15832.csv", stringsAsFactors=F)
d <- fread("~/documents/inviz/gbif-observations-dwca/observations.csv")
d <- filter(d, 
            is.finite(decimalLatitude),
            countryCode=="US",
            kingdom %in% c("Animalia", "Fungi", "Plantae"))
ddd <- d

coordinates(d) <- c("decimalLongitude", "decimalLatitude")
crs(d) <- crs(s)
d <- crop(d, e)

# spatial overlay
o <- over(d, s)
o <- cbind(coordinates(d), d@data, o[,c("STATEFP", "COUNTYFP", "COUNTYNS", "NAME", "ALAND")])

# summarize by species-county
sc <- filter(o, 
             out_of_range=="false",
             quality_grade=="research") %>%
      mutate(FIPS=paste0(STATEFP, COUNTYFP)) %>%
      group_by(FIPS, kingdom, scientific_name) %>%
      summarize(n=n()) %>%
      group_by(FIPS) %>%
      summarize(n_spp=n()) %>%
      mutate(FIPS=as.character(FIPS))

# add missing (zero obs) counties
sc <- s@data %>%
      mutate(FIPS=paste0(STATEFP, COUNTYFP)) %>%
      dplyr::select(FIPS) %>%
      full_join(sc) %>%
      mutate(n_spp = ifelse(is.na(n_spp), 0, n_spp)) %>%
      arrange(FIPS) %>%
      mutate(id=FIPS) %>%
      dplyr::select(-FIPS)

# load unemployment data for comparison
u <- read.table("~/documents/inviz/sandbox/unemployment.tsv", header=T, colClasses="character")

setdiff(sc$id, u$id)
setdiff(u$id, sc$id)

sc <- filter(sc, id != "NANA")

sc <- u %>%
      dplyr::select(id) %>%
      full_join(sc) %>%
      mutate(n_spp = ifelse(is.na(n_spp), 0, n_spp)) %>%
      arrange(id)

write.csv(sc, "~/documents/inviz/sandbox/inat_counties.csv", row.names=F)



##########
##########

# taxonomic hierarchy
d <- ddd %>%
      filter(taxonRank=="species",
             kingdom %in% c("Animalia", "Fungi", "Plantae")) %>%
      mutate(species=regexpr(" ", scientificName),
             species=substr(scientificName, species+1, nchar(scientificName))) %>%
      group_by(kingdom, phylum, class, order, family, genus, species) %>%
      summarize(n_records=n()) %>%
      arrange(kingdom, phylum, class, order, family, genus, species) %>%
      as.data.frame() 

#target json structure: https://bl.ocks.org/mbostock/5944371
# at any level, each taxon is a list of two elements: a name, and a list of children.

# leaf-level nodes have data on species and frequency
p <- lapply(1:nrow(d), function(i) list(name=d$species[i], size=d$n_records[i]))

# function aggregates taxa list into parent clades
group_taxa <- function(data, groupings){
      #data <- p
      #groupings <- d[,c("genus", "species")]
      
      parents <- unique(groupings[,1])
      lapply(parents, function(x){
            #x <- parents[10]
            children <- which(groupings[,1]==x)
            list(name=x, children=data[children])
      })
}

# apply over all taxonomic levels, generating tree-like list
levels <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
for(level in rev(levels)){
      if(level=="species") next()
      child_level <- levels[match(level, levels)+1]
      p <- group_taxa(p, d[,c(level, child_level)])
}

# serialize to json, and indent
json <- prettify(toJSON(p))
write(json, "taxonomy.json")




#########

phylo <- list(name="life",
              children=list())

locate_parent <- function(taxon){
      
}

levels <- c("life", "kingdom", "phylum", "class", "order", "family", "genus", "species")
for(level in levels[2:length(levels)]){
      parent_level <- levels[match(level, levels)-1]
      child_level <- levels[match(level, levels)+1]
      
      taxa <- d[[level]]
      
      parent <- d[,parent_level][d[,level]==taxon]
}


for(k in unique(d$kingdom)){
      new <- list(list(name=k,
                  children=list()))
      phylo[phylo$name=="life"][["children"]] <- c(phylo[phylo$name=="life"][["children"]], new)
      
      for(p in unique(d$phylum[d$kingdom==k])){
            new <- list(list(name=p,
                             children=list()))
            phylo[phylo$name=="life"][["children"]] <- c(phylo[phylo$name=="life"][["children"]], new)
      }
      
}

str(phylo)





hc <- hclust(dist(d))



library(jsonlite)
# recreate your data.frame
DF <- 
      data.frame(V1=c(717374788,429700970),
                 V2=c(694405490, 420694891),
                 V3=c(606978836,420694211),
                 V4=c(578345907,420792447),
                 V5=c(555450273,420670045))

# transform the data.frame into the described structure
idsIndexes <- which(names(DF) != 'V1')
a <- lapply(1:nrow(DF),FUN=function(i){ 
      list(V1=list(id=DF[i,'V1']),
           results=lapply(idsIndexes,
                          FUN=function(j)list(id=DF[i,j])))
})

# serialize to json
txt <- toJSON(a)
# if you want, indent the json
txt <- prettify(txt)


