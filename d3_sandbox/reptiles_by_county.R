
library(rgdal)
library(dplyr)
library(raster)

setwd("~/documents/d3/e1")

# load counties shapefile and crop to US48
s <- readOGR("cb_2015_us_county_5m", "cb_2015_us_county_5m")
e <- extent(-126, -66, 25, 49)
s <- crop(s, e)

# load inat records
d <- read.csv("observations-15832.csv", stringsAsFactors=F)
d <- filter(d, is.finite(latitude))
coordinates(d) <- c("longitude", "latitude")
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
      group_by(FIPS, scientific_name) %>%
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
