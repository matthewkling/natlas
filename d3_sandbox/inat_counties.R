
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
#write.csv(o, "inat_counties.csv", row.names=F)

# 
good <- filter(o, out_of_range=="false") %>%
      group_by(scientific_name) %>%
      summarize(n=n())

# summarize by species-county
sc <- filter(o, 
            out_of_range=="false",
            quality_grade=="research") %>%
      group_by(COUNTYNS, scientific_name) %>%
      summarize(n=n()) %>%
      group_by(COUNTYNS) %>%
      summarize(n_spp=n()) %>%
      mutate(COUNTYNS=as.character(COUNTYNS))

# add missing (zero obs) counties
sc <- dplyr::select(s@data, COUNTYNS) %>%
      mutate(COUNTYNS=as.character(COUNTYNS)) %>%
      full_join(sc) %>%
      mutate(n_spp = ifelse(is.na(n_spp), 0, n_spp))


