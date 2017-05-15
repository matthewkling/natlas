
# clip raw US coastline to SF bay area

library(rgdal)
library(raster)

setwd("~/documents/inviz/taxorama/raw_data")
coast <- readOGR("coastline_shapefile", "coastline_nad27")

sf <- extent(-126, -117, 35, 39)
coast <- crop(coast, sf)

writeOGR(coast, dsn="coastline_shapefile", layer="coastline_bay_area", driver="ESRI Shapefile")
