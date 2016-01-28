#Dot Map Project in R
#This R script contains:
# 1. Function that read shape and outputs data with coordinates and corresponding quadkey ref.
# 2. Function to draw out tiles according to the data output from 1
# an R implementation of https://github.com/unorthodox123/RacialDotMap
# created by: Soon Ju Kim and Justin Joque, University of Michigan

## Source global map tiles functions
source("globalmaptiles.r")
## Check if packages exist and download them 
if(!(require(maptools))){
  install.packages("maptools");
  require(maptools);
}
if(!(require(sp))){
  install.packages("sp");
  require(sp);
}
if(!(require(rgeos))){
  install.packages("rgeos");
  require(rgeos);
}
if(!(require(rgdal))){
  install.packages("rgdal");
  require(rgdal);
}
## Load package libraries 
library("maptools")
library("sp")
library("rgeos")
library("rgdal")

## 2. Read shape, output data with coordinates and quadkey reference 
# Read zoom level (zoom levels should be from 4 to 13)

zoom= 14;
# Convert coordinates to quadkey
shape1= readShapeSpatial("vermont/tabblock2010_50_pophu.shp")
shape= shape1[1:200,];
#1. Obtain Coordinates 
coords= totalcoordstate(shape); 
#2. Convert to Tiles
meters= coordstoMeters(coords, origin.shift); 
pixels= meterstoPixels(meters, zoom, origin.shift);
tiles= pixelstoTiles(pixels, tile.size); 
#3. Convert to Google Quadkey
quadkey= apply(tiles, 1, tilestoQuadkey, zoom= zoom)
#4. Combine meter coordinates with quadkey values
quad.coord= cbind(quadkey, meters)
#5 draw tiles
zoomlevels = c(2,3,4,5,6,7,8,9,10)
for (i in zoomlevels){
  	quad.coord$quadzoom = substring(quad.coord$quadkey,1,i)
  	quadlevel = unique(quad.coord$quadzoom)
  	print(quadlevel)
  	lapply(quadlevel,draw.tile)
}


