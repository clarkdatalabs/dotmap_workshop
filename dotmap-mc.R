#Dot Map Project in R
#This R script contains:
# 1. Function that read shape and outputs data with coordinates and
# corresponding quadkey ref.
# 2. Function to draw out tiles according to the data output from 1
# an R implementation of https://github.com/unorthodox123/RacialDotMap
# created by: Soon Ju Kim and Justin Joque, University of Michigan

## load library
library(parallel)

# For multicore, get number of cores from PBS or set manually
# NP = as.numeric(Sys.getenv('PBS_NP'))
NP = 10
options(mc.cores=NP)

begin = Sys.time()
## Source global map tiles functions, includes library load statements
source("globalmaptiles.R")

## Set the maximum zoom level
zoom = 14;

## Read shape, output data with coordinates and quadkey reference 
# Convert coordinates to quadkey
shape= readShapeSpatial("vermont/tabblock2010_50_pophu.shp")
# Take a sample for quick testing
# shape= shape[1:100,];

## Obtain Coordinates
coords= totalcoordstate(shape);

## Convert to Tiles
meters= coordstoMeters(coords, origin.shift); 
pixels= meterstoPixels(meters, zoom, origin.shift);
tiles= pixelstoTiles(pixels, tile.size);
 
## Convert to Microso Quadkey
quadkey= apply(tiles, 1, tilestoQuadkey, zoom= zoom)

## Combine meter coordinates with quadkey values
quad.coord= data.frame(quadkey, meters$mx, meters$my)

## Print time for processing shape file(s)
end_shape = Sys.time()
shape_time = end_shape - begin
print("End shape file processing")
print(shape_time)

## Draw tiles
zoomlevels = c(2:14)
for (i in zoomlevels){
  	quad.coord$quadzoom = substring(quad.coord$quadkey,1,i)
  	quadlevel = unique(quad.coord$quadzoom)
  	print(i)
  	mclapply(quadlevel,draw.tile)
}

## Print tile creation time
end_tiles = Sys.time()
tile_time = end_tiles - end_shape
print("End tile creation")
print(tile_time)

