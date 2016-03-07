#Dot Map Project in R
#This R script contains:
# 1. Function that read shape and outputs data with coordinates and
# corresponding quadkey ref.
# 2. Function to draw out tiles according to the data output from 1
# an R implementation of https://github.com/unorthodox123/RacialDotMap
# created by: Soon Ju Kim and Justin Joque, University of Michigan

begin = Sys.time()
## Source global map tiles functions, includes library load statements
source("globalmaptiles.R")

## Set the maximum zoom level
zoom = 14;

## Read shape, output data with coordinates and quadkey reference
# Convert coordinates to quadkey
print("Reading in shapefiles")

# Create a vector of the FIPS codes
states = c(33,50)

for (state in states){
	shape= readOGR('shapefiles',paste("tabblock2010_",state,"_pophu",sep=""))
	# Take a sample for quick testing
	# shape= shape[1:100,];

	## Obtain Coordinates
	coords= totalcoordstate(shape);

	## Convert to Tiles
	meters= coordstoMeters(coords, origin.shift);
	pixels= meterstoPixels(meters, zoom, origin.shift);
	tiles= pixelstoTiles(pixels, tile.size);

	## Convert to Microsoft Quadkey
	quadkey= apply(tiles, 1, tilestoQuadkey, zoom= zoom)

	## Combine meter coordinates with quadkey values
	quad.hold= data.frame(quadkey, meters$mx, meters$my)
	if(exists("quad.coord")){
		quad.coord = rbind(quad.hold,quad.coord)
	}else{
		quad.coord = quad.hold
	}
	rm(quad.hold)
}

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
  	lapply(quadlevel,draw.tile)
}

## Print tile creation time
end_tiles = Sys.time()
tile_time = end_tiles - end_shape
print("End tile creation")
print(tile_time)
