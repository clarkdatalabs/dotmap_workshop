## Load package libraries 
library("maptools")
library("sp")
library("rgeos")
library("rgdal")
library("Cairo")

#Dot Map Project in R
# an R implementation of https://github.com/unorthodox123/RacialDotMap
# created by: Soon Ju Kim and Justin Joque, University of Michigan

# ***** means possible parallelization

# Initial settings
tile.size= 256; 
initial.res= (2*pi*6378137)/tile.size; 
origin.shift= (2*pi*6378137)/as.double(2.0); 
resolution = function(zoom){
  return(initial.res/(2^zoom))
}

# Read Shapefile, Convert to Coordinates                                        # shapefile comes with coords, what exactly are we trying to do?
totalcoordstate = function(state) {
  # state is a spatial polygons df
  withpop = state[state$POP10 != 0, ]
  pops = withpop$POP10
  result = vector('list', nrow(withpop))
  for (i in 1:nrow(withpop)){                                                   # ***** double parallel possibility here
    result[[i]] = sapply(pops[i], function(j) spsample(withpop[i,], n=j, type='random',iter=15)) # ***** e.g. with pvec or parSapply
  }
  sp::SpatialPoints(do.call('rbind', sapply(result, coordinates)))              # should shave about 20 secs on Vermont
}


coordstoMeters= function (coords, origin.shift){ 
  mx= coords$x * origin.shift / 180.0;
  my1= log(tan((90 + coords$y) * pi / 360.0)) / (pi / 180.0);
  my= my1 * origin.shift / 180.0;
  return (data.frame(mx,my));
}

# Meters to Pixels
meterstoPixels = function (meters, zoom, origin.shift){
  #calculate resolution, given zoom:                                            # there are pixels and gridtopology classes in sp that might work here,
  res = resolution(zoom)                                                        # but my contextual knowledge is lacking at this point
  data.frame(px = meters$mx / res + origin.shift / res,
             py = meters$my / res + origin.shift / res)
}

# Pixels to Meters
pixelstoMeters = function(px, py, zoom, origin.shift){
	res = resolution(zoom)
	mx = px * res - origin.shift
	my = py * res - origin.shift
	return(c(mx,my))                                                              # not presently used, but is a cbind desired here?
}

pixelstoTiles= function (pixels, tile.size){
  tiles= data.frame(tx= numeric(nrow(pixels)), ty=numeric(nrow(pixels)));
  tiles$tx= ceiling(pixels[[1]] / as.double(tile.size)) - 1;
  tiles$ty= ceiling(pixels[[2]] / as.double(tile.size)) - 1;
  return(tiles);
}

# Tile to Quadkey
tilestoQuadkey = function(tiles, zoom) {                                       # this function I don't have much context.
  # tiles is data.frame
  #Convert TMS tile coordinates to Quadtree
  quadkey = ""
  tiles[2] = (2 ^ (zoom) - 1) - tiles[2]
  quad = function(zoomlevel){
    digit = 0
    mask = 1 * (2 ^ (zoomlevel - 1))                # bitwise shift 1 to the left by i-1 amount; 
    if ((bitwAnd(tiles[1], mask) != 0)) {
      digit = digit + 1                                                      
    }
    if ((bitwAnd(tiles[2], mask) != 0)) {
      digit = digit + 2
    }
    quadkey = paste0(quadkey, digit)
  }
  paste0(sapply(zoom:0, quad), collapse='')                                     # *****  pvec
}



#returns tms value
googleTiles = function(googleTile, level){
	return(c(googleTile[1], 2^level - 1 - googleTile[2]))
}

#quadkey to tile
quadkeytoTiles = function (quadkey){
	tileX = 0
	tileY = 0
	levelofDetail = nchar(quadkey)

	for (i in levelofDetail:1){
		mask = bitwShiftL(1,i-1)
		char = substr(quadkey,levelofDetail - i + 1,levelofDetail - i + 1)
		if (char == '1') {tileX = bitwOr(tileX,mask)}
		if (char == '2') {tileY = bitwOr(tileY,mask)}
		if (char == '3') {
			tileX = bitwOr(tileX,mask)
			tileY = bitwOr(tileY,mask)
		}
	}

	return(c(tileX,tileY,levelofDetail))
}

tilebounds = function(tileX, tileY, levelofDetail,orgiin.shift){
	ll = pixelstoMeters(tileX * tile.size, tileY * tile.size, levelofDetail,origin.shift)
	ur = pixelstoMeters((tileX+1) * tile.size, (tileY+1) * tile.size, levelofDetail,origin.shift)
	return(c(ll[1],ll[2],ur[1],ur[2]))
}

# Draw tiles from quadkey reference
draw.tile =function(quadkey){
  A = 1000	
  width = 512
  zoomlevel=nchar(quadkey)
  google_tile = quadkeytoTiles(quadkey)
  tms_tile = googleTiles(google_tile,zoomlevel)
  bounds = tilebounds(tms_tile[1],tms_tile[2],zoomlevel,origin.shift)

  tile_ll = bounds[1] / A
  tile_bb = bounds[2] / A
  tile_rr = bounds[3] / A
  tile_tt = bounds[4] / A

  xscale = width/(tile_rr - tile_ll)
  yscale = width/(tile_tt - tile_bb)
  scale = min(c(xscale,yscale))

  #quad.coord$quadshort = substring(quad.coord$quadkey,1,zoomlevel)
  coords = quad.coord[quad.coord$quadzoom == quadkey,]

  coords$mx = (coords$meters.mx/A - tile_ll) * scale
  coords$my = (coords$meters.my/A - tile_tt) * -scale


  dir.create(paste(zoomlevel,"/",sep=""),showWarnings=FALSE)
  dir.create(paste(zoomlevel,"/",tms_tile[1],"/",sep=""),showWarnings=FALSE)
  CairoPNG(file = paste(zoomlevel,"/",tms_tile[1],"/",tms_tile[2],".png",sep=""), width=512, height=512, bg = "transparent")
  #plot to the corners
  par(mar=c(0,0,0,0))
  plot(coords$mx,coords$my,pch=20,xlim=c(1,512),ylim=c(512,1),cex=dot_size(zoomlevel),col=rgb(0,0,0,dot_opac(zoomlevel)/255), axes=FALSE,xaxs="i",yaxs="i",bty="n")
  dev.off()
}

dot_size <- function(zoom) {
  switch(as.character(zoom),
         "4"=0.1,"5"=0.1,"6"=0.1,"7" = 0.1,"8"=0.1,"9"=0.2,"10"=0.2,"11"=0.2,"12"=0.2,"13"=0.2,"14"=0.2)
}

dot_opac <- function(zoom) {
  switch(as.character(zoom),
         "4"=153,"5"=153,"6"=179,"7" = 179,"8"=204,"9"=204,"10"=230,"11"=230,"12"=255,"13"=255,"14"=255)
}
