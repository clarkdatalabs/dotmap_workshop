#Dot Map Project in R
# an R implementation of https://github.com/unorthodox123/RacialDotMap
# created by: Soon Ju Kim and Justin Joque, University of Michigan


# Initial settings
tile.size= 256; 
initial.res= (2*pi*6378137)/tile.size; 
origin.shift= (2*pi*6378137)/as.double(2.0); 
resolution= function(zoom){
  return(initial.res/(2^zoom));
}

# Read Shapefile, Convert to Coordinates
totalcoordstate= function(state){
  withpop = state[state$POP10 != 0,]
  totalblocks= nrow(withpop);
  result= data.frame(xcoord= numeric(), ycoord= numeric()); 
  names(result)= c('x-coord', 'y-coord');
  for (i in 1:totalblocks){
    shape= withpop[i,];
    for (j in 1:shape$POP10){
      pointin = FALSE
      while(!pointin){
        randx= runif(1, shape@bbox[1], shape@bbox[3]);
        randy= runif(1, shape@bbox[2], shape@bbox[4]);
        point = over(SpatialPoints(cbind(randx,randy)), shape, fn=NULL)
        if (is.na(point$STATEFP10) == FALSE){
          result[nrow(result)+1,] = c(randx,randy);
          pointin = TRUE
        }
      } 
    }
  }
  return (result); 
}

# From Lat/Lon to Meters  
coordstoMeters= function (coords, origin.shift){
  meters= data.frame(mx= numeric(nrow(coords)), my= numeric(nrow(coords))); 
  meters$mx= coords[[1]] * origin.shift / 180.0;
  my1= log(tan((90 + coords[[2]]) * pi / 360.0)) / (pi / 180.0);
  meters$my= my1 * origin.shift / 180.0;
  return (meters);
}

# Meters to Pixels
meterstoPixels= function (meters, zoom, origin.shift){
  pixels= data.frame(px= numeric(nrow(meters)), py= numeric(nrow(meters)));
  #calculate resolution, given zoom:
  res = resolution (zoom);
  pixels$px= meters$mx / res + origin.shift / res;
  pixels$py= meters$my / res + origin.shift / res;
  return(pixels);
}

# Pixels to Meters
pixelstoMeters = function(px,py, zoom, origin.shift){
	res = resolution(zoom)
	mx = px * res - origin.shift;
	my = py * res - origin.shift;
	return(c(mx,my));
}

# Pixels to Tiles
pixelstoTiles= function (pixels, tile.size){
  tiles= data.frame(tx= numeric(nrow(pixels)), ty=numeric(nrow(pixels)));
  tiles$tx= ceiling(pixels[[1]] / as.double(tile.size)) - 1;
  tiles$ty= ceiling(pixels[[2]] / as.double(tile.size)) - 1;
  return(tiles);
}

# Tile to Quadkey  
tilestoQuadkey= function (tiles, zoom) {
  #Convert TMS tile coordinates to Quadtree 
  quadkey= "";
  tiles[[2]]= (2^(zoom) - 1) - tiles[[2]];
  #not sure about my for loop for the quadtree function
  for (i in zoom:0) {
    digit= 0; 
    mask= 1 * (2^(i-1)); #what does 1 << (i-1) mean 
    if ((bitwAnd(tiles[[1]],mask) != 0)) {digit= digit + 1}
    if ((bitwAnd(tiles[[2]],mask) != 0)) {digit= digit +2}
    quadkey= paste (quadkey, digit, sep= '');
  }
  return(quadkey);
}


#returns tms value
googleTiles = function(googleTile, level){
	return(c(googleTile[1],2^level - 1 - googleTile[2]));
}

#quadkey to tile
quadkeytoTiles = function (quadkey){
	tileX = 0;
	tileY = 0;
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
	ll = pixelstoMeters(tileX * tile.size, tileY * tile.size, levelofDetail,origin.shift);
	ur = pixelstoMeters((tileX+1) * tile.size, (tileY+1) * tile.size, levelofDetail,origin.shift);
	return(c(ll[1],ll[2],ur[1],ur[2]))
}


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

  quad.coord$quadshort = substring(quad.coord$quadkey,1,zoomlevel)
  coords = quad.coord[quad.coord$quadshort == quadkey,]

  coords$mx = (coords$mx/A - tile_ll) * scale
  coords$my = (coords$my/A - tile_tt) * -scale

  dir.create(paste(zoomlevel,"/",sep=""),showWarnings=FALSE)
  dir.create(paste(zoomlevel,"/",tms_tile[1],"/",sep=""),showWarnings=FALSE)
  png(file = paste(zoomlevel,"/",tms_tile[1],"/",tms_tile[2],".png",sep=""), width=512, height=512, bg = "transparent")
  #plot to the corners
  par(mar=c(0,0,0,0))
  plot(coords$px,coords$py,pch=20,cex=1,xlim=c(1,512),ylim=c(512,1),xaxs="i",yaxs="i")
  dev.off()
}
