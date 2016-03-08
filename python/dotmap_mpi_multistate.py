# -*- coding: utf-8 -*-
"""
Created on Thu Dec 10 16:22:34 2015

@author: Alex Cao, University of Michigan
"""
import pandas as pd
import time
import os
from random import uniform
from osgeo import ogr
from shapely.wkb import loads
from shapely.geometry import Point
import createtile as tile
from globalmaptiles import GlobalMercator
from mpi4py import MPI # line 1
import numpy as np
from itertools import chain
import math

max_blocks = float(350e3)

# Read the FIPS codes from a file
with open('states.txt', 'r') as states_file:
    FIPS = [line.strip() for line in states_file]

# Override the full fips code list for shorter processing
FIPS = [6]
FIPS = [str(x).zfill(2) for x in FIPS]

comm = MPI.COMM_WORLD

merc = GlobalMercator()

for state_fips in FIPS:

    #%% Phase 1: Generate People
    # timing
    start_time = time.time()
    
    # specify zoom level limits
    lowerzoom = 3
    upperzoom = 13
    
    # specify shapefile
    shapefile = os.path.join("Shapefiles","tabblock2010_{}_pophu.shp".format(state_fips))
                   
    # open the shapefile
    ds = ogr.Open(shapefile)
    
    # obtain the first (and only) layer in the shapefile
    lyr = ds.GetLayerByIndex(0)
    lyr.ResetReading()
    if comm.rank == 0:
        print("FIPS {} has {} census blocks".format(state_fips, len(lyr)) )

    census_blocks = len(lyr)
    partitions = int(math.ceil(census_blocks/max_blocks))
    seq = np.around(np.linspace(0,census_blocks,partitions+1)).astype(int)
    tiles = set()
    total_people = 0
    for k in range(partitions):
        t0 = time.time()
        if comm.rank == 0:
            print("Partition {}".format(k))
        data = None
        X = None
        Y = None
        Q = None
        # iterate through every feature (Census Block Polygon) in the layer,
        # obtain the population count, and create a point for each person within
        # that feature and append/extend to a numpy array
        lat = np.array([])
        lon = np.array([])
        quadkey = np.array([])
        for j in range(comm.rank+seq[k], seq[k+1], comm.size):
            feat = lyr[j]
            if j%10000 < comm.size and comm.rank == 0:
                print("{:.1f}% complete".format(float(j)/len(lyr)*100))
            pop = feat.GetFieldAsInteger("POP10")
            if pop == 0:
                continue
            # obtain the OGR polygon object from the feature
            geom = feat.GetGeometryRef()    
            if geom is None:
                continue   
            # convert the OGR Polygon into a Shapely Polygon    
            poly = loads(geom.ExportToWkb())    
            if poly is None:
                continue                
            # obtain the "boundary box" of extreme points of the polygon
            bbox = poly.bounds    
            if not bbox:
                continue    
            # get bounding box for polygon
            leftmost,bottommost,rightmost,topmost = bbox
            # obtain population in each census block
            people_x = np.array([])
            people_y = np.array([])
            people_quadkey = np.array([])
            for i in range(pop):
                # choose a random longitude and latitude within the boundary box
                # and within the polygon of the census block            
                while True:                
                    samplepoint = Point(uniform(leftmost, rightmost),uniform(bottommost, topmost))                       
                    if poly.contains(samplepoint):
                        break
                # convert the longitude and latitude coordinates to meters and
                # a tile reference
                x, y = merc.LatLonToMeters(samplepoint.y, samplepoint.x)
                tx,ty = merc.MetersToTile(x, y, upperzoom)            
                # create a quadkey for each point object
                people_quadkey = np.append(people_quadkey,merc.QuadTree(tx, ty, upperzoom))
                people_x = np.append(people_x,x)
                people_y = np.append(people_y,y)
            lat = np.append(lat,people_y)
            lon = np.append(lon,people_x)
            quadkey = np.append(quadkey,people_quadkey)
            lat = np.round(lat,1)
            lon = np.round(lon,1)
            
        if comm.rank == 0:
            t1 = time.time()
            print("partition {} complete {:.1f}s".format(k,t1-t0))
            
        Y = comm.gather(lat, root=0)
        X = comm.gather(lon, root=0)
        Q = comm.gather(quadkey, root=0)
        lat = None
        lon = None
        quadkey = None
        comm.Barrier()
        if comm.rank == 0:
            t2 = time.time()
            print("gather {:.1f}s".format(t2-t1))    
               
        Y = comm.bcast(Y, root=0)
        X = comm.bcast(X, root=0)
        Q = comm.bcast(Q, root=0)
        comm.Barrier()
        if comm.rank == 0:
            t3 = time.time()
            print("bcast {:.1f}s".format(t3-t2))    
            
        LON = chain.from_iterable(X)
        LAT = chain.from_iterable(Y)
        QUADKEY = chain.from_iterable(Q)
        X = None
        Y = None
        Q = None
        comm.Barrier()    
        if comm.rank == 0:
            t4 = time.time()
            print("flatten {:.1f}s".format(t4-t3))      

        data = pd.DataFrame(zip(LON,LAT,QUADKEY), columns=['x','y','quadkey'])
        Lon = None
        Lat = None
        Quadkey = None       
        comm.Barrier()        
        if comm.rank == 0:
            t5 = time.time()
            total_people += data.shape[0]
            print("dataframe {:.1f}s".format(t5-t4))    
            print("{} people took {:.1f}s".format(total_people,t5-t0))
            print("Rate of {:.1f} persons per sec".format(total_people/(t5-t0)))
        
        #%% Phase 2: Generate Tile
        
        # create a range of descending zoomlevels 
        zoomlevels = range(upperzoom,lowerzoom,-1)
        # track number of tiles
        N = 0
        # loop through zoom levels
        for j in range(len(zoomlevels)):
            level = zoomlevels[j]
            # grab correct quadkey string based on zoom level
            data.loc[:,'quadkey'] = data['quadkey'].map(lambda x: x[0:level])
            # group dataframe by quadkey
            groups =  data.groupby('quadkey')
            # get list of unique quadkeys and length
            quadtree = data['quadkey'].unique()
            tiles = tiles.union(set(quadtree))            
            n = len(quadtree)
            # loop through quadkeys
            for i in range(comm.rank, n, comm.size): # line 3
                quadkey = quadtree[i]
                # generate tile function, 1 means append to tile
                tile.generate_tile(groups.get_group(quadkey), quadkey, level, 1, int(state_fips) )
    
        # line 4
        comm.Barrier()
        
        if comm.rank == 0:
            t6 = time.time()
            print("Creating {} png files took {:.1f}s".format(len(tiles),t6-t5))
            print("Total time {:.1f}".format(t6-start_time))
            print("")
