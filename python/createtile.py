# -*- coding: utf-8 -*-
"""
Created on Fri Feb 05 10:44:53 2016

@author: caoa
"""

from PIL import Image, ImageDraw
import os
import globalmaptiles as gmt

# Choose colour from blue2green palette
def choose_color(fips):
    red = [0]*10
    green = [0,28,56,85,113,141,170,198,226,255]
    blue = [255,226,198,170,141,113,85,56,28,0]
    n = fips%10
    return (red[n],green[n],blue[n])    

# determine alpha level of point(s)
def transparent(level):
    if level == 4 or level == 5:
        return 153
    elif level == 6 or level == 7:
        return 179
    elif level == 8 or level == 9:
        return 204
    elif level == 10 or level == 11:
        return 230
    elif level == 12 or level == 13:
        return 255
    else:
        return 0.0

# create png file given quadkey
def generate_tile(df, quadkey, level, append=False, fips=50):

    proj = gmt.GlobalMercator()
    google_tile = proj.QuadKeyToGoogleTile(quadkey)
    tms_tile = proj.GoogleToTMSTile(google_tile[0],google_tile[1],level)
    bounds = proj.TileBounds(tms_tile[0],tms_tile[1],level)

    tile_size = 512
    width = int(tile_size*3)
    bkgrd = 0
    filename = os.path.join('tiles',str(level),str(tms_tile[0]),"{}.png".format(tms_tile[1]))
    if append:
        try:
            img = Image.open(filename)
            img = img.resize((width,width),resample=Image.LANCZOS)        
        except IOError:
            img = Image.new('RGBA',(width,width),(bkgrd,bkgrd,bkgrd,0))
    else:
        img = Image.new('RGBA',(width,width),(bkgrd,bkgrd,bkgrd,0))        
    draw = ImageDraw.Draw(img)

    A = 1000
    tile_ll = bounds[0]/A
    tile_bb = bounds[1]/A
    tile_rr = bounds[2]/A
    tile_tt = bounds[3]/A
    
    xscale = width/(tile_rr - tile_ll)
    yscale = width/(tile_tt - tile_bb)
    scale = min(xscale, yscale)
     
    px = (scale*(df['x']/A - tile_ll)).astype(int)
    py = (-scale*(df['y']/A - tile_tt)).astype(int)
    rgb = choose_color(fips)
    draw.point(zip(px,py), fill=(rgb[0],rgb[1],rgb[2],transparent(level)))
    
    img = img.resize((tile_size,tile_size),resample=Image.BICUBIC)
    try:
        img.save(filename,'PNG')
    except:
        try:
            os.makedirs(os.path.join('tiles',str(level),str(tms_tile[0])) )
            img.save(filename,'PNG')
        except OSError:
            img.save(filename,'PNG')        
