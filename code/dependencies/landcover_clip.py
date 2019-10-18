#Settings
import arcpy
import os
import csv

# import Dropbox root path
with open('data.txt', 'r') as myfile:
    ddir = myfile.read().replace('\n', '')


arcpy.env.overwriteOutput = True
arcpy.env.workspace = ddir


landcover = os.path.join(ddir, "raw_shape_files\\Land_Cover\\nlcd_2006_impervious_2011_edition_2014_10_10")
landcover_tx = os.path.join(ddir, "int_shape_files\\Land_Cover\\landcover.tif")


texas = os.path.join(ddir, "shape_files\\texas_countysub\\cb_2015_48_cousub_500k.shp")
texas_outline = os.path.join(ddir, "shape_files\\texas_countysub\\texas_outline.shp")

# first dissolve all internal boundaries to create a texas outline
arcpy.Dissolve_management(texas, texas_outline, "STATEFP")

# clip US landcover raster file with texas outline shapefile
arcpy.Clip_management(landcover, "#", landcover_tx, texas_outline, "0", "ClippingGeometry")

arcpy.Delete_management(texas_outline)


"C:\\Users\\YixinSun\\Documents\\Dropbox\\texas"