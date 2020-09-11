#!/usr/bin/python
# -*- coding: utf-8 -*-
import ee
import re

"""Module for passing GEOJSON (created in R) to Earth Engine.

>>> sfg_as_ee_py(x)

These functions are used in R/sf_as_ee.R
"""
def sfg_as_ee_py(x, sfc_class, opt_geodesic, opt_evenOdd):
    x = re.sub(r':null', ':None', x)
    sfc_class = re.sub('sfc_','',sfc_class)
    # sfg to ee
    if sfc_class in ["MULTIPOLYGON","POLYGON"]:
        return ee.Geometry(geo_json = eval(x),
                           opt_proj = "EPSG:4326",
                           opt_geodesic = opt_geodesic,
                           opt_evenOdd = opt_evenOdd)
    elif sfc_class in ["MULTILINESTRING","LINESTRING"]:
        return ee.Geometry(geo_json = eval(x),
                           opt_proj = "EPSG:4326",
                           opt_geodesic = opt_geodesic)
    elif sfc_class in ["MULTIPOINT","POINT"]:
        return ee.Geometry(geo_json = eval(x),
                           opt_proj = "EPSG:4326")
    else:
        return False
