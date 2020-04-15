#!/usr/bin/python
# -*- coding: utf-8 -*-
import ee
import re

"""Module for passing GEOJSON (created in R) to Earth Engine.

>>> sfg_as_ee_py(x)

These functions are using in R/sf_as_ee.R
"""

def sfg_as_ee_py(x, sfc_class, opt_proj, opt_geodesic, opt_evenOdd):
    x = re.sub(r':null', ':None', x)
    # sfg to ee
    if sfc_class in ["sfc_MULTIPOLYGON","sfc_POLYGON"]:
        return ee.Geometry(geo_json = eval(x),
                           opt_proj = opt_proj,
                           opt_geodesic = opt_geodesic,
                           opt_evenOdd = opt_evenOdd)
    elif sfc_class in ["sfc_LINESTRING","sfc_LINESTRING"]:
        return ee.Geometry(geo_json = eval(x),
                           opt_proj = opt_proj,
                           opt_geodesic = opt_geodesic)
    elif sfc_class in ["sfc_MULTIPOINT","sfc_POINT"]:
        return ee.Geometry(geo_json = eval(x),
                           opt_proj = opt_proj)
    else:
        return False
