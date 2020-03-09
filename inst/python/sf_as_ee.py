#!/usr/bin/python
# -*- coding: utf-8 -*-
import ee
import re

"""Module for passing GEOJSON (created in R) to Earth Engine.

>>> sf_as_ee_py(x)
>>> sfc_as_ee_py(x)
>>> sfg_as_ee_py(x)

These functions are using in R/sf_as_ee.R
"""

def sf_as_ee_py(x):
    x = re.sub(r':null', ':None', x)
    return ee.FeatureCollection(eval(x)['features'])

def sfc_as_ee_py(x, opt_proj, opt_geodesic, opt_evenOdd):
    x = re.sub(r':null', ':None', x)
    # sfc to ee (as GeometryCollections)
    return ee.Geometry(geo_json = eval(x),
                       opt_proj = opt_proj,
                       opt_geodesic = opt_geodesic,
                       opt_evenOdd = opt_evenOdd)



def sfg_as_ee_py(x, opt_proj, opt_geodesic, opt_evenOdd):
    x = re.sub(r':null', ':None', x)
    # sfg to ee
    return ee.Geometry(geo_json = eval(x),
                       opt_proj = opt_proj,
                       opt_geodesic = opt_geodesic,
                       opt_evenOdd = opt_evenOdd)
