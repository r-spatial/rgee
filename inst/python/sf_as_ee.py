#!/usr/bin/python
# -*- coding: utf-8 -*-
import ee


"""Module for passing GEOJSON (created in R) to Earth Engine.

>>> ee_sf_as_ee_py(x)
>>> ee_sfc_as_ee_py(x)
>>> ee_sfg_as_ee_py(x)

These functions are using in R/sf_as_ee.R
"""


def ee_sf_as_ee_py(x):
    # sf to ee
    return ee.FeatureCollection(eval(x)['features'])


def ee_sfc_as_ee_py(x):
    # sfc to ee
    return ee.Geometry(eval(x))


def ee_sfg_as_ee_py(x):
    # sfg to ee
    return ee.Geometry(eval(x))
