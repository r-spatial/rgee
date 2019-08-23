#!/usr/bin/python
# -*- coding: utf-8 -*-
import ee
import sys

"""Module for create map access tokens

>>> ee_map_py(eeobject, vizparams)

These functions are using in R/ee_map.R
"""

EE_TILES = 'https://earthengine.googleapis.com/map/{mapid}/{{z}}/{{x}}/{{y}}?token={token}'

def ee_map_py(eeobject, vizparams):
    """Fetch and return a map id and token

    Args:
        eeobject (ee): Earth Engine object of class: Geometry, Feature, FeatureCollection or Image.
        vizparams (dict): The visualization parameters.  See ee.data.getMapId.

    Returns:
          An object containing a mapid and access token, or an error message.

    Examples:
        >>> eeobject = ee.Image()
        >>> vizparams = {}
        >>> ee_map_py(eeobject, vizparams)
    """
    if eeobject.name() is 'Geometry':
        try:
            token = ee.FeatureCollection(ee.Feature(eeobject))\
                      .draw(**vizparams)\
                      .getMapId()
        except:
            print('Error: The Earth Engine Geometry object malformed')
    elif eeobject.name() is 'Feature':
        try:
            token = ee.FeatureCollection(ee.Feature(eeobject))\
                      .draw(**vizparams)\
                      .getMapId()
        except:
            print('Error: The Earth Engine Feature object malformed')
    elif eeobject.name() is 'FeatureCollection':
        try:
            token = eeobject.draw(**vizparams).getMapId()
        except:
            print('Error: The Earth Engine FeatureCollection object malformed')
    elif eeobject.name() is 'Image':
        try:
            token = eeobject.visualize(**vizparams).getMapId()
        except:
            print('Error: The Earth Engine Image object malformed')
    else:
        sys.exit('ee_map only support Geometry, Image, Feature and FeatureCollection')
    return EE_TILES.format(**token)
