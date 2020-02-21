#!/usr/bin/python
# -*- coding: utf-8 -*-
import ee
import sys

"""Function to get the EE map token

>>> ee_map_py(eeobject, vizparams)

This function is used in R/ee_map.R
"""

#EE_TILES = 'https://earthengine.googleapis.com/map/{mapid}/{{z}}/{{x}}/{{y}}?token={token}'
# eeobject = r['eeobject']
# vizparams = r['vizparams']

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
            EE_TILES = ee.FeatureCollection(ee.Feature(eeobject))\
                      .draw(**vizparams)\
                      .getMapId()["tile_fetcher"].url_format
        except:
            print('Error: The Earth Engine Geometry object malformed')
    elif eeobject.name() is 'Feature':
        try:
            EE_TILES = ee.FeatureCollection(ee.Feature(eeobject))\
                      .draw(**vizparams)\
                      .getMapId()["tile_fetcher"].url_format
        except:
            print('Error: The Earth Engine Feature object malformed')
    elif eeobject.name() is 'FeatureCollection':
        try:
            EE_TILES = eeobject.draw(**vizparams).getMapId()["tile_fetcher"].url_format
        except:
            print('Error: The Earth Engine FeatureCollection object malformed')
    elif eeobject.name() is 'Image':
        try:
            EE_TILES = eeobject.visualize(**vizparams).getMapId()["tile_fetcher"].url_format
        except:
            print('Error: The Earth Engine Image object malformed')
    else:
        sys.exit('ee_map only support Geometry, Image, Feature and FeatureCollection')
    return EE_TILES
