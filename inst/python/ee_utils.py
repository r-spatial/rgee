#!/usr/bin/python
# -*- coding: utf-8 -*-
"""Python utils module for rgee
>>> ee_create_json_py | utils-upload.R | ee_gcs_to_asset_image function
>>> eedate_to_rdate | ee_Date.R | eedate_to_rdate function
"""
import os
import ee
import json


# utils-upload.R --> ee_gcs_to_asset_image function
def ee_create_json_py(towrite, manifest):
    with open(towrite, 'w') as outfile:
        json.dump(manifest, outfile)
    return(True)

# ee_Date.R --> eedate_to_rdate function
def eedate_to_rdate(eedate):
  return float(eedate.getInfo()['value'])
