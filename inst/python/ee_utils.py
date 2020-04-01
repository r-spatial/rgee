#!/usr/bin/python
# -*- coding: utf-8 -*-
"""Python utils module for rgee
>>> ee_create_json_py | utils-upload.R | ee_gcs_to_asset_image function
>>> eedate_to_rdate | ee_Date.R | eedate_to_rdate function
"""
import os
import ee
import json
import base64
import hashlib


# utils-upload.R --> ee_gcs_to_asset_image function
def ee_create_json_py(towrite, manifest):
    with open(towrite, 'w') as outfile:
        json.dump(manifest, outfile)
    return(True)

# ee_Date.R --> eedate_to_rdate function
def eedate_to_rdate(eedate):
  return float(eedate.getInfo()['value'])

# ee_Initialize.R --> ee_create_credentials_earthengine
def _base64param(byte_string):
  """Encodes bytes for use as a URL parameter."""
  return base64.urlsafe_b64encode(byte_string).rstrip(b'=')

# ee_Initialize.R --> ee_create_credentials_earthengine
def create_codes():
  code_verifier = _base64param(os.urandom(32))
  code_challenge = _base64param(hashlib.sha256(code_verifier).digest())
  return code_verifier, code_challenge

# Get current python version
def ee_getversion():
  return ee.__version__
