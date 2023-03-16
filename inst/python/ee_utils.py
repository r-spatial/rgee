#!/usr/bin/python
# -*- coding: utf-8 -*-
"""Python utils module for rgee
>>> ee_create_json_py | utils-upload.R | ee_gcs_to_asset_image function
>>> eedate_to_rdate | ee_Date.R | eedate_to_rdate function
"""
import base64
import hashlib
import json
import os

import ee


# utils-upload.R --> ee_gcs_to_asset_image function
def ee_create_json_py(towrite, manifest):
    with open(towrite, "w") as outfile:
        json.dump(manifest, outfile)
    return True


# ee_Date.R --> eedate_to_rdate function
def eedate_to_rdate(eedate):
    return float(eedate.getInfo()["value"])


# ee_Date.R --> ee_get_date function
def eedate_to_rdate_ic(ic, var="system:time_start"):
    ic_dates = list()
    for img in ic.aggregate_array(var).getInfo():
        if isinstance(img, int):
            ic_dates.append(float(img))
        elif isinstance(img, dict):
            ic_dates.append(float(img["value"]))
        else:
            raise ValueError("img must be a int or a dictionary with a 'value' key.")
    if len(ic_dates) == 0:
        return None
    return ic_dates


# ee_Initialize.R --> ee_create_credentials_earthengine
def _base64param(byte_string):
    """Encodes bytes for use as a URL parameter."""
    return base64.urlsafe_b64encode(byte_string).rstrip(b"=")


# ee_Initialize.R --> ee_create_credentials_earthengine
def create_codes(*nonce_keys):
    """Makes random nonces, and adds PKCE challenges for each _verifier nonce."""
    table = {}
    for key in nonce_keys:
        table[key] = _base64param(os.urandom(32))
        if key.endswith('_verifier'):
            # Generate a challenge that the server will use to ensure that requests
            # only work with our verifiers.  https://tools.ietf.org/html/rfc7636
            pkce_challenge = _base64param(hashlib.sha256(table[key]).digest())
            table[key.replace('_verifier', '_challenge')] = pkce_challenge
    return {k: v.decode() for k, v in table.items()}


# Get current Earth Engine version
def ee_getversion():
    return ee.__version__


def ee_path():
    cred_path = os.path.expanduser("~/.config/earthengine/")
    return cred_path
