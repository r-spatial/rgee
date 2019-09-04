#!/usr/bin/python
# -*- coding: utf-8 -*-

import ee
import os

"""Module for return quota usage details for the asset root with the given ID.

>>> humansize(nbytes)
>>> quota(ID)

The functions in this module, which are using for checking earth engine quota,
were obtained from geeup: a Python CLI for Earth Engine Uploads with Selenium
Support <https://github.com/samapriya/geeup>, the acknowledgement for these
functions should be always given to Samapriya Roy.

These functions are using in R/ee_quota.R
"""
def quota(ID):
    """Print your earth engine quota quickly.

    Args:
        ID (str): The ID of the asset to check

    Examples:
        >>> quota('/users/csaybar')
    """
    quota = ee.data.getAssetRootQuota(ID)
    total_msg = str(quota['asset_size']['limit'])
    used_msg = str(quota['asset_size']['usage'])
    #return 'Total Quota: %s \n Used Quota: %s' % (total_msg, used_msg)
    return [total_msg, used_msg]
