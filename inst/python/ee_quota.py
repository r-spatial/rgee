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

suffixes = ['B', 'KB', 'MB', 'GB', 'TB', 'PB']


def humansize(nbytes):
    """Change the unit of measurement of bytes

    Args:
        nbytes (int): Number of bytes.

    Returns:
        Number of bytes in a more human-comprehensible way

    Examples:
        >>> humansize(10^4)
    """
    i = 0
    while nbytes >= 1024 and i < len(suffixes) - 1:
        nbytes /= 1024.
        i += 1
    f = ('%.2f' % nbytes).rstrip('0').rstrip('.')
    return '%s %s' % (f, suffixes[i])


def quota(ID):
    """Print your earth engine quota quickly.

    Args:
        ID (str): The ID of the asset to check

    Returns:
        Bool : If it is True means that no existing execution errors

    Examples:
        >>> quota('/users/csaybar')
    """
    quota = ee.data.getAssetRootQuota(ID)
    total_msg = str(humansize(quota['asset_size']['limit']))
    used_msg = str(humansize(quota['asset_size']['usage']))
    return 'Total Quota: %s \n Used Quota: %s' % (total_msg, used_msg)
