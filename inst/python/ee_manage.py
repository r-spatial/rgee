#!/usr/bin/python
# -*- coding: utf-8 -*-

import ee
import os
import datetime
""" Generate a table with all the tasks that is running or has finished.

>>> genreport()
>>> quota(ID)

The functions in this module, which are using for checking earth engine quota,
were obtained from geeup: a Python CLI for Earth Engine Uploads with Selenium
Support <https://github.com/samapriya/geeup>, the acknowledgement for these
functions should be always given to Samapriya Roy.

These functions are using in R/ee_manage.R
"""

def genreport():
    """ Generated report includes taskId, data time, task status and type
    Args:

    Examples:
        >>> genreport()
    """
    taks_list = []
    status=ee.data.getTaskList()
    for items in status:
        ttype=items['task_type']
        tdesc=items['description']
        tstate=items['state']
        tid=items['id']
        tcreate=datetime.datetime.fromtimestamp(items['creation_timestamp_ms']/1000).strftime('%Y-%m-%d %H:%M:%S')
        tstart=datetime.datetime.fromtimestamp(items['start_timestamp_ms']/1000).strftime('%Y-%m-%d %H:%M:%S')
        tupdate=datetime.datetime.fromtimestamp(items['update_timestamp_ms']/1000).strftime('%Y-%m-%d %H:%M:%S')
        tdiffstart=items['start_timestamp_ms']/1000-items['creation_timestamp_ms']/1000
        tdiffend=items['update_timestamp_ms']/1000-items['start_timestamp_ms']/1000
        try:
          error_message = items['error_message']
        except:
          error_message = "NULL"
        dict_summary = {
          'tid':tid,
          'tstate':tstate,
          'tdesc':tdesc,
          'ttype':ttype,
          'tcreate':tcreate,
          'tdiffstart':tdiffstart,
          'tdiffend':tdiffend,
          'error_message':error_message
          }
        taks_list.append(dict_summary)
    return taks_list


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
