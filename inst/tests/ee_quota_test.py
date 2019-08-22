#Stolen from https://github.com/samapriya/geeup/blob/84c637ff430c38104e4f3a27e287dafdacd7dd2d/geeup/geeup.py
#Author: Samapriya Roy <https://github.com/samapriya>
from __future__ import print_function
import ee
import os

suffixes = ['B', 'KB', 'MB', 'GB', 'TB', 'PB']
def humansize(nbytes):
    i = 0
    while nbytes >= 1024 and i < len(suffixes)-1:
        nbytes /= 1024.
        i += 1
    f = ('%.2f' % nbytes).rstrip('0').rstrip('.')
    return '%s %s' % (f, suffixes[i])

def quota(ID):
    quota = ee.data.getAssetRootQuota(ID)
    total_msg = str(humansize(quota['asset_size']['limit']))
    used_msg = str(humansize(quota['asset_size']['usage']))
    return "Total Quota: %s \n Used Quota: %s" % (total_msg , used_msg)

