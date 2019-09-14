#! /usr/bin/env python
# -*- coding: utf-8 -*-
import ee

"""Format a table of triplets into a 2D table of rowId x colId.

>>> table_format(table, rowId, colId,val)

table_format is a sightly modification of:
https://code.earthengine.google.com/9c091e27ca86c3d1e6adbefea8769cc3, the
acknowledgement for these functions should be always given to Nick Clinton.
It is use in R/ee_extract.R
"""

def table_format(table, rowId, colId,val):
    """Format a table of triplets into a 2D table of rowId x colId.

    Args:
        triplets: EE Image or ImageCollection
        rowID: Row unique ID.
        colID: Col unique ID.
        fun_name: Function to apply

    Returns:
        A EE table
    """
    # Get the list of all features with a unique row ID.
    rows = table.distinct(rowId)
    # Join the table to the unique IDs to get a collection in which
    # each feature stores a list of all features having a common row ID.
    joined = ee.Join.saveAll('matches').apply(
        primary =  rows,
        secondary = table,
        condition = ee.Filter.equals(
          leftField = rowId,
          rightField = rowId
        ))
    def get_f(row):
        # Get the list of all features with a unique row ID and
        # Map a function over the list of rows to return a list of
        # column ID and value.
        values = ee.List(row.get('matches')).map(lambda feature: [
          ee.Feature(feature).get(colId),
          ee.Feature(feature).get(val)])
        # Return the row with its ID property and properties for
        # all matching columns IDs storing the output of the reducer.
        # The Dictionary constructor is using a list of key, value pairs.
        return row.select([rowId]).set(ee.Dictionary(values.flatten()))
    return joined.map(get_f)
