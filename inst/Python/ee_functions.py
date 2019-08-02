import ee
import webbrowser

def ee_filter_py(opt_filter):
  return ee.Filter(opt_filter=opt_filter)

def ee_image_py(args, version):
  return ee.Image(args, version)

def ee_imagecollection_py(args):
  return ee.ImageCollection(args)

def ee_geometry_py(geo_json, opt_proj, opt_geodesic,opt_evenOdd):
  return ee.Geometry(geo_json, opt_proj, opt_geodesic,opt_evenOdd)

def ee_feature_py(geom, opt_properties):
  return ee.Feature(geom, opt_properties)

def ee_featurecollection_py(args, opt_column):
  return ee.FeatureCollection(args, opt_column)

def ee_reducer_py():
  return ee.Reducer

def ee_export_py():
  return ee.batch.Export
