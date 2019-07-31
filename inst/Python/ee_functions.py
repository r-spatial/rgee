import ee
import webbrowser

def ee_filter_py():
  return ee.Filter

def ee_image_py(ID):
  return ee.Image(ID)

#TODO
def ee_geometry_py(ID):
  return ee.Geometry(ID)

def ee_feature_py(ID):
  return ee.Feature(ID)

def ee_imagecollection_py(ID):
  return ee.ImageCollection(ID)

def ee_featurecollection_py(ID):
  return ee.FeatureCollection(ID)

def ee_reducer_py():
  return ee.Reducer

def ee_export_py():
  return ee.batch.Export
