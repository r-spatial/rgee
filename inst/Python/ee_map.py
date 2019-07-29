import ee
import sys

EE_TILES = 'https://earthengine.googleapis.com/map/{mapid}/{{z}}/{{x}}/{{y}}?token={token}'
def ee_map_py(eeobject,vizparams):
  if eeobject.name() is 'Feature':
    try:
      token = ee.FeatureCollection(ee.Feature(eeobject))\
                .draw(**vizparams)\
                .getMapId()
    except:
      print("Error: Check out the ee.Feature availability or visualization parameters.")
  if eeobject.name() is 'FeatureCollection':
    try:
      token = eeobject.draw(**vizparams).getMapId()
    except:
      print("Error: Check out the ee.FeatureCollection availability or visualization parameters.")
  elif eeobject.name() is 'Image':
    try:
      token = eeobject.visualize(**vizparams).getMapId()
    except:
      print("Error: Check out the ee.Image availability or visualization parameters.")
  else:
    sys.exit("ee_map_py only support: ee.Image, ee.Feature, and ee.FeatureCollection")
  return EE_TILES.format(**token)
