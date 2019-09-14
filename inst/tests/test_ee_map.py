import unittest
import ee
import re
from inst.python.ee_map import *
ee.Initialize()

EE_TILES = 'https://earthengine.googleapis.com/map/{mapid}/{{z}}/{{x}}/{{y}}?token={token}'
query = re.compile('token=(.*)')

# test-cases
geom = ee.Geometry.Point([-73.53522, -15.75453])
geom_viz = {"pointRadius":10,"color":"FF0000"}
eeobject_fc = ee.FeatureCollection("users/csaybar/DLdemos/train_set")
image = ee.Image("LANDSAT/LC08/C01/T1/LC08_044034_20140318")
collection = ee.ImageCollection("LANDSAT/LC08/C01/T1_TOA")\
               .filter(ee.Filter().eq("WRS_PATH", 44))\
               .filter(ee.Filter().eq("WRS_ROW", 34))\
               .filterDate("2014-01-01", "2015-01-01")\
               .sort("CLOUD_COVER")

class test_map(unittest.TestCase):
  def test_map_geom(self):
    token = query.findall(ee_map_py(eeobject=geom,vizparams=geom_viz))[0]
    self.assertIsNotNone(token)
  def test_map_fc(self):
    token = query.findall(ee_map_py(eeobject=eeobject_fc,vizparams=geom_viz))[0]
    self.assertIsNotNone(token)
  def test_map_image(self):
    token = query.findall(ee_map_py(eeobject=image,vizparams={}))[0]
    self.assertIsNotNone(token)

if __name__ == '__main__':
    unittest.main(
        failfast=False,
        buffer=False,
        catchbreak=False)
