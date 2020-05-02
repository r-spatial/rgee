import unittest
import ee
from inst.python.sf_as_ee import *
ee.Initialize()

data_polygon = {
  "type": "FeatureCollection",
  "features": [{"type": "Feature",
                "properties": {},
                "geometry": {"type": "MultiPolygon",
                             "coordinates": [[[[-5, 40],
                                               [65, 40],
                                               [65, 60],
                                               [-5, 60],
                                               [-5, 60],
                                               [-5, 40]]]]
}}]}
data_polygon = str(data_polygon)
expected_centroid = [30.000000000000004, 54.56306390486972]

class test_sf_as_ee(unittest.TestCase):
    def test_sf_as_ee_py(self):
      """
      sf test: (Useful to testing future changes for earthengine-api)
      """
      ee_data_polygon = ee_sf_as_ee_py(data_polygon)
      centroid_point = ee_data_polygon.geometry().centroid().getInfo()['coordinates']
      self.assertEqual(centroid_point[0], expected_centroid[0])
      self.assertEqual(centroid_point[1], expected_centroid[1])
    def test_sfc_as_ee_py(self):
      """
      sfc test: (Useful to testing future changes for earthengine-api)
      """
      sfc_export = str(eval(data_polygon)['features'][0]['geometry'])
      ee_data_polygon = ee_sfc_as_ee_py(sfc_export)
      centroid_point = ee_data_polygon.centroid().getInfo()['coordinates']
      self.assertEqual(centroid_point[0], expected_centroid[0])
      self.assertEqual(centroid_point[1], expected_centroid[1])
    def test_sfg_as_ee_py(self):
      """
      sfg test: (Useful to testing future changes for earthengine-api)
      """
      sfc_export = str(eval(data_polygon)['features'][0]['geometry'])
      ee_data_polygon = ee_sfg_as_ee_py(sfc_export)
      centroid_point = ee_data_polygon.centroid().getInfo()['coordinates']
      self.assertEqual(centroid_point[0], expected_centroid[0])
      self.assertEqual(centroid_point[1], expected_centroid[1])

if __name__ == '__main__':
    unittest.main(
        failfast=False,
        buffer=False,
        catchbreak=False)
