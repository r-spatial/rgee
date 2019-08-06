#eval(r['geojson_list'])
#x=r['geojson_list']
def ee_as_ee_py(x):
  return ee.FeatureCollection(eval(x)['features'])
