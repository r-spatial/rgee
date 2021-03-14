from collections import OrderedDict
from urllib import request
import json
import re

def dataset_upgrade():
  earth_engine_list = "https://raw.githubusercontent.com/samapriya/Earth-Engine-Datasets-List/master/gee_catalog.json"
  
  with request.urlopen(earth_engine_list) as url:
      ee_datasets = json.loads(url.read().decode())
  
  FeatureCollection = OrderedDict()
  ImageCollection = OrderedDict()
  Image = OrderedDict()
  
  for ee_dataset in ee_datasets:
    key_name = re.sub("/", "_", ee_dataset["id"])
    if ee_dataset["type"] == "table":
      FeatureCollection[key_name] = ee_dataset["id"]
    elif ee_dataset["type"] == "image":
      Image[key_name] = ee_dataset["id"]
    elif ee_dataset["type"] == "image_collection":
      ImageCollection[key_name] = ee_dataset["id"]
    else:
      raise Exception("We expected a type value: image, image_collection or table.")
  
  eeDataset = OrderedDict(
      FeatureCollection = FeatureCollection, 
      ImageCollection = ImageCollection, 
      Image = Image
  )
  
  with open('inst/dataset.json', 'w') as json_file:
    json.dump(eeDataset, json_file)
  
  return True

if __name__ == "__main__":
  dataset_upgrade()