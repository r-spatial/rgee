import unittest
import urllib
import os
import ee
from inst.python.ee_selenium_functions import *
ee.Initialize()

gmail_password = os.environ["GMAIL_PASSWORD"]
dirname = os.path.expanduser('~/.config/earthengine/')
username = 'csaybar@gmail.com'

# download a Image
image_url = 'https://github.com/r-spatial/sf/raw/master/inst/tif/geomatrix.tif'
geomatrix = os.path.expanduser('~/geomatrix.tif')
urllib.request.urlretrieve(image_url, geomatrix)

class test_selenium(unittest.TestCase):
  def test_upload_to_gcs(self):
    session = ee_get_google_auth_session_py(username, gmail_password,dirname)
    upload_url = ee_get_upload_url_py(session)
    ssurl = ee_file_to_gcs_py(session = session,file_path = geomatrix,ftype = "tif",upload_url = upload_url)
    self.assertEqual(os.path.basename(ssurl), os.path.basename(geomatrix))

if __name__ == '__main__':
    unittest.main(
        failfast=False,
        buffer=False,
        catchbreak=False)
