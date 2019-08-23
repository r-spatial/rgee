import unittest
import requests
from inst.python.ee_check_utils import *

def exists(path):
    r = requests.head(path)
    return (r.status_code == 200) | (r.status_code == 302)

class test_ckeck_utils(unittest.TestCase):
    def test_geckodown(self):
      """
      Is geckodrive URI still available?
      """
      comb_linux="linux64.tar.gz"
      comb_windows="win64.zip"
      comb_mac="macos.tar.gz"
      source = requests.get("https://github.com/mozilla/geckodriver/releases/latest").text
      soup = BeautifulSoup(source.encode("utf-8"),'lxml')
      vr = str(soup.title.text.encode("utf-8")).split(' ')[1]
      http_gecko = "https://github.com/mozilla/geckodriver/releases/download/"
      container_linux = http_gecko+vr+"/geckodriver-"+vr+'-'+comb_linux
      container_windows = http_gecko+vr+"/geckodriver-"+vr+'-'+comb_windows
      container_macos = http_gecko+vr+"/geckodriver-"+vr+'-'+comb_mac
      self.assertTrue(exists(container_linux))
      self.assertTrue(exists(container_windows))
      self.assertTrue(exists(container_macos))

if __name__ == '__main__':
    unittest.main(
        failfast=False,
        buffer=False,
        catchbreak=False)
