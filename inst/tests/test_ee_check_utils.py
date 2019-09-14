import unittest
import requests
from inst.python.ee_check_utils import *

def exists(path):
    r = requests.head(path)
    return (r.status_code == 200) | (r.status_code == 302)

class test_ckeck_utils(unittest.TestCase):
    def test_geckodown(self):
      """
      Is googlechrome URI still available?
      """
      download_chromedriver(directory,operating_system, version)
      os_linux="linux"
      os_windows="windows"
      os_mac="macos"
      directory = os.path.join(os.path.expanduser("~/.config/earthengine"),"csaybar/")
      download_chromedriver(directory, os_windows, version='76.0.3809.126')
      download_chromedriver(directory, os_mac, version='76.0.3809.126')
      download_chromedriver(directory, os_linux, version='76.0.3809.126')

if __name__ == '__main__':
    unittest.main(
        failfast=False,
        buffer=False,
        catchbreak=False)
