import unittest
import ee
import re
from inst.python.ee_quota import *
ee.Initialize()

bytes_list = [2**10,2**15,24242,5124,421,5]
humansize_results = ['1 KB','32 KB', '23.67 KB','5 KB','421 B','5 B']

class test_quota(unittest.TestCase):
    def test_humansize(self):
      """
      ee_quota: Checking outputs.
      """
      humansize_list = list(map(humansize,bytes_list))
      self.assertEqual(humansize_list, humansize_results)
    def test_quota(self):
        message_number = re.findall(r'\d+', quota("users/csaybar"))
        self.assertIsNotNone(message_number)

if __name__ == '__main__':
    unittest.main(
        failfast=False,
        buffer=False,
        catchbreak=False)
