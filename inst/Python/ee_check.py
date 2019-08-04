# Is the Earth-Engine Python API installed in your SO?
import imp

def ee_check_py():
  try:
      imp.find_module('ee')
      found = True
  except ImportError:
      found = False
  return found

