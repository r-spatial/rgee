from bs4 import BeautifulSoup
import requests
import zipfile
import os
import platform
import tarfile
from pySmartDL import SmartDL
from selenium.webdriver import Firefox
from selenium.webdriver.firefox.options import Options


def ee_check_drivers(driverdir,headless=True):
    options = Options()
    options.add_argument('-headless')
    authorization_url="https://www.google.com/"
    if os.name=="nt":
      path_driver = os.path.join(driverdir,"geckodriver.exe")
      if headless:
        try:
          driver = Firefox(executable_path=path_driver, firefox_options=options)
        except:
          return(False)
      else:
        try:
          driver = Firefox(executable_path=path_driver, firefox_options=options)
        except:
          return(False)
    elif os.name=="posix":
      path_driver = os.path.join(driverdir,"geckodriver")
      if headless:
        try:
          driver = Firefox(executable_path=path_driver, firefox_options=options)
        except:
          return(False)
      else:
        try:
          driver = Firefox(executable_path=path_driver, firefox_options=options)
        except:
          return(False)
    driver.get(authorization_url)
    driver.quit()
    return(True)


#' Download the last version of geckover
#' author: Samapriya Roy <https://github.com/samapriya/>
#' modified by: Cesar Aybar <https://github.com/csaybar/>
def geckodown_linux(directory,vr):
    comb="linux"+str(platform.machine()[-2:])+".tar.gz"
    source = requests.get("https://github.com/mozilla/geckodriver/releases/latest").text
    soup = BeautifulSoup(source.encode("utf-8"),'lxml')
    if vr is None:
        vr = str(soup.title.text.encode("utf-8")).split(' ')[1]
    else:
        vr = 'v'+vr
    container = "https://github.com/mozilla/geckodriver/releases/download/"+vr+"/geckodriver-"+vr+'-'+comb
    print("Downloading from: "+str(container))
    try:
        url = container
        dest = directory
        obj = SmartDL(url, dest)
        obj.start()
        path=obj.get_dest()
        print(os.path.join(directory,'geckodriver-'+vr+'-linux64.zip'))
        filepath=os.path.join(directory,'geckodriver-'+vr+'-'+comb)
        if (filepath.endswith("tar.gz")):
            tar = tarfile.open(filepath,'r:*')
            tar.extractall(directory)
            tar.close()
            #print "Extracted in Current Directory"
            print("Use selenium driver path as "+os.path.join(directory,"geckodriver"))
        os.remove(filepath)
    except Exception as e:
        print('Issues updating with error '+str(e))

def geckodown_win(directory,vr):
    comb="win"+str(platform.machine()[-2:])+".zip"
    source=requests.get("https://github.com/mozilla/geckodriver/releases/latest").text
    soup=BeautifulSoup(source.encode("utf-8"),'lxml')
    if vr is None:
        vr = str(soup.title.text.encode("utf-8")).split(' ')[1]
    else:
        vr = 'v'+vr
    container="https://github.com/mozilla/geckodriver/releases/download/"+vr+"/geckodriver-"+vr+'-'+comb
    print("Downloading from: "+str(container))
    try:
        url = container
        dest = directory
        obj = SmartDL(url, dest)
        obj.start()
        path=obj.get_dest()
        print(os.path.join(directory,'geckodriver-'+vr+'-win64.zip'))
        archive=zipfile.ZipFile(os.path.join(directory,'geckodriver-'+vr+'-'+comb))
        for files in archive.namelist():
            archive.extractall(directory)
        print("Use selenium driver path as "+str(directory))
        os.remove(archive)
    except Exception as e:
        print('Issues updating with error '+str(e))




#Python Standard Libraries
def ee_check_py_webbrowser():
  try:
    import webbrowser
    try:
      return webbrowser.__version__
    except:
      return True
  except:
    return False

def ee_check_py_request():
  try:
    import request
    try:
      return request.__version__
    except:
      return True
  except:
    return False

def ee_check_py_zipfile():
  try:
    import zipfile
    try:
      return zipfile.__version__
    except:
      return True
  except:
    return False

def ee_check_py_tarfile():
  try:
    import tarfile
    try:
      return tarfile.__version__
    except:
      return True
  except:
    return False

def ee_check_py_subprocess():
  try:
    import subprocess
    try:
      return subprocess.__version__
    except:
      return True
  except:
    return False

def ee_check_py_retrying():
  try:
    import retrying
    try:
      return retrying.__version__
    except:
      return True
  except:
    return False

def ee_check_py_ast():
  try:
    import ast
    try:
      return ast.__version__
    except:
      return True
  except:
    return False


def ee_check_py_sys():
  try:
    import sys
    try:
      return sys.__version__
    except:
      return True
  except:
    return False

def ee_check_py_os():
  try:
    import os
    try:
      return os.__version__
    except:
      return True
  except:
    return False


def ee_check_py_time():
  try:
    import time
    try:
      return time.__version__
    except:
      return True
  except:
    return False


#Third-party package
def ee_check_py_ee():
  try:
    import ee
    return ee.__version__
  except:
    return False

def ee_check_py_selenium():
  try:
    import selenium
    return selenium.__version__
  except:
    return False

def ee_check_py_bs4():
  try:
    import bs4
    return bs4.__version__
  except:
    return False

def ee_check_py_platform():
  try:
    import platform
    return platform.__version__
  except:
    return False

def ee_check_py_json():
  try:
    import json
    return json.__version__
  except:
    return False

def ee_check_py_pysmartDL():
  try:
    import pySmartDL
    return pySmartDL.__version__
  except:
    return False

def ee_check_py_requests_toolbelt():
  try:
    import requests_toolbelt
    return requests_toolbelt.__version__
  except:
    return False
