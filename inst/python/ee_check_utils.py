from bs4 import BeautifulSoup
import requests
import zipfile
import os
import platform
import tarfile
from pySmartDL import SmartDL
from selenium.webdriver import Firefox
from selenium.webdriver.firefox.options import Options
import sys

def ee_check_drivers_py(driverdir,display_in_browser=True):
    options = Options()
    options.add_argument('-headless')
    authorization_url="https://www.google.com/"
    if os.name=="nt":
      path_driver = os.path.join(driverdir,"geckodriver.exe")
      if display_in_browser:
        try:
          driver = Firefox(executable_path=path_driver)
        except:
          return(False)
      else:
        try:
          driver = Firefox(executable_path=path_driver, firefox_options=options)
        except:
          return(False)
    elif os.name=="posix":
      path_driver = os.path.join(driverdir,"geckodriver")
      if display_in_browser:
        try:
          driver = Firefox(executable_path=path_driver)
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


#' Download the last version of geckodriver
#' author: Samapriya Roy <https://github.com/samapriya/>
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

#' Download the last version of geckodriver
#' author: Samapriya Roy <https://github.com/samapriya/>
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
