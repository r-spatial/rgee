#' Download the last version of geckover
#' author: Samapriya Roy <https://github.com/samapriya/>
#' modified by: Cesar Aybar <https://github.com/csaybar/>
#'
from bs4 import BeautifulSoup
import requests
import csv
import zipfile
import os
import platform
import tarfile
from pathlib import Path
from pySmartDL import SmartDL

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
    except Exception as e:
        print('Issues updating with error '+str(e))

