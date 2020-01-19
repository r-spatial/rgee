#!/usr/bin/python
# -*- coding: utf-8 -*-
"""Module for return quota usage details for the asset root with the given ID.

>>> ee_get_google_auth_session_py(username, password, dirname)
>>> ee_create_json_py(towrite, manifest)
>>> retry_if_ee_error(exception)
>>> ee_get_upload_url_py(session)
>>> ee_file_to_gcs_py(session,file_path,ftype,upload_url):
>>> ee_create_json_py(towrite, manifest)

The functions in this module, which are using for upload files to
gs://earthengine-uploads/, were obtained from geeup: a Python CLI
for Earth Engine Uploads with Selenium Support
<https://github.com/samapriya/geeup>, they have only experience slight
modifications, therefore, the acknowledgement for these
functions should be always given to Samapriya Roy.

These functions are using in R/utils-upload.R
"""

import os
import ee
import ast
import sys
import json
import time
import pickle
import requests
import selenium
import subprocess

from selenium import webdriver
from selenium.webdriver import Chrome
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from requests_toolbelt import MultipartEncoder
#ee_get_google_auth_session_py(username, password, dirname)
# username = r['username']
# password = r['password']
# dirname = r['dirname']
# ee_get_google_auth_session_py(username, password, dirname)
def ee_get_google_auth_session_py(username, password, dirname):
    """ Get cookies from https://code.earthengine.google.com using Selenium

    Args:
        username: gmail username.
        password: gmail password.
        dirname: geckodriver path

    Returns:
        A session object that contains the necessary cookies for getting
        pass to gs://earthengine-uploads/

    Examples:
        >>> import os
        >>> username = 'csaybar@gmail.com'
        >>> password = os.environ['GMAIL_PASSWORD']
        >>> dirname = '/home/aybarpc01/.config/earthengine/'
        >>> session = ee_get_google_auth_session_py(username, password, dirname)
    """
    options = Options()
    authorization_url = 'https://code.earthengine.google.com/'
    uname = username
    passw = password
    if os.name == 'nt':
        path_driver = os.path.join(dirname, 'geckodriver.exe')
        driver = Chrome(executable_path=path_driver,
                        chrome_options=options)
    elif os.name == 'posix':
        path_driver = os.path.join(dirname, 'chromedriver')
        driver = Chrome(executable_path=path_driver,
                        chrome_options=options)
    driver.get(authorization_url)
    username = driver.find_element_by_xpath('//*[@id="identifierId"]')
    username.send_keys(uname)
    driver.find_element_by_id('identifierNext').click()
    password = WebDriverWait(driver, 5).until(EC.element_to_be_clickable((By.XPATH, "//input[@name='password']")))
    password.send_keys(passw)
    task_pass = WebDriverWait(driver, 5).until(EC.element_to_be_clickable((By.XPATH, '//*[@id="passwordNext"]')))
    time.sleep(1)
    task_pass.click()
    try:
        element_g = '//*[@id="view_container"]/div/div/div[2]/div/div[1]/div/form/span/section/div/div/div/div/ul/li[1]/div/div[1]/div/div[2]/div[1]'
        re_confirm =  WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.XPATH,element_g)))
        re_confirm.click()
        ee_button = '//*[@id="main"]/div[1]/div[1]/div/div[1]/div/div[1]/div/div[2]/div[1]/div[1]/div/div[1]/button'
        WebDriverWait(driver,20).until(EC.presence_of_element_located((By.XPATH, ee_button)))
    except Exception as e:
        pass
    cookies = driver.get_cookies()
    session = requests.Session()
    for cookie in cookies:
        session.cookies.set(cookie['name'], cookie['value'])
    driver.close()
    return session


def retry_if_ee_error(exception):
    return isinstance(exception, ee.EEException)


def ee_get_upload_url_py(session):
    """ Create a provisional bucket (available in gs://earthengine-uploads/) and get URL

    Args:
        session: A session object that contains cookies from
                 https://code.earthengine.google.com

    Returns:
         URL for ingest the data via POST request

    Examples:
        >>> import os
        >>> username = 'csaybar@gmail.com'
        >>> password = os.environ['GMAIL_PASSWORD']
        >>> dirname = '/home/aybarpc01/.config/earthengine/'
        >>> session = ee_get_google_auth_session_py(username, password, dirname)
        >>> upload_url = ee_get_upload_url_py(session)
    """
    rr = session.get('https://code.earthengine.google.com/assets/upload/geturl')
    try:
        d = ast.literal_eval(rr.text)
        return d['url']

    except Exception as e:
        print(e)


def ee_file_to_gcs_py(session, file_path, ftype, upload_url):
    """ Upload a file to gs://earthengine-uploads/ via POST request
    Args:
        session (request): A session object that contains cookies from
                           https://code.earthengine.google.com.
        file_path (str): fullname of the file.
        ftype (str): Type of the file. Only shapefile and geotiff are suported.
        upload_url:  URL for the new :class:`Request` object.

    Returns:
        A session object that contains the necessary cookies for getting
        pass to gs://earthengine-uploads/

    Examples:
        >>> import os
        >>> username = 'csaybar@gmail.com'
        >>> password = os.environ['GMAIL_PASSWORD']
        >>> dirname = '/home/aybarpc01/.config/earthengine/'
        >>> session = ee_get_google_auth_session_py(username, password, dirname)
        >>> upload_url = ee_get_upload_url_py(session)
        >>> filepath = "/inst/data/arequipa.shp"
        >>> ee_file_to_gcs_py(session, filepath, 'shp', upload_url)
    """
    with open(file_path, 'rb') as f:
        file_name = os.path.basename(file_path)
        if ftype == 'tif':
            m = MultipartEncoder(fields={'image_file': (file_name, f)})
        elif ftype == 'shapefile':
            m = MultipartEncoder(fields={'table_file': (file_name, f)})
        else:
            pass
        try:
            headers = {
              'sec-fetch-mode': 'cors',
              'origin': 'https://code.earthengine.google.com',
              'accept-encoding': 'gzip, deflate, br',
              'accept-language': 'en-US,en;q=0.9',
              'content-type': m.content_type,
              'accept': '*/*',
              'referer': 'https://code.earthengine.google.com/',
              'authority': 'code.earthengine.google.com',
              'sec-fetch-site': 'same-origin'
              }
            resp = session.post(upload_url, data=m,headers = headers)
            gsid = resp.json()[0]
            return gsid
        except Exception as e:
            return resp

def ee_create_json_py(towrite, manifest):
    with open(towrite, 'w') as outfile:
        json.dump(manifest, outfile)
    return(True)

def save_py_object(eeobject, filename):
    with open(filename, 'wb') as pickle_file:
      pickle.dump(eeobject, pickle_file)
    return(True)

def load_py_object(filename):
    with open(filename, 'rb') as pickle_file:
        py_pickle_file = pickle.load(pickle_file)
    return py_pickle_file

#It is necessary due 2^31-1 (.Machine$integer.max)
def r_to_eeDate(Rdate):
  return float(ee.Date(Rdate).getInfo()['value'])

def eeDate_to_r(eedate):
  return float(eedate.getInfo()['value'])
