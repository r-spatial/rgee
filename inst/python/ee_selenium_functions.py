import subprocess
import retrying
import json
import ast
import ee
import sys
import os
import time
import selenium
import requests
from requests_toolbelt import MultipartEncoder

from selenium import webdriver
from selenium.webdriver import Firefox
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By

def ee_get_google_auth_session_py(username, password,dirname):
    options = Options()
    options.add_argument('-headless')
    authorization_url="https://code.earthengine.google.com"
    uname = username
    passw= password
    if os.name=="nt":
      path_driver = os.path.join(dirname,"geckodriver.exe")
      driver = Firefox(executable_path=path_driver, firefox_options=options)
    elif os.name=="posix":
      path_driver = os.path.join(dirname,"geckodriver")
      driver = Firefox(executable_path=path_driver, firefox_options=options)
    driver.get(authorization_url)
    username = driver.find_element_by_xpath('//*[@id="identifierId"]')
    username.send_keys(uname)
    driver.find_element_by_id("identifierNext").click()
    password = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.XPATH, "//input[@name='password']")))
    password.send_keys(passw)
    task_pass = WebDriverWait(driver, 15).until(EC.element_to_be_clickable((By.XPATH, '//*[@id="passwordNext"]')))
    time.sleep(1)
    task_pass.click()
    try:
      WebDriverWait(driver,3).until(EC.presence_of_element_located((By.XPATH,'//*[@id=":1c"]')))
    except:
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
    rr=session.get("https://code.earthengine.google.com/assets/upload/geturl")
    try:
        d = ast.literal_eval(rr.text)
        return d['url']
    except Exception as e:
        print(e)

@retrying.retry(retry_on_exception=retry_if_ee_error,
                wait_exponential_multiplier=1000,
                wait_exponential_max=4000,
                stop_max_attempt_number=3)
def ee_file_to_gcs_py(session, file_path,ftype,upload_url):
    with open(file_path, 'rb') as f:
        file_name=os.path.basename(file_path)
        if ftype == 'tif':
          m=MultipartEncoder(fields={'image_file':(file_name, f)})
        elif ftype == 'shapefile':
          m=MultipartEncoder(fields={'table_file':(file_name, f)})
        else:
          pass
        try:
            resp = session.post(upload_url, data=m, headers={'Content-Type': m.content_type})
            gsid = resp.json()[0]
            return gsid
        except Exception as e:
            print(e)


def ee_create_json_py(towrite,manifest):
  with open(towrite, 'w') as outfile:
          json.dump(manifest, outfile)
