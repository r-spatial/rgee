import ee
import sys
import os
import time
import selenium
from selenium import webdriver
from selenium.webdriver import Firefox
from selenium.webdriver.firefox.options import Options
from selenium.webdriver.common.keys import Keys
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By

def ee_get_google_auth_session(username, password,dirname):
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
    password = WebDriverWait(driver, 10).until(
      EC.element_to_be_clickable((By.XPATH, "//input[@name='password']"))
    )
    password.send_keys(passw)
    time.sleep(2)
    password.find_element_by_xpath('//*[@id="passwordNext"]').click()
    try:
        driver.find_element_by_xpath("//div[@id='view_container']/form/div[2]/div/div/div/ul/li/div/div[2]/p").click()
        driver.find_element_by_xpath("//div[@id='submit_approve_access']/content/span").click()
    except Exception as e:
        pass
    cookies = driver.get_cookies()
    session = requests.Session()
    for cookie in cookies:
        session.cookies.set(cookie['name'], cookie['value'])
    #driver.close()
    return session

def ee_check_selenium_firefox(driverdir):
    options = Options()
    options.add_argument('-headless')
    authorization_url="https://www.google.com/"
    if os.name=="nt":
      path_driver = os.path.join(driverdir,"geckodriver.exe")
      driver = Firefox(executable_path=path_driver)
    elif os.name=="posix":
      path_driver = os.path.join(driverdir,"geckodriver")
      driver = Firefox(executable_path=path_driver)
    driver.get(authorization_url)
    driver.quit()
    return("Selenium-Firefox (geckodriver) was installed correctly")
