#! /usr/bin/env python
# -*- coding: utf-8 -*-

"""Check Selenium drivers and download geckodriver for linux, mac and windows.

>>> ee_check_drivers_py(driverdir,display_in_browser=True)
>>> geckodown_linux(directory,vr)
>>> geckodown_win(directory,vr)
>>> geckodown_mac(directory,vr)

The functions for download geckodriver, which are present in this module,
are a slight modification of geeup, a Python CLI for Earth Engine Uploads with
Selenium Support <https://github.com/samapriya/geeup>, the acknowledgement for
these functions should be always given to Samapriya Roy.
"""
from __future__ import print_function

import os
import sys
import zipfile
import tarfile
import platform
import requests

from bs4 import BeautifulSoup
from pySmartDL import SmartDL
from selenium.webdriver import Firefox
from selenium.webdriver.firefox.options import Options


def ee_check_drivers_py(driverdir, display_in_browser=True):
    """Check if selenium works in their system

    Args:
        driverdir (str): geckodriver path
        display_in_broswer (bool): If it is True, firefox browser will display it.

    Returns:
        Bool : If it is True means that no existing execution errors

    Examples:
        >>> driverdir = '/home/aybarpc01/.config/earthengine/'
        >>> ee_check_drivers_py(driverdir, display_in_browser=True)
    """
    options = Options()
    options.add_argument('-headless')
    authorization_url = 'https://www.google.com/'
    if os.name == 'nt':
        path_driver = os.path.join(driverdir, 'geckodriver.exe')
        if display_in_browser:
            try:
                driver = Firefox(executable_path=path_driver)
            except:
                return False
        else:
            try:
                driver = Firefox(executable_path=path_driver,
                                 firefox_options=options)
            except:
                return False
    elif os.name == 'posix':
        path_driver = os.path.join(driverdir, 'geckodriver')
        if display_in_browser:
            try:
                driver = Firefox(executable_path=path_driver)
            except:
                return False
        else:
            try:
                driver = Firefox(executable_path=path_driver,
                                 firefox_options=options)
            except:
                return False
    driver.get(authorization_url)
    driver.quit()
    return True


def geckodown_linux(directory, vr=None):
    """Download geckodriver driver for linux

    Args:
        directory (str): name where the downloaded file is saved.
        vr (float): geckodriver version

    Returns:
        None

    Examples:
        >>> driverdir = '/home/aybarpc01/.config/earthengine/'
        >>> geckodown_linux(driverdir, vr= 0.24.0)
    """
    comb = 'linux' + str(platform.machine()[-2:]) + '.tar.gz'
    firefox_url = 'https://github.com/mozilla/geckodriver/releases/latest'
    source = requests.get(firefox_url).text
    soup = BeautifulSoup(source.encode('utf-8'), 'lxml')
    if vr is None:
        vr = str(soup.title.text.encode('utf-8')).split(' ')[1]
    else:
        vr = 'v' + vr
    download_url = 'https://github.com/mozilla/geckodriver/releases/download/'
    container = "%s/%s/geckodriver-%s-%s" % (download_url, vr, vr, comb)
    print('Downloading from: ' + str(container))
    try:
        url = container
        dest = directory
        obj = SmartDL(url, dest)
        obj.start()
        path = obj.get_dest()
        message = os.path.join(directory, 'geckodriver-' + vr + '-linux64.zip')
        print(message)
        filepath = os.path.join(directory, 'geckodriver-' + vr + '-' + comb)
        if filepath.endswith('tar.gz'):
            tar = tarfile.open(filepath, 'r:*')
            tar.extractall(directory)
            tar.close()
        message_2 = 'Use selenium driver path as'
        print(message_2 +  os.path.join(directory, 'geckodriver'))
        os.remove(filepath)
    except Exception as e:
        print('Issues updating with error ' + str(e))


def geckodown_win(directory, vr=None):
    """Download geckodriver driver for Windows

    Args:
        directory (str): name where the downloaded file is saved.
        vr (float): geckodriver version

    Returns:
        None

    Examples:
        >>> driverdir = 'C://Users/aybarpc01/Desktop/earthengine/'
        >>> geckodown_win(driverdir, vr= 0.24.0)
    """
    comb = 'win' + str(platform.machine()[-2:]) + '.zip'
    firefox_url = 'https://github.com/mozilla/geckodriver/releases/latest'
    source = requests.get(firefox_url).text
    soup = BeautifulSoup(source.encode('utf-8'), 'lxml')
    if vr is None:
        vr = str(soup.title.text.encode('utf-8')).split(' ')[1]
    else:
        vr = 'v' + vr
    download_url = 'https://github.com/mozilla/geckodriver/releases/download/'
    container = "%s/%s/geckodriver-%s-%s" % (download_url, vr, vr, comb)
    print('Downloading from: ' + str(container))
    try:
        url = container
        dest = directory
        obj = SmartDL(url, dest)
        obj.start()
        path = obj.get_dest()
        message = os.path.join(directory, 'geckodriver-' + vr + '-win64.zip')
        print(message)
        archive = zipfile.ZipFile(os.path.join(directory, 'geckodriver-'
                                   + vr + '-' + comb))
        for files in archive.namelist():
            archive.extractall(directory)
        print('Use selenium driver path as ' + str(directory))
        os.remove(archive)
    except Exception as e:
        print('Issues updating with error ' + str(e))


def geckodown_mac(directory, vr=None):
    """Download geckodriver driver for MacOS

    Args:
        directory (str): name where the downloaded file is saved.
        vr (float): geckodriver version

    Returns:
        None

    Examples:
        >>> driverdir = '/home/aybarpc01/.config/earthengine/'
        >>> geckodown_mac(driverdir, vr= 0.24.0)
    """
    comb = "macos.tar.gz"
    firefox_url = 'https://github.com/mozilla/geckodriver/releases/latest'
    source = requests.get(firefox_url).text
    soup = BeautifulSoup(source.encode('utf-8'), 'lxml')
    if vr is None:
        vr = str(soup.title.text.encode('utf-8')).split(' ')[1]
    else:
        vr = 'v' + vr
    download_url = 'https://github.com/mozilla/geckodriver/releases/download/'
    container = "%s/%s/geckodriver-%s-%s" % (download_url, vr, vr, comb)
    print('Downloading from: ' + str(container))
    try:
        url = container
        dest = directory
        obj = SmartDL(url, dest)
        obj.start()
        path = obj.get_dest()
        print(os.path.join(directory, 'geckodriver-' + vr + '-linux64.zip'))
        filepath = os.path.join(directory, a='geckodriver-' + vr + '-' + comb)
        if (filepath.endswith("tar.gz")):
            tar = tarfile.open(filepath, 'r:*')
            tar.extractall(directory)
            tar.close()
            path_msg = os.path.join(directory, "geckodriver")
            print("Use selenium driver path as " + path_msg)
    except Exception as e:
        print('Issues updating with error '+str(e))
