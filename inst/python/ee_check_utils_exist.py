#!/usr/bin/python
# -*- coding: utf-8 -*-

"""Check if Python modules exist

>>> ee_check_py_webbrowser()
>>> ee_check_py_platform()
>>> ee_check_py_requests()
>>> ee_check_py_zipfile()
>>> ee_check_py_tarfile()
>>> ee_check_py_subprocess()
>>> ee_check_py_ast()
>>> ee_check_py_sys()
>>> ee_check_py_os()
>>> ee_check_py_time()
>>> ee_check_py_json()

>>> ee_check_py_ee()
>>> ee_check_py_selenium()
>>> ee_check_py_bs4()
>>> ee_check_py_requests_toolbelt()

These functions will be used in R/ee_check.R
"""
def ee_check_py_webbrowser():
    try:
        import webbrowser
        try:
            return webbrowser.__version__
        except:
            return True
    except:
        return False

def ee_check_py_pickle():
    try:
        import pickle
        try:
            return pickle.__version__
        except:
            return True
    except:
        return False

def ee_check_py_platform():
    try:
        import platform
        try:
            return platform.__version__
        except:
            return True
    except:
        return False


def ee_check_py_requests():
    try:
        import requests
        try:
            return requests.__version__
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


def ee_check_py_json():
    try:
        import json
        try:
            return json.__version__
        except:
            return True
    except:
        return False


# Third-party package
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


def ee_check_py_requests_toolbelt():
    try:
        import requests_toolbelt
        return requests_toolbelt.__version__
    except:
        return False
