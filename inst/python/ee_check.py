#!/usr/bin/python
# -*- coding: utf-8 -*-

"""Check if Python modules exist

>>> ee_check_py_ee()
>>> ee_check_py_pyasn1()
>>> ee_check_py_urllib3()
>>> ee_check_py_setuptools()
>>> ee_check_py_oauth2client()

These functions will be used in R/ee_check.R
"""
def ee_check_py_pyasn1():
    import pyasn1
    return pyasn1.__version__

def ee_check_py_urllib3():
    import urllib3
    return urllib3.__version__

def ee_check_py_setuptools():
    import setuptools
    setuptools.__version__

def ee_check_py_oauth2client():
    import oauth2client
    return oauth2client.__version__

def ee_check_py_numpy():
    import numpy
    return numpy.__version__

def ee_check_py_ee():
    import ee
    return ee.__version__
