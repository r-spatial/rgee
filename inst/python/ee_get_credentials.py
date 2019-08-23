#!/usr/bin/python
# -*- coding: utf-8 -*-
import ee
import webbrowser

"""Helper function for authenticating Earth Engine Accounts since R

>>> ee_authenticate_py()
>>> request_ee_token_py(auth_code)

These functions will be used in R/ee_check.R
"""


def ee_authenticate_py():
    auth_url = ee.oauth.get_authorization_url()
    # call auth_url in browser to grand access by the user
    webbrowser.open_new(auth_url)
    return True


def request_ee_token_py(auth_code):
    token = ee.oauth.request_token(auth_code)
    ee.oauth.write_token(token)
