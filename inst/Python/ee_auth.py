import ee
import webbrowser

def request_ee_code():
    # get authorisation url
    auth_url = ee.oauth.get_authorization_url()
    # call auth_url in browser to grand access by the user
    webbrowser.open_new(auth_url)

def request_ee_token(auth_code):
    token = ee.oauth.request_token(auth_code)
    ee.oauth.write_token(token)


