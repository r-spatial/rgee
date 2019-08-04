import ee
import webbrowser

def ee_authenticate_py():
    # get authorisation url
    auth_url = ee.oauth.get_authorization_url()
    # call auth_url in browser to grand access by the user
    webbrowser.open_new(auth_url)
    return True

def request_ee_token_py(auth_code):
    token = ee.oauth.request_token(auth_code)
    ee.oauth.write_token(token)

def ee_init_py():
  try:
    ee.Initialize()
  except ee.EEException as e:
    print('The Earth Engine package failed to initialize; delete credentials with ee_remove_credentials() and try again.')
  except:
    print("Unexpected error:", sys.exc_info()[0])
    raise
