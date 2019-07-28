import ee
import webbrowser

def ee_authenticate():
    # get authorisation url
    auth_url = ee.oauth.get_authorization_url()
    # call auth_url in browser to grand access by the user
    webbrowser.open_new(auth_url)

def request_ee_token(auth_code):
    token = ee.oauth.request_token(auth_code)
    ee.oauth.write_token(token)

def ee_init():
  try:
    ee.Initialize()
    print('The Earth Engine package initialized successfully!')
  except ee.EEException as e:
    print('The Earth Engine package failed to initialize; try running ee.Initialize(get_credentials=TRUE)')
  except:
    print("Unexpected error:", sys.exc_info()[0])
    raise

def py_evaluate(x):
  ee_eval = eval(x)
  return ee_eval

def ee_mapview():
    try:
      ee.Initialize()
    except ee.EEException, e:
      print('The Earth Engine package failed to initialize; try running ee.Initialize(get_credentials=TRUE)')
