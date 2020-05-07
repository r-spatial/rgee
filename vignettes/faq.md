Here you can find possible **solutions for common `rgee` problems**. We
will add to this page as soon as additional problems are reported. If
these solutions do not work for you, or you have a different problem,
signal it at:
<a href="https://github.com/csaybar/rgee/issues" class="uri">https://github.com/csaybar/rgee/issues</a>

### Installation problem

#### ee\_create\_pyenv take by default a Python 2 version (ERROR: rgee just run under Python 3.5 &gt;=)

It is a common scenario mostly for Windows users that have a lot of
Python Envs setting on PATH. For solve this problem, take into account
the follows steps:

``` r
  # 1. Find manually the Python Env that you want to use, for instance:
  myPythonPath <- "C:/Python36/ArcGISx6410.4/python.exe"
  
  # 2. Set the RETICULATE PYTHON PATH (restart R to see changes).
  ee_set_pyenv(myPythonPath, install = TRUE)

  # 3. Create a Python environment, e.g. ee.
  myPythonEnv <- ee_create_pyenv(python_env = "ee")

  # 4. Set the RETICULATE_PYTHON and RETICULATE_PYTHON_ENV (restart R to see changes).
  ee_set_pyenv(myPythonEnv, install = TRUE)
  
  # 5. Install Python package dependencies (restart R to see changes).
  ee_install_python_packages()
  
  # 6. Initialize rgee again!
  ee_Initialize()
```
