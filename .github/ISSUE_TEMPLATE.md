* rgee version:
* R version:
* Operating System:

#### At submit an issue, please attached the following information of your `rgee` session:

- [ ] You have the Python API installed (from terminal):
```
earthengine -h
```

- [ ] You can find the credentials file on your system: 
```r
library(rgee)
ee_path <- path.expand("~/.config/earthengine/credentials")
file.exists(ee_path)
```
- [ ] You can run a simple EE command from R: 

```r
library(rgee)

# Initialize the Earth Engine module.
ee_Initialize()

# Print metadata for a DEM dataset.
print(ee$Image('USGS/SRTMGL1_003')$getInfo())
``` 

Attach your Python (reticulate) configuration:

```r
library(reticulate)
py_config()
```

### Description

Describe what you were trying to get done.
Tell us what happened, what went wrong, and what you expected to happen.

### What I Did

```
Paste the command(s) you ran and the output.
If there was a crash, please include the traceback here.
```
