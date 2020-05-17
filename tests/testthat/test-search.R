context("rgee: test-search test")

# Pre-checking ------------------------------------------------------
# Google credentials were loaded in the system?
skip_if_no_credentials <- function(user) {
  ee_path <- path.expand(sprintf("~/.config/earthengine/%s", user))
  credentials <- list.files(
    path = ee_path,
    pattern = "@gmail.com|credentials|GCS_AUTH_FILE.json"
  )
  if (length(credentials) != 3) {
    skip("All google credentials were not found")
  }
}

# Neccesary Python packages were loaded?
skip_if_no_pypkg <- function() {
  have_ee <- reticulate::py_module_available("ee")
  have_numpy <- reticulate::py_module_available("numpy")
  if (isFALSE(have_ee)) {
    skip("ee not available for testing")
  }
  if (isFALSE(have_numpy)) {
    skip("numpy not available for testing")
  }
}

# Init Earth Engine just if it is necessary
init_rgee <- function() {
  ee_reattach()
  tryCatch(
    expr = ee$Image()$getInfo(),
    error = function(e) {
      ee_reattach()
      ee_Initialize(
        email = 'data.colec.fbf@gmail.com',
        drive = TRUE,
        gcs = TRUE
      )
    }
  )
}

user <- "data.colec.fbf"
skip_if_no_credentials(user)
skip_if_no_pypkg()
init_rgee()
# -------------------------------------------------------------------------


db <- paste0(
  "https://raw.githubusercontent.com/csaybar/Earth-Engine-Datasets-List/",
  "10c09b65e93d156c297628f035bf372b101867d3/eed-2020-03-30.csv",
  collapse = ""
)

test_that("simple search",{
  myquery <- ee_search_dataset(path_dataset = db, upgrade = TRUE) %>%
    ee_search_type("Image")  %>%
    ee_search_provider("WWF") %>%
    ee_search_tags("srtm", "flow", "direction", "dem") %>%
    ee_search_tagstitle("srtm", "flow", "direction", "dem") %>%
    ee_search_title("15", "Flow", logical_operator = "AND")
  expect_type(myquery$id[1],"character")
})

test_that("testing date queries",{
  myquery <- ee_search_dataset() %>%
    ee_search_type("Image")  %>%
    ee_search_startdate('2010-01-01') %>%
    ee_search_enddate('2010-12-31')
  extract_year <- regmatches(x = myquery$start_date,
                             m = regexpr(
                                pattern = "..$",
                                text =  myquery$start_date)
                            )
  expect_equal(mean(as.numeric(extract_year)),10)
})


test_that("Get title",{
  tl <- ee_search_title_list(ee_search_dataset())
  expect_type(tl,'character')
})

test_that("ee_search_tagstitle - AND",{
  my_db <- ee_search_dataset() %>%
    ee_search_tagstitle("srtm",logical_operator = 'AND')
  expect_type(my_db$id,'character')
})

test_that("provider list",{
  expect_type(
    ee_search_dataset() %>% ee_search_provider_list(),
    "character"
  )
})

# ERROR ee_search ---------------------------------------------------------
test_that("error 01",{
  expect_error(
    ee_search_dataset() %>% ee_search_type("XxX")
  )
})

test_that("error 02",{
  expect_error(
    ee_search_dataset() %>% ee_search_provider("peru")
  )
})

test_that("error 03",{
  ee_s_search <- ee_search_dataset() %>%
    ee_search_tags("srtm", "flow", "direction", "dem", logical_operator = "OR")
  expect_s3_class(ee_s_search,"data.frame")
  ee_s_search <- ee_search_dataset() %>%
    ee_search_tags("srtm", "flow", "direction", "dem", logical_operator = "AND")
  expect_s3_class(ee_s_search,"data.frame")
  expect_error(ee_search_dataset() %>%
    ee_search_tags("srtm", "flow", "direction", "dem", logical_operator = "ORd")
  )
})

test_that("error 04",{
  ee_s_search <- ee_search_dataset() %>%
    ee_search_title("srtm", "flow", "direction", "dem", logical_operator = "OR")
  expect_s3_class(ee_s_search,"data.frame")
  expect_error(ee_search_dataset() %>%
                 ee_search_title("srtm", "flow", "direction", "dem",
                                 logical_operator = "ORd")
  )
})

test_that("error 05", {
  expect_error(
    ee_search_dataset() %>%
      ee_search_tagstitle("srtm", "flow", logical_operator = "ORd")
  )
})

test_that("error 05", {
  expect_error(
    ee_search_dataset() %>%
      ee_search_tagstitle("srtm", "flow", logical_operator = "ORd")
  )
})

test_that("ee_search_display", {
  ss <- ee_search_dataset() %>%
    ee_search_tagstitle("srtm", "flow", logical_operator = "OR") %>%
    ee_search_display(maxdisplay = 1)
  expect_equal(ss, TRUE)
})

