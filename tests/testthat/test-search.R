context("rgee: test-search test")

if (isFALSE(exists('ee'))) {
  ee_reattach()
  ee_Initialize(
    email = 'data.colec.fbf@gmail.com',
    drive = TRUE,
    gcs = TRUE
  )
}

test_that("simple search",{
  myquery <- ee_dataset(quiet = TRUE) %>%
    ee_search_type("Image")  %>%
    ee_search_provider("WWF") %>%
    ee_search_tags("srtm", "flow", "direction", "dem") %>%
    ee_search_tagstitle("srtm", "flow", "direction", "dem") %>%
    ee_search_title("15", "Flow", logical_operator = "AND")
  expect_type(myquery$id[1],"character")
})

test_that("testing date queries",{
  myquery <- ee_dataset() %>%
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

test_that("Update dataset",{
  my_db <- ee_dataset(upgrade = TRUE)
  expect_type(my_db$id,'character')
})

test_that("Get title",{
  tl <- ee_search_title_list(ee_dataset())
  expect_type(tl,'character')
})

test_that("ee_search_tagstitle - AND",{
  my_db <- ee_dataset() %>%
    ee_search_tagstitle("srtm",logical_operator = 'AND')
  expect_type(my_db$id,'character')
})

test_that("provider list",{
  expect_type(
    ee_dataset() %>% ee_search_provider_list(),
    "character"
  )
})

# ERROR ee_search ---------------------------------------------------------
test_that("error 01",{
  expect_error(
    ee_dataset() %>% ee_search_type("XxX")
  )
})

test_that("error 02",{
  expect_error(
    ee_dataset() %>% ee_search_provider("peru")
  )
})

test_that("error 03",{
  ee_s_search <- ee_dataset() %>%
    ee_search_tags("srtm", "flow", "direction", "dem", logical_operator = "OR")
  expect_s3_class(ee_s_search,"data.frame")
  ee_s_search <- ee_dataset() %>%
    ee_search_tags("srtm", "flow", "direction", "dem", logical_operator = "AND")
  expect_s3_class(ee_s_search,"data.frame")
  expect_error(ee_dataset() %>%
    ee_search_tags("srtm", "flow", "direction", "dem", logical_operator = "ORd")
  )
})

test_that("error 04",{
  ee_s_search <- ee_dataset() %>%
    ee_search_title("srtm", "flow", "direction", "dem", logical_operator = "OR")
  expect_s3_class(ee_s_search,"data.frame")
  expect_error(ee_dataset() %>%
                 ee_search_title("srtm", "flow", "direction", "dem", logical_operator = "ORd")
  )
})

test_that("error 05", {
  expect_error(
    ee_dataset() %>%
      ee_search_tagstitle("srtm", "flow", logical_operator = "ORd")
  )
})

test_that("error 05", {
  expect_error(
    ee_dataset() %>%
      ee_search_tagstitle("srtm", "flow", logical_operator = "ORd")
  )
})

test_that("ee_search_display", {
  ss <- ee_dataset() %>%
    ee_search_tagstitle("srtm", "flow", logical_operator = "OR") %>%
    ee_search_display(maxdisplay = 1)
  expect_equal(ss, TRUE)
})

