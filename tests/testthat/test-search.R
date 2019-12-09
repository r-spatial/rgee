context("rgee: test-search test")

test_that("simple search",{
  myquery <- ee_dataset() %>%
    ee_search_type("Image")  %>%
    ee_search_provider("WWF") %>%
    ee_search_tags("srtm", "flow", "direction", "dem") %>%
    ee_search_tagstitle("srtm", "flow", "direction", "dem") %>%
    ee_search_title("15", "Flow", logical_operator = "AND")
  expect_equal(myquery$id[1],"WWF/HydroSHEDS/15ACC")
})
