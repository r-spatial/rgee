library(rgee)

# ee_search_provider_list()
# ee_search_title_list()
myquery <- ee_search_dataset() %>%
  ee_search_type("Image") %>%
  ee_search_provider("WWF") %>%
  ee_search_tags("srtm", "flow", "direction", "dem") %>%
  ee_search_title("15", "Flow", logical_operator = "AND") %>%
  ee_search_display()
