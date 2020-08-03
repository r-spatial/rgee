#' rgee Demo #6: Available cloud-free Landsat images for a specific region
#' @author Antony Barja

library(tidyverse)
library(rgee)
library(sf)

ee_Initialize()

ee_search_dataset() %>%
  ee_search_type("ImageCollection") %>%
  ee_search_provider("USGS") %>%
  ee_search_tags("reflectance") %>%
  select(id)

# Functions ---------------------------------------------------------------
img_available <- function(id, path, row, cloud) {
  options(dplyr.summarise.inform = FALSE)
  lista <- list()
  for (i in 1:length(id)) {
    images <- ee$ImageCollection(id[i])$
      filter(ee$Filter$eq("WRS_PATH", path))$
      filter(ee$Filter$eq("WRS_ROW", row))$
      filterMetadata("CLOUD_COVER", "less_than", cloud)

    ee_get_date_ic(images) %>%
      as_tibble() %>%
      separate(time_start, into = c("anio", "mes", "dia"), sep = "-") %>%
      group_by(anio, mes, .drop = FALSE) %>%
      summarise(total = as.integer(table(mes))) %>%
      mutate(type = str_sub(id[i], start = 9, end = 12)) -> images_date

    lista[[i]] <- images_date
    lista_end <- do.call(rbind, lapply(lista, function(x) as.data.frame((x))))
  }
  return(lista_end)
}

plot_available_img <- function(name, img_available) {
  plot_img <- img_available %>%
    as_tibble() %>% 
    mutate_if(is.character,as.factor) %>% 
    ggplot(aes(x = anio , y = factor(mes, labels = month.abb[1:length(levels(mes))]))) +
    geom_raster(aes(fill = type), alpha = 0.7) +
    geom_point(aes(size = factor(total)), color = "black") +
    geom_point(shape = 21, color = "white", size = 3) +
    scale_fill_viridis_d("Type", direction = -1, option = "plasma") +
    theme_minimal() +
    theme() +
    labs(
      title = paste0("A set of available images of ", name),
      x = "",
      y = "",
      size = "Total \nimages"
    )
  print(plot_img)
}


names_id <- c(
  "LANDSAT/LC08/C01/T1_SR",
  "LANDSAT/LT05/C01/T1_SR",
  "LANDSAT/LE07/C01/T1_SR"
)

lista <- img_available(
  id = names_id,
  path = 4,
  row = 71,
  cloud = 5
)

plot_available_img(name = "Landsat 5, 7 and 8", img_available = lista)
