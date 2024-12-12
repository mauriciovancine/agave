#' -----
#' aim: agave
#' author: mauricio
#' date: 2024-11-11
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(geobr)
library(terra)
library(tmap)
library(rgrass)
library(landscapemetrics)

# options
options(scipen = 1000)

# import data -------------------------------------------------------------

## carbon ----
# carbon <- terra::rast("01_data/StockChange_54052/SA_LUC_54052_v3.tif")
# carbon

## mapbiomas ----
# mapbiomas <- terra::rast("01_data/MapBiomas_Col8_54052/MB_Col8_SA_54052_v2.tif")
# mapbiomas

## municipalities ----
mun_tucano <- geobr::read_municipality(code_muni = 2931905, year = 2020) %>% 
    terra::vect() %>% 
    terra::project(., carbon)
mun_tucano

tm_shape(mun_tucano) +
    tm_polygons()

mun_itaiba <- geobr::read_municipality(code_muni = 2607505, year = 2020) %>% 
    terra::vect() %>% 
    terra::project(., carbon)
mun_itaiba

tm_shape(mun_itaiba) +
    tm_polygons()

mun_salinas <- geobr::read_municipality(code_muni = 3157005, year = 2020) %>% 
    terra::vect() %>% 
    terra::project(., carbon)
mun_salinas

tm_shape(mun_salinas) +
    tm_polygons()

# export
# terra::writeVector(mun_tucano, "01_data/municipios/mun_tucano.shp")
# terra::writeVector(mun_itaiba, "01_data/municipios/mun_itaiba.shp")
# terra::writeVector(mun_salinas, "01_data/municipios/mun_salinas.shp")

## maps ----
# plot(carbon)
# plot(mun_tucano, add = TRUE)
# plot(mun_itaiba, add = TRUE)
# plot(mun_salinas, add = TRUE)

# plot(mapbiomas)
# plot(mun_tucano, add = TRUE)
# plot(mun_itaiba, add = TRUE)
# plot(mun_salinas, add = TRUE)

# prepare data ------------------------------------------------------------

## crop ----
# carbon_tucano <- terra::crop(carbon, mun_tucano, mask = TRUE)
# carbon_itaiba <- terra::crop(carbon, mun_itaiba, mask = TRUE)
# carbon_salinas <- terra::crop(carbon, mun_salinas, mask = TRUE)

carbon_tucano <- terra::rast("01_data/carbon_tucano.tif")
carbon_itaiba <- terra::rast("01_data/carbon_itaiba.tif")
carbon_salinas <- terra::rast("01_data/carbon_salinas.tif")

mapbiomas_tucano <- terra::ifel(
    terra::crop(mapbiomas, mun_tucano, mask = TRUE) %in% c(3:6, 49, 11, 12, 50, 13), 1, 0) %>% 
    terra::crop(mun_tucano, mask = TRUE)
mapbiomas_itaiba <- terra::ifel(
    terra::crop(mapbiomas, mun_itaiba, mask = TRUE) %in% c(3:6, 49, 11, 12, 50, 13), 1, 0) %>% 
    terra::crop(mun_itaiba, mask = TRUE)
mapbiomas_salinas <- terra::ifel(
    terra::crop(mapbiomas, mun_salinas, mask = TRUE) %in% c(3:6, 49, 11, 12, 50, 13), 1, 0) %>% 
    terra::crop(mun_salinas, mask = TRUE)

mapbiomas_tucano <- terra::rast("01_data/mapbiomas_tucano.tif")
mapbiomas_itaiba <- terra::rast("01_data/mapbiomas_itaiba.tif")
mapbiomas_salinas <- terra::rast("01_data/mapbiomas_salinas.tif")

plot(carbon_tucano)
plot(mapbiomas_tucano)

plot(carbon_itaiba)
plot(mapbiomas_itaiba)

plot(carbon_salinas)
plot(mapbiomas_salinas)

## prepare carbon data 
par(mfrow = c(3, 1))
hist(carbon_tucano)
hist(carbon_itaiba)
hist(carbon_salinas)

da_carbon <- dplyr::bind_rows(
    tibble::tibble(carbon = as.numeric(carbon_tucano[]),
                   municipality = "tucano"),
    tibble::tibble(carbon = as.numeric(carbon_itaiba[]),
                   municipality = "itaiba"),
    tibble::tibble(carbon = as.numeric(carbon_salinas[]),
                   municipality = "salinas")) %>% 
    tidyr::drop_na()
da_carbon

ggplot(data = da_carbon, aes(x = carbon)) +
    geom_histogram(color = "white", fill = "steelblue", bins = 20) +
    # xlim(0, 50) +
    facet_grid(facets = ~municipality, scales = "free") +
    labs(x = "Carbon/hectare", y = "Frequency") +
    theme_bw(base_size = 20)
ggsave(filename = "02_results/00_hist_carbon.png", width = 30, height = 15, units = "cm", dpi = 300)

carbon_tucano20 <- carbon_tucano >= 20
carbon_itaiba20 <- carbon_itaiba >= 20
carbon_salinas20 <- carbon_salinas >= 20

carbon_tucano40 <- carbon_tucano >= 40
carbon_itaiba40 <- carbon_itaiba >= 40
carbon_salinas40 <- carbon_salinas >= 40

carbon_tucano20_40 <- terra::ifel(carbon_tucano >= 20 & carbon_tucano <= 40, 1, 0)
carbon_itaiba20_40 <- terra::ifel(carbon_itaiba >= 20 & carbon_itaiba <= 40, 1, 0)
carbon_salinas20_40 <- terra::ifel(carbon_salinas >= 20 & carbon_salinas <= 40, 1, 0)

mapbiomas_tucano_carbon20 <- mapbiomas_tucano + carbon_tucano20
mapbiomas_tucano_carbon20 <- terra::ifel(mapbiomas_tucano_carbon20 > 0, 1, mapbiomas_tucano_carbon20)
mapbiomas_tucano_carbon20

mapbiomas_itaiba_carbon20 <- mapbiomas_itaiba + carbon_itaiba20
mapbiomas_itaiba_carbon20 <- terra::ifel(mapbiomas_itaiba_carbon20 > 0, 1, mapbiomas_itaiba_carbon20)
mapbiomas_itaiba_carbon20

mapbiomas_salinas_carbon20 <- mapbiomas_salinas + carbon_salinas20
mapbiomas_salinas_carbon20 <- terra::ifel(mapbiomas_salinas_carbon20 > 0, 1, mapbiomas_salinas_carbon20)
mapbiomas_salinas_carbon20

mapbiomas_tucano_carbon40 <- mapbiomas_tucano + carbon_tucano40
mapbiomas_tucano_carbon40 <- terra::ifel(mapbiomas_tucano_carbon40 > 0, 1, mapbiomas_tucano_carbon40)
mapbiomas_tucano_carbon40

mapbiomas_itaiba_carbon40 <- mapbiomas_itaiba + carbon_itaiba40
mapbiomas_itaiba_carbon40 <- terra::ifel(mapbiomas_itaiba_carbon40 > 0, 1, mapbiomas_itaiba_carbon40)
mapbiomas_itaiba_carbon40

mapbiomas_salinas_carbon40 <- mapbiomas_salinas + carbon_salinas40
mapbiomas_salinas_carbon40 <- terra::ifel(mapbiomas_salinas_carbon40 > 0, 1, mapbiomas_salinas_carbon40)
mapbiomas_salinas_carbon40

plot(carbon_tucano20)
plot(mapbiomas_tucano)
plot(mapbiomas_tucano_carbon20)
plot(mun_tucano, border = "red", lwd = 2, add = TRUE)

plot(carbon_tucano40)
plot(mapbiomas_tucano)
plot(mapbiomas_tucano_carbon40)
plot(mun_tucano, border = "red", lwd = 2, add = TRUE)

plot(carbon_itaiba20)
plot(mapbiomas_itaiba)
plot(mapbiomas_itaiba_carbon20)
plot(mun_itaiba, border = "red", lwd = 2, add = TRUE)

plot(carbon_itaiba40)
plot(mapbiomas_itaiba)
plot(mapbiomas_itaiba_carbon40)
plot(mun_itaiba, border = "red", lwd = 2, add = TRUE)

plot(carbon_salinas20)
plot(mapbiomas_salinas)
plot(mapbiomas_salinas_carbon20)
plot(mun_salinas, border = "red", lwd = 2, add = TRUE)

plot(carbon_salinas40)
plot(mapbiomas_salinas)
plot(mapbiomas_salinas_carbon40)
plot(mun_salinas, border = "red", lwd = 2, add = TRUE)

# export
terra::writeRaster(carbon_tucano, "01_data/municipios/carbon_tucano.tif", overwrite = TRUE)
terra::writeRaster(carbon_tucano20, "01_data/municipios/carbon_tucano20.tif", overwrite = TRUE)
terra::writeRaster(carbon_tucano20_40, "01_data/municipios/carbon_tucano20_40.tif", overwrite = TRUE)
terra::writeRaster(carbon_tucano40, "01_data/municipios/carbon_tucano40.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_tucano, "01_data/municipios/mapbiomas_tucano.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_tucano_carbon40, "01_data/municipios/mapbiomas_tucano_carbon20.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_tucano_carbon40, "01_data/municipios/mapbiomas_tucano_carbon40.tif", overwrite = TRUE)

terra::writeRaster(carbon_itaiba, "01_data/municipios/carbon_itaiba.tif", overwrite = TRUE)
terra::writeRaster(carbon_itaiba20, "01_data/municipios/carbon_itaiba20.tif", overwrite = TRUE)
terra::writeRaster(carbon_itaiba20_40, "01_data/municipios/carbon_itaiba20_40.tif", overwrite = TRUE)
terra::writeRaster(carbon_itaiba40, "01_data/municipios/carbon_itaiba40.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba, "01_data/municipios/mapbiomas_itaiba.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_carbon20, "01_data/municipios/mapbiomas_itaiba_carbon20.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_carbon40, "01_data/municipios/mapbiomas_itaiba_carbon40.tif", overwrite = TRUE)

terra::writeRaster(carbon_salinas, "01_data/municipios/carbon_salinas.tif", overwrite = TRUE)
terra::writeRaster(carbon_salinas20, "01_data/municipios/carbon_salinas20.tif", overwrite = TRUE)
terra::writeRaster(carbon_salinas20_40, "01_data/municipios/carbon_salinas20_40.tif", overwrite = TRUE)
terra::writeRaster(carbon_salinas40, "01_data/municipios/carbon_salinas40.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas, "01_data/municipios/mapbiomas_salinas.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_carbon20, "01_data/municipios/mapbiomas_salinas_carbon20.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_carbon40, "01_data/municipios/mapbiomas_salinas_carbon40.tif", overwrite = TRUE)

# metrics -----------------------------------------------------------------

## number of patches and area ----
lms_area_tucano <- landscapemetrics::calculate_lsm(mapbiomas_tucano, metric = "area") %>% 
    dplyr::filter(metric == "area", class == 1) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(np = n(),
                     area_total = sum(value),
                     area_mn = mean(value),
                     area_sd = sd(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(class = "tucano") %>% 
    dplyr::select(-metric)
lms_area_tucano

lms_area_tucano_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_tucano_carbon20, metric = "area") %>%
    dplyr::filter(metric == "area", class == 1) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(np = n(),
                     area_total = sum(value),
                     area_mn = mean(value),
                     area_sd = sd(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(class = "tucano_carbon20") %>% 
    dplyr::select(-metric)
lms_area_tucano_carbon20

lms_area_tucano_carbon40 <- landscapemetrics::calculate_lsm(mapbiomas_tucano_carbon40, metric = "area") %>%
    dplyr::filter(metric == "area", class == 1) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(np = n(),
                     area_total = sum(value),
                     area_mn = mean(value),
                     area_sd = sd(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(class = "tucano_carbon40") %>% 
    dplyr::select(-metric)
lms_area_tucano_carbon40

lms_area_itaiba <- landscapemetrics::calculate_lsm(mapbiomas_itaiba, metric = "area") %>% 
    dplyr::filter(metric == "area", class == 1) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(np = n(),
                     area_total = sum(value),
                     area_mn = mean(value),
                     area_sd = sd(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(class = "itaiba") %>% 
    dplyr::select(-metric)
lms_area_itaiba

lms_area_itaiba_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_itaiba_carbon20, metric = "area") %>%
    dplyr::filter(metric == "area", class == 1) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(np = n(),
                     area_total = sum(value),
                     area_mn = mean(value),
                     area_sd = sd(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(class = "itaiba_carbon20") %>% 
    dplyr::select(-metric)
lms_area_itaiba_carbon20

lms_area_itaiba_carbon40 <- landscapemetrics::calculate_lsm(mapbiomas_itaiba_carbon40, metric = "area") %>%
    dplyr::filter(metric == "area", class == 1) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(np = n(),
                     area_total = sum(value),
                     area_mn = mean(value),
                     area_sd = sd(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(class = "itaiba_carbon40") %>% 
    dplyr::select(-metric)
lms_area_itaiba_carbon40

lms_area_salinas <- landscapemetrics::calculate_lsm(mapbiomas_salinas, metric = "area") %>% 
    dplyr::filter(metric == "area", class == 1) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(np = n(),
                     area_total = sum(value),
                     area_mn = mean(value),
                     area_sd = sd(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(class = "salinas") %>% 
    dplyr::select(-metric)
lms_area_salinas

lms_area_salinas_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_salinas_carbon20, metric = "area") %>% 
    dplyr::filter(metric == "area", class == 1) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(np = n(),
                     area_total = sum(value),
                     area_mn = mean(value),
                     area_sd = sd(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(class = "salinas_carbon20") %>% 
    dplyr::select(-metric)
lms_area_salinas_carbon20

lms_area_salinas_carbon40 <- landscapemetrics::calculate_lsm(mapbiomas_salinas_carbon40, metric = "area") %>% 
    dplyr::filter(metric == "area", class == 1) %>% 
    dplyr::group_by(metric) %>% 
    dplyr::summarise(np = n(),
                     area_total = sum(value),
                     area_mn = mean(value),
                     area_sd = sd(value)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(class = "salinas_carbon40") %>% 
    dplyr::select(-metric)
lms_area_salinas_carbon40

## shape ----
lms_shape_tucano <- landscapemetrics::calculate_lsm(mapbiomas_tucano, metric = "shape") %>% 
    dplyr::filter(class == 1, metric %in% c("shape_mn", "shape_sd")) %>% 
    dplyr::mutate(class = "tucano") %>% 
    dplyr::select(class, metric, value)
lms_shape_tucano

lms_shape_tucano_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_tucano_carbon20, metric = "shape") %>% 
    dplyr::filter(class == 1, metric %in% c("shape_mn", "shape_sd")) %>% 
    dplyr::mutate(class = "tucano_carbon20") %>% 
    dplyr::select(class, metric, value)
lms_shape_tucano_carbon20

lms_shape_tucano_carbon40 <- landscapemetrics::calculate_lsm(mapbiomas_tucano_carbon40, metric = "shape") %>% 
    dplyr::filter(class == 1, metric %in% c("shape_mn", "shape_sd")) %>% 
    dplyr::mutate(class = "tucano_carbon40") %>% 
    dplyr::select(class, metric, value)
lms_shape_tucano_carbon40

lms_shape_itaiba <- landscapemetrics::calculate_lsm(mapbiomas_itaiba, metric = "shape") %>% 
    dplyr::filter(class == 1, metric %in% c("shape_mn", "shape_sd")) %>% 
    dplyr::mutate(class = "itaiba") %>% 
    dplyr::select(class, metric, value)
lms_shape_itaiba

lms_shape_itaiba_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_itaiba_carbon20, metric = "shape") %>% 
    dplyr::filter(class == 1, metric %in% c("shape_mn", "shape_sd")) %>% 
    dplyr::mutate(class = "itaiba_carbon20") %>% 
    dplyr::select(class, metric, value)
lms_shape_itaiba_carbon20

lms_shape_itaiba_carbon40 <- landscapemetrics::calculate_lsm(mapbiomas_itaiba_carbon40, metric = "shape") %>% 
    dplyr::filter(class == 1, metric %in% c("shape_mn", "shape_sd")) %>% 
    dplyr::mutate(class = "itaiba_carbon40") %>% 
    dplyr::select(class, metric, value)
lms_shape_itaiba_carbon40

lms_shape_salinas <- landscapemetrics::calculate_lsm(mapbiomas_salinas, metric = "shape") %>% 
    dplyr::filter(class == 1, metric %in% c("shape_mn", "shape_sd")) %>% 
    dplyr::mutate(class = "salinas") %>% 
    dplyr::select(class, metric, value)
lms_shape_salinas

lms_shape_salinas_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_salinas_carbon20, metric = "shape") %>% 
    dplyr::filter(class == 1, metric %in% c("shape_mn", "shape_sd")) %>% 
    dplyr::mutate(class = "salinas_carbon20") %>% 
    dplyr::select(class, metric, value)
lms_shape_salinas_carbon20

lms_shape_salinas_carbon40 <- landscapemetrics::calculate_lsm(mapbiomas_salinas_carbon40, metric = "shape") %>% 
    dplyr::filter(class == 1, metric %in% c("shape_mn", "shape_sd")) %>% 
    dplyr::mutate(class = "salinas_carbon40") %>% 
    dplyr::select(class, metric, value)
lms_shape_salinas_carbon40

## core ----
lms_core_tucano <- landscapemetrics::calculate_lsm(mapbiomas_tucano, metric = "core") %>% 
    dplyr::filter(class == 1, metric %in% c("core_mn", "core_sd")) %>% 
    dplyr::mutate(class = "tucano") %>% 
    dplyr::select(class, metric, value)
lms_core_tucano

lms_core_tucano_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_tucano_carbon20, metric = "core") %>% 
    dplyr::filter(class == 1, metric %in% c("core_mn", "core_sd")) %>% 
    dplyr::mutate(class = "tucano_carbon20") %>% 
    dplyr::select(class, metric, value)
lms_core_tucano_carbon20

lms_core_itaiba <- landscapemetrics::calculate_lsm(mapbiomas_itaiba, metric = "core") %>% 
    dplyr::filter(class == 1, metric %in% c("core_mn", "core_sd")) %>% 
    dplyr::mutate(class = "itaiba") %>% 
    dplyr::select(class, metric, value)
lms_core_itaiba

lms_core_itaiba_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_itaiba_carbon20, metric = "core") %>% 
    dplyr::filter(class == 1, metric %in% c("core_mn", "core_sd")) %>% 
    dplyr::mutate(class = "itaiba_carbon20") %>% 
    dplyr::select(class, metric, value)
lms_core_itaiba_carbon20

lms_core_itaiba_carbon40 <- landscapemetrics::calculate_lsm(mapbiomas_itaiba_carbon40, metric = "core") %>% 
    dplyr::filter(class == 1, metric %in% c("core_mn", "core_sd")) %>% 
    dplyr::mutate(class = "itaiba_carbon40") %>% 
    dplyr::select(class, metric, value)
lms_core_itaiba_carbon40

lms_core_salinas <- landscapemetrics::calculate_lsm(mapbiomas_salinas, metric = "core") %>% 
    dplyr::filter(class == 1, metric %in% c("core_mn", "core_sd")) %>% 
    dplyr::mutate(class = "salinas") %>% 
    dplyr::select(class, metric, value)
lms_core_salinas

lms_core_salinas_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_salinas_carbon20, metric = "core") %>% 
    dplyr::filter(class == 1, metric %in% c("core_mn", "core_sd")) %>% 
    dplyr::mutate(class = "salinas_carbon20") %>% 
    dplyr::select(class, metric, value)
lms_core_salinas_carbon20

lms_core_salinas_carbon40 <- landscapemetrics::calculate_lsm(mapbiomas_salinas_carbon40, metric = "core") %>% 
    dplyr::filter(class == 1, metric %in% c("core_mn", "core_sd")) %>% 
    dplyr::mutate(class = "salinas_carbon40") %>% 
    dplyr::select(class, metric, value)
lms_core_salinas_carbon40

## enn ----
lms_enn_tucano <- landscapemetrics::calculate_lsm(mapbiomas_tucano, metric = "enn") %>% 
    dplyr::filter(class == 1, metric %in% c("enn_mn", "enn_sd")) %>% 
    dplyr::mutate(class = "tucano") %>% 
    dplyr::select(class, metric, value)
lms_enn_tucano

lms_enn_tucano_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_tucano_carbon20, metric = "enn") %>% 
    dplyr::filter(class == 1, metric %in% c("enn_mn", "enn_sd")) %>% 
    dplyr::mutate(class = "tucano_carbon20") %>% 
    dplyr::select(class, metric, value)
lms_enn_tucano_carbon20

lms_enn_tucano_carbon40 <- landscapemetrics::calculate_lsm(mapbiomas_tucano_carbon40, metric = "enn") %>% 
    dplyr::filter(class == 1, metric %in% c("enn_mn", "enn_sd")) %>% 
    dplyr::mutate(class = "tucano_carbon40") %>% 
    dplyr::select(class, metric, value)
lms_enn_tucano_carbon40

lms_enn_itaiba <- landscapemetrics::calculate_lsm(mapbiomas_itaiba, metric = "enn") %>% 
    dplyr::filter(class == 1, metric %in% c("enn_mn", "enn_sd")) %>% 
    dplyr::mutate(class = "itaiba") %>% 
    dplyr::select(class, metric, value)
lms_enn_itaiba

lms_enn_itaiba_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_itaiba_carbon20, metric = "enn") %>% 
    dplyr::filter(class == 1, metric %in% c("enn_mn", "enn_sd")) %>% 
    dplyr::mutate(class = "itaiba_carbon20") %>% 
    dplyr::select(class, metric, value)
lms_enn_itaiba_carbon20

lms_enn_itaiba_carbon40 <- landscapemetrics::calculate_lsm(mapbiomas_itaiba_carbon40, metric = "enn") %>% 
    dplyr::filter(class == 1, metric %in% c("enn_mn", "enn_sd")) %>% 
    dplyr::mutate(class = "itaiba_carbon40") %>% 
    dplyr::select(class, metric, value)
lms_enn_itaiba_carbon40

lms_enn_salinas <- landscapemetrics::calculate_lsm(mapbiomas_salinas, metric = "enn") %>% 
    dplyr::filter(class == 1, metric %in% c("enn_mn", "enn_sd")) %>% 
    dplyr::mutate(class = "salinas") %>% 
    dplyr::select(class, metric, value)
lms_enn_salinas

lms_enn_salinas_carbon20 <- landscapemetrics::calculate_lsm(mapbiomas_salinas_carbon20, metric = "enn") %>% 
    dplyr::filter(class == 1, metric %in% c("enn_mn", "enn_sd")) %>% 
    dplyr::mutate(class = "salinas_carbon20") %>% 
    dplyr::select(class, metric, value)
lms_enn_salinas_carbon20

lms_enn_salinas_carbon40 <- landscapemetrics::calculate_lsm(mapbiomas_salinas_carbon40, metric = "enn") %>% 
    dplyr::filter(class == 1, metric %in% c("enn_mn", "enn_sd")) %>% 
    dplyr::mutate(class = "salinas_carbon40") %>% 
    dplyr::select(class, metric, value)
lms_enn_salinas_carbon40

## bind metrics ----
metrics <- dplyr::bind_rows(
    lms_area_tucano,
    lms_area_tucano_carbon20,
    lms_area_tucano_carbon40,
    lms_area_itaiba,
    lms_area_itaiba_carbon20,
    lms_area_itaiba_carbon40,
    lms_area_salinas,
    lms_area_salinas_carbon20,
    lms_area_salinas_carbon40) %>% 
    tidyr::pivot_longer(-class, names_to = "metric", values_to = "value") %>% 
    tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
    dplyr::bind_rows(
        dplyr::bind_cols(
            lms_core_tucano %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value),
            lms_core_tucano_carbon20 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_core_tucano_carbon40 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_core_itaiba %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_core_itaiba_carbon20 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_core_itaiba_carbon40 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_core_salinas %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_core_salinas_carbon20 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_core_salinas_carbon40 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2)),
        dplyr::bind_cols(
            lms_shape_tucano %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value),
            lms_shape_tucano_carbon20 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_shape_tucano_carbon40 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_shape_itaiba %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_shape_itaiba_carbon20 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_shape_itaiba_carbon40 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_shape_salinas %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_shape_salinas_carbon20 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_shape_salinas_carbon40 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2)),
        dplyr::bind_cols(
            lms_enn_tucano %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value),
            lms_enn_tucano_carbon20 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_enn_tucano_carbon40 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_enn_itaiba %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_enn_itaiba_carbon20 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_enn_itaiba_carbon40 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_enn_salinas %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_enn_salinas_carbon20 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2),
            lms_enn_salinas_carbon40 %>% 
                tidyr::pivot_wider(id_cols = metric, names_from = class, values_from = value) %>% 
                dplyr::select(2))) %>% 
    dplyr::mutate_at(.vars = vars(tucano:salinas_carbon40), round, 2)
metrics

## balance ----
metrics_balance <- metrics %>% 
    dplyr::mutate(tucano_balance20 = (tucano_carbon20 - tucano)/tucano_carbon20 * 100, 
                  .after = tucano_carbon20) %>%
    dplyr::mutate(tucano_balance40 = (tucano_carbon40 - tucano)/tucano_carbon40 * 100, 
                  .after = tucano_carbon40) %>% 
    dplyr::mutate(itaiba_balance20 = (itaiba_carbon20 - itaiba)/itaiba_carbon20 * 100, 
                  .after = itaiba_carbon20) %>%
    dplyr::mutate(itaiba_balance40 = (itaiba_carbon40 - itaiba)/itaiba_carbon40 * 100, 
                  .after = itaiba_carbon40) %>% 
    dplyr::mutate(salinas_balance20 = (salinas_carbon20 - salinas)/salinas_carbon20 * 100, 
                  .after = salinas_carbon20) %>% 
    dplyr::mutate(salinas_balance40 = (salinas_carbon40 - salinas)/salinas_carbon40 * 100, 
                  .after = salinas_carbon40) %>% 
    dplyr::mutate_at(.vars = vars(tucano:salinas_balance40), round, 2)
metrics_balance

## export ----
writexl::write_xlsx(metrics_balance, "02_results/landscape_metrics_balance.xlsx")

# spatialize metrics ---------------------------------------------------------

## area ----
mapbiomas_tucano_area_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano == 1, 1, NA), progress = TRUE, metric = "area")
mapbiomas_tucano_area_spatial
plot(mapbiomas_tucano_area_spatial$layer_1$lsm_p_area)

mapbiomas_tucano_carbon20_area_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano_carbon20 == 1, 1, NA), progress = TRUE, metric = "area")
mapbiomas_tucano_carbon20_area_spatial
plot(mapbiomas_tucano_carbon20_area_spatial$layer_1$lsm_p_area)

mapbiomas_tucano_carbon40_area_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano_carbon40 == 1, 1, NA), progress = TRUE, metric = "area")
mapbiomas_tucano_carbon40_area_spatial
plot(mapbiomas_tucano_carbon40_area_spatial$layer_1$lsm_p_area)

mapbiomas_itaiba_area_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba == 1, 1, NA), progress = TRUE, metric = "area")
mapbiomas_itaiba_area_spatial
plot(mapbiomas_itaiba_area_spatial$layer_1$lsm_p_area)

mapbiomas_itaiba_carbon20_area_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba_carbon20 == 1, 1, NA), progress = TRUE, metric = "area")
mapbiomas_itaiba_carbon20_area_spatial
plot(mapbiomas_itaiba_carbon20_area_spatial$layer_1$lsm_p_area)

mapbiomas_itaiba_carbon40_area_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba_carbon40 == 1, 1, NA), progress = TRUE, metric = "area")
mapbiomas_itaiba_carbon40_area_spatial
plot(mapbiomas_itaiba_carbon40_area_spatial$layer_1$lsm_p_area)

mapbiomas_salinas_area_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas == 1, 1, NA), progress = TRUE, metric = "area")
mapbiomas_salinas_area_spatial
plot(mapbiomas_salinas_area_spatial$layer_1$lsm_p_area)

mapbiomas_salinas_carbon20_area_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas_carbon20 == 1, 1, NA), progress = TRUE, metric = "area")
mapbiomas_salinas_carbon20_area_spatial
plot(mapbiomas_salinas_carbon20_area_spatial$layer_1$lsm_p_area)

mapbiomas_salinas_carbon40_area_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas_carbon40 == 1, 1, NA), progress = TRUE, metric = "area")
mapbiomas_salinas_carbon40_area_spatial
plot(mapbiomas_salinas_carbon40_area_spatial$layer_1$lsm_p_area)

## core ----
mapbiomas_tucano_core_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano == 1, 1, NA), progress = TRUE, metric = "core")
mapbiomas_tucano_core_spatial
plot(mapbiomas_tucano_core_spatial$layer_1$lsm_p_core)

mapbiomas_tucano_carbon20_core_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano_carbon20 == 1, 1, NA), progress = TRUE, metric = "core")
mapbiomas_tucano_carbon20_core_spatial
plot(mapbiomas_tucano_carbon20_core_spatial$layer_1$lsm_p_core)

mapbiomas_tucano_carbon40_core_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano_carbon40 == 1, 1, NA), progress = TRUE, metric = "core")
mapbiomas_tucano_carbon40_core_spatial
plot(mapbiomas_tucano_carbon40_core_spatial$layer_1$lsm_p_core)

mapbiomas_itaiba_core_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba == 1, 1, NA), progress = TRUE, metric = "core")
mapbiomas_itaiba_core_spatial
plot(mapbiomas_itaiba_core_spatial$layer_1$lsm_p_core)

mapbiomas_itaiba_carbon20_core_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba_carbon20 == 1, 1, NA), progress = TRUE, metric = "core")
mapbiomas_itaiba_carbon20_core_spatial
plot(mapbiomas_itaiba_carbon20_core_spatial$layer_1$lsm_p_core)

mapbiomas_itaiba_carbon40_core_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba_carbon40 == 1, 1, NA), progress = TRUE, metric = "core")
mapbiomas_itaiba_carbon40_core_spatial
plot(mapbiomas_itaiba_carbon40_core_spatial$layer_1$lsm_p_core)

mapbiomas_salinas_core_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas == 1, 1, NA), progress = TRUE, metric = "core")
mapbiomas_salinas_core_spatial
plot(mapbiomas_salinas_core_spatial$layer_1$lsm_p_core)

mapbiomas_salinas_carbon20_core_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas_carbon20 == 1, 1, NA), progress = TRUE, metric = "core")
mapbiomas_salinas_carbon20_core_spatial
plot(mapbiomas_salinas_carbon20_core_spatial$layer_1$lsm_p_core)

mapbiomas_salinas_carbon40_core_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas_carbon40 == 1, 1, NA), progress = TRUE, metric = "core")
mapbiomas_salinas_carbon40_core_spatial
plot(mapbiomas_salinas_carbon40_core_spatial$layer_1$lsm_p_core)

## shape ----
mapbiomas_tucano_shape_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano == 1, 1, NA), progress = TRUE, metric = "shape")
mapbiomas_tucano_shape_spatial
plot(mapbiomas_tucano_shape_spatial$layer_1$lsm_p_shape)

mapbiomas_tucano_carbon20_shape_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano_carbon20 == 1, 1, NA), progress = TRUE, metric = "shape")
mapbiomas_tucano_carbon20_shape_spatial
plot(mapbiomas_tucano_carbon20_shape_spatial$layer_1$lsm_p_shape)

mapbiomas_tucano_carbon40_shape_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano_carbon40 == 1, 1, NA), progress = TRUE, metric = "shape")
mapbiomas_tucano_carbon40_shape_spatial
plot(mapbiomas_tucano_carbon40_shape_spatial$layer_1$lsm_p_shape)

mapbiomas_itaiba_shape_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba == 1, 1, NA), progress = TRUE, metric = "shape")
mapbiomas_itaiba_shape_spatial
plot(mapbiomas_itaiba_shape_spatial$layer_1$lsm_p_shape)

mapbiomas_itaiba_carbon20_shape_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba_carbon20 == 1, 1, NA), progress = TRUE, metric = "shape")
mapbiomas_itaiba_carbon20_shape_spatial
plot(mapbiomas_itaiba_carbon20_shape_spatial$layer_1$lsm_p_shape)

mapbiomas_itaiba_carbon40_shape_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba_carbon40 == 1, 1, NA), progress = TRUE, metric = "shape")
mapbiomas_itaiba_carbon40_shape_spatial
plot(mapbiomas_itaiba_carbon40_shape_spatial$layer_1$lsm_p_shape)

mapbiomas_salinas_shape_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas == 1, 1, NA), progress = TRUE, metric = "shape")
mapbiomas_salinas_shape_spatial
plot(mapbiomas_salinas_shape_spatial$layer_1$lsm_p_shape)

mapbiomas_salinas_carbon20_shape_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas_carbon20 == 1, 1, NA), progress = TRUE, metric = "shape")
mapbiomas_salinas_carbon20_shape_spatial
plot(mapbiomas_salinas_carbon20_shape_spatial$layer_1$lsm_p_shape)

mapbiomas_salinas_carbon40_shape_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas_carbon40 == 1, 1, NA), progress = TRUE, metric = "shape")
mapbiomas_salinas_carbon40_shape_spatial
plot(mapbiomas_salinas_carbon40_shape_spatial$layer_1$lsm_p_shape)

## enn ----
mapbiomas_tucano_enn_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano == 1, 1, NA), progress = TRUE, metric = "enn")
mapbiomas_tucano_enn_spatial
plot(mapbiomas_tucano_enn_spatial$layer_1$lsm_p_enn)

mapbiomas_tucano_carbon20_enn_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano_carbon20 == 1, 1, NA), progress = TRUE, metric = "enn")
mapbiomas_tucano_carbon20_enn_spatial
plot(mapbiomas_tucano_carbon20_enn_spatial$layer_1$lsm_p_enn)

mapbiomas_tucano_carbon40_enn_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_tucano_carbon40 == 1, 1, NA), progress = TRUE, metric = "enn")
mapbiomas_tucano_carbon40_enn_spatial
plot(mapbiomas_tucano_carbon40_enn_spatial$layer_1$lsm_p_enn)

mapbiomas_itaiba_enn_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba == 1, 1, NA), progress = TRUE, metric = "enn")
mapbiomas_itaiba_enn_spatial
plot(mapbiomas_itaiba_enn_spatial$layer_1$lsm_p_enn)

mapbiomas_itaiba_carbon20_enn_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba_carbon20 == 1, 1, NA), progress = TRUE, metric = "enn")
mapbiomas_itaiba_carbon20_enn_spatial
plot(mapbiomas_itaiba_carbon20_enn_spatial$layer_1$lsm_p_enn)

mapbiomas_itaiba_carbon40_enn_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_itaiba_carbon40 == 1, 1, NA), progress = TRUE, metric = "enn")
mapbiomas_itaiba_carbon40_enn_spatial
plot(mapbiomas_itaiba_carbon40_enn_spatial$layer_1$lsm_p_enn)

mapbiomas_salinas_enn_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas == 1, 1, NA), progress = TRUE, metric = "enn")
mapbiomas_salinas_enn_spatial
plot(mapbiomas_salinas_enn_spatial$layer_1$lsm_p_enn)

mapbiomas_salinas_carbon20_enn_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas_carbon20 == 1, 1, NA), progress = TRUE, metric = "enn")
mapbiomas_salinas_carbon20_enn_spatial
plot(mapbiomas_salinas_carbon20_enn_spatial$layer_1$lsm_p_enn)

mapbiomas_salinas_carbon40_enn_spatial <- landscapemetrics::spatialize_lsm(
    landscape = terra::ifel(mapbiomas_salinas_carbon40 == 1, 1, NA), progress = TRUE, metric = "enn")
mapbiomas_salinas_carbon40_enn_spatial
plot(mapbiomas_salinas_carbon40_enn_spatial$layer_1$lsm_p_enn)

## export ----
terra::writeRaster(mapbiomas_tucano_area_spatial$layer_1$lsm_p_area, 
                   "02_results/mapbiomas_area_tucano_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_tucano_carbon20_area_spatial$layer_1$lsm_p_area, 
                   "02_results/mapbiomas_area_tucano_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_tucano_carbon40_area_spatial$layer_1$lsm_p_area, 
                   "02_results/mapbiomas_area_tucano_carbon40_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_area_spatial$layer_1$lsm_p_area, 
                   "02_results/mapbiomas_area_itaiba_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_carbon20_area_spatial$layer_1$lsm_p_area, 
                   "02_results/mapbiomas_area_itaiba_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_carbon40_area_spatial$layer_1$lsm_p_area, 
                   "02_results/mapbiomas_area_itaiba_carbon40_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_area_spatial$layer_1$lsm_p_area, 
                   "02_results/mapbiomas_area_salinas_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_carbon20_area_spatial$layer_1$lsm_p_area, 
                   "02_results/mapbiomas_area_salinas_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_carbon40_area_spatial$layer_1$lsm_p_area, 
                   "02_results/mapbiomas_area_salinas_carbon40_spatial.tif", overwrite = TRUE)


terra::writeRaster(mapbiomas_tucano_core_spatial$layer_1$lsm_p_core, 
                   "02_results/mapbiomas_core_tucano_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_tucano_carbon20_core_spatial$layer_1$lsm_p_core, 
                   "02_results/mapbiomas_core_tucano_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_tucano_carbon40_core_spatial$layer_1$lsm_p_core, 
                   "02_results/mapbiomas_core_tucano_carbon40_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_core_spatial$layer_1$lsm_p_core, 
                   "02_results/mapbiomas_core_itaiba_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_carbon20_core_spatial$layer_1$lsm_p_core, 
                   "02_results/mapbiomas_core_itaiba_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_carbon40_core_spatial$layer_1$lsm_p_core, 
                   "02_results/mapbiomas_core_itaiba_carbon40_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_core_spatial$layer_1$lsm_p_core, 
                   "02_results/mapbiomas_core_salinas_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_carbon20_core_spatial$layer_1$lsm_p_core, 
                   "02_results/mapbiomas_core_salinas_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_carbon40_core_spatial$layer_1$lsm_p_core, 
                   "02_results/mapbiomas_core_salinas_carbon40_spatial.tif", overwrite = TRUE)


terra::writeRaster(mapbiomas_tucano_shape_spatial$layer_1$lsm_p_shape, 
                   "02_results/mapbiomas_shape_tucano_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_tucano_carbon20_shape_spatial$layer_1$lsm_p_shape, 
                   "02_results/mapbiomas_shape_tucano_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_tucano_carbon40_shape_spatial$layer_1$lsm_p_shape, 
                   "02_results/mapbiomas_shape_tucano_carbon40_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_shape_spatial$layer_1$lsm_p_shape, 
                   "02_results/mapbiomas_shape_itaiba_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_carbon20_shape_spatial$layer_1$lsm_p_shape, 
                   "02_results/mapbiomas_shape_itaiba_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_shape_spatial$layer_1$lsm_p_shape, 
                   "02_results/mapbiomas_shape_salinas_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_carbon20_shape_spatial$layer_1$lsm_p_shape, 
                   "02_results/mapbiomas_shape_salinas_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_carbon40_shape_spatial$layer_1$lsm_p_shape, 
                   "02_results/mapbiomas_shape_itaiba_carbon40_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_carbon40_shape_spatial$layer_1$lsm_p_shape, 
                   "02_results/mapbiomas_shape_salinas_carbon40_spatial.tif", overwrite = TRUE)


terra::writeRaster(mapbiomas_tucano_enn_spatial$layer_1$lsm_p_enn, 
                   "02_results/mapbiomas_enn_tucano_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_tucano_carbon20_enn_spatial$layer_1$lsm_p_enn, 
                   "02_results/mapbiomas_enn_tucano_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_tucano_carbon40_enn_spatial$layer_1$lsm_p_enn, 
                   "02_results/mapbiomas_enn_tucano_carbon40_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_enn_spatial$layer_1$lsm_p_enn, 
                   "02_results/mapbiomas_enn_itaiba_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_carbon20_enn_spatial$layer_1$lsm_p_enn, 
                   "02_results/mapbiomas_enn_itaiba_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_itaiba_carbon40_enn_spatial$layer_1$lsm_p_enn, 
                   "02_results/mapbiomas_enn_itaiba_carbon40_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_enn_spatial$layer_1$lsm_p_enn, 
                   "02_results/mapbiomas_enn_salinas_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_carbon20_enn_spatial$layer_1$lsm_p_enn, 
                   "02_results/mapbiomas_enn_salinas_carbon20_spatial.tif", overwrite = TRUE)
terra::writeRaster(mapbiomas_salinas_carbon40_enn_spatial$layer_1$lsm_p_enn, 
                   "02_results/mapbiomas_enn_salinas_carbon40_spatial.tif", overwrite = TRUE)

## maps ----
list_metrics <- dir(path = "02_results/", pattern = ".tif", full.names = TRUE)
list_metrics

for(i in list_metrics){
    
    r <- terra::rast(i)
    
    name <- stringr::str_replace(stringr::str_split(basename(i), "_", simplify = TRUE)[, 2], ".tif", "")
    
    map <- tm_shape(r) +
        tm_raster(col = "value",
                  col.scale = tm_scale_continuous(values = "viridis"),
                  col.legend = tm_legend(title = name, 
                                         position = tm_pos_in("left", "bottom"),
                                         reverse = TRUE))
    tmap::tmap_save(map, stringr::str_replace(paste0(stringr::str_replace(i, ".tif", ".png")), "02_results/", "02_results/map_"))
    
}

# end ---------------------------------------------------------------------