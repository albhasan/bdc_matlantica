library(dplyr)
library(ensurer)
library(ggplot2)
library(purrr)
library(sf)
library(sits)
library(stringr)
library(tidyr)

source("./scripts/00_util.R")

samples_file <- "./data/samples/samples_2018.rds"
bdc_grid_shp <- "./data/bdc_grid/BDC_MD.shp"
stopifnot(file.exists(samples_file))
stopifnot(file.exists(bdc_grid_shp))

bdc_grid <- bdc_grid_shp %>%
    sf::read_sf() %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::select(id)

sits_samples_tb <- samples_file %>%
    readRDS() %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
                 crs = 4326,
                 remove = FALSE) %>%
    sf::st_join(bdc_grid) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::filter(id %in% c("042055", 
                            "043054", 
                            "043055")) %>%
    dplyr::select(-id) %>%
    (function(x){
        x %>%
            dplyr::group_by(label) %>%
            dplyr::summarise(Samples = n()) %>%
            print(n = Inf)
        return(x)
    }) %>%
    mutate(cube = "matlantica") %>%
    ensurer::ensure_that(nrow(.) > 0,
                         err_des = "No samples match!")

class(sits_samples_tb) <- class(cerrado_2classes)

#####################
# NOTE: Remove a point with time series of NAs.
count_na <- function(x){
    return(sum(is.na(x)))
}
sits_samples_tb <- sits_samples_tb %>%
    dplyr::mutate(n_na = purrr::map(time_series, count_na)) %>% 
    dplyr::filter(n_na == 0) %>%
    dplyr::select(-n_na)
rm(count_na)
# sits_samples_tb <- sits_samples_tb %>%
#     dplyr::bind_rows(point_ts)
# sits_samples_tb %>%
#     saveRDS(file = "./data/samples/samples_2018.rds")
#####################

sits_samples_tb %>%
    is_sits_valid() %>%
    dplyr::count(label)
#    label                            n
#  1 AQUICULTURA                      1
#  2 CANA                            27
#  3 FLORESTA PLANTADA              317
#  4 FORMAÇÃO CAMPESTRE               6
#  5 FORMAÇÃO FLORESTAL             842
#  6 FORMAÇÃO SAVÃNICA                9
#  7 INFRAESTRUTURA URBANA           42
#  8 LAVOURA PERENE                   6
#  9 LAVOURA TEMPORÁRIA             466
# 10 MANGUE                           4
# 11 MINERAÇÃO                        2
# 12 NÃO OBSERVADO                    2
# 13 OUTRA ÁREA NÃO VEGETADA          9
# 14 OUTRA FORMAÇÃO NÃO FLORESTAL     9
# 15 PASTAGEM                       643
# 16 RIO, LAGO E OCEANO              36

samples_tb <- sits_samples_tb %>%
    tidyr::unnest(time_series) %>%
    dplyr::select(-longitude, -latitude, -cube,
                  Label = label) %>%
    dplyr::select(order(colnames(.))) %>%
    dplyr::select(Label, start_date, end_date, Index, everything())

my_date <- samples_tb %>%
    dplyr::select(start_date, end_date) %>%
    unlist() %>%
    lubridate::as_date() %>%
    range() %>%
    paste(collapse = "_")



#---- Plot samples' time series ----





# library("leaflet")
# sits_samples_tb %>%
#     dplyr::select(label, longitude, latitude) %>%
#     leaflet::leaflet() %>%
#     leaflet::addProviderTiles(providers$Esri.WorldImagery, 
#                               group = "World Imagery") %>%
#     addCircleMarkers(label = sits_samples_tb$label, 
#                popup = sits_samples_tb$label)



plot_tb <- samples_tb %>%
    tidyr::pivot_longer(cols = !tidyselect::all_of(c("start_date",
                                                     "end_date",
                                                     "Label", "Index")),
                        names_to = "Band",
                        values_to = "Value")

plot_tb %>%
    dplyr::filter(Band %in% c("B1", "B2", "B3", "B4")) %>%
    f_plot() +
    ggplot2::ggtitle("L8 samples - Flat bands") +
    ggplot2::ggsave(filename = paste0("./data/samples/plot_samples_bands_flat_",
                                      my_date, ".png"),
                    width = 297,
                    height = 420,
                    units = "mm")

plot_tb %>%
    dplyr::filter(Band %in% c("B5", "B6", "B7")) %>%
    f_plot() +
    ggplot2::ggtitle("L8 samples - Bands") +
    ggplot2::ggsave(filename = paste0("./data/samples/plot_samples_bands_",
                                      my_date, ".png"),
                    width = 297,
                    height = 420,
                    units = "mm")

plot_tb %>%
    dplyr::filter(Band %in% c("EVI", "NDVI")) %>%
    f_plot() +
    ggplot2::ggtitle("L8 samples - Vegetation Indexes") +
    ggplot2::ggsave(filename = paste0("./data/samples/plot_samples_indices_",
                                      my_date, ".png"),
                    width = 297,
                    height = 420,
                    units = "mm")

sits_samples_tb %>%
    saveRDS(file = "./data/samples/samples.rds")
