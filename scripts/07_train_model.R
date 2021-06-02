library(dplyr)
library(ensurer)
library(lubridate)
library(purrr)
library(sits)



#--- Configuration ----

samples_file <- "/home/alber-d006/Documents/bdc_matlantica/data/samples/samples.rds"
model_file   <- "/home/alber-d006/Documents/bdc_matlantica/results/first_classification/ml_model.rds"
stopifnot(file.exists(samples_file))
stopifnot(dir.exists(dirname(model_file)))

ml_method <- sits::sits_rfor(trees = 2000)

source("./scripts/00_util.R")



#---- Script ----

samples_tb <- samples_file %>%
  readRDS() %>%
  # NOTE: Filter SOM samples:
  #dplyr::filter(eval == "clean") %>%
  #dplyr::select(-id, -old_label, -id_sample, -id_neuron, -eval, -post_prob) %>%
  # NOTE: Filter original samples:
  dplyr::filter(label %in% c("CANA", "FLORESTA PLANTADA", "FORMAÇÃO FLORESTAL", 
                             "LAVOURA PERENE", "LAVOURA TEMPORÁRIA", 
                             "PASTAGEM")) %>% 
  dplyr::mutate(label = dplyr::recode(label,
                                    "CANA"               = "Agricultura", 
                                    "FLORESTA PLANTADA"  = "Floresta plantada", 
                                    "FORMAÇÃO FLORESTAL" = "Formação florestal", 
                                    "LAVOURA PERENE"     = "Agricultura", 
                                    "LAVOURA TEMPORÁRIA" = "Agricultura", 
                                    "PASTAGEM"           = "Pastagem"))

samples_tb %>%
  is_sits_valid() %>%
  dplyr::count(label)
# ORIGINAL
#   label                  n
# 1 CANA                  27
# 2 FLORESTA PLANTADA    317
# 3 FORMAÇÃO FLORESTAL   842
# 4 LAVOURA PERENE         6
# 5 LAVOURA TEMPORÁRIA   466
# 6 PASTAGEM             643
# 
# RECODED
# label                  n
# 1 Agricultura          499
# 2 Floresta plantada    317
# 3 Formação florestal   842
# 4 Pastagem             643

ml_model <- sits::sits_train(samples_tb,
                             ml_method = ml_method)

saveRDS(object = ml_model,
        file = model_file)
