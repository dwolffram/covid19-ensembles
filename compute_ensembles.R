setwd("/home/wolffram/covid19-ensembles")

source("data_loading.R")
source("scoring.R")
source("ensemble_methods.R")
source("ensemble_functions.R")

library(doParallel)


models <- c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", "JHU_IDD-CovidSP",
            "LANL-GrowthRate", "MOBS-GLEAM_COVID", "PSI-DRAFT", "UCLA-SuEIR", 
            "UMass-MechBayes", "YYG-ParamSearch")

# DC,11,District of Columbia
# AS,60,American Samoa
# GU,66,Guam
# MP,69,Northern Mariana Islands
# PR,72,Puerto Rico
# UM,74,U.S. Minor Outlying Islands
# VI,78,Virgin Islands

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")

df <- load_forecasts(models=models, targets=c("1 wk ahead cum death"),
                     exclude_locations=exclude_locations, start_date="2020-05-23",
                     intersect_dates=TRUE)

length(unique(df$target_end_date))

# no_cores <- detectCores() - 1  
no_cores <- 32
registerDoParallel(cores=no_cores)  

ensembles <- c("EWA", "MED", "V2", "V3", "V4", "QRA2", "QRA3", 
               "QRA4", "GQRA2", "GQRA3", "GQRA4")
window_sizes <- 1:4

# dates <- as.Date(c("2020-07-25", "2020-08-01", "2020-08-08", "2020-08-15", "2020-08-22"))

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles=ensembles)

file_name <- paste0("data/ensemble_forecasts/df_ensembles_1wk_", Sys.Date(), ".csv")
write.csv(df_ensembles, file_name, row.names=FALSE)


### EXCLUDE US FROM TRAINING

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles=ensembles, 
                                   exclude_us_from_training=TRUE)

file_name <- paste0("data/ensemble_forecasts/df_ensembles_4wk_noUS_", Sys.Date(), ".csv")
write.csv(df_ensembles, file_name, row.names=FALSE)

## ADD INV ENSEMBLE

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles=c('INV'))
file_name <- paste0("data/ensemble_forecasts/df_ensemble_INV_4wk_", Sys.Date(), ".csv")
write.csv(df_ensembles, file_name, row.names=FALSE)

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles=c('INV'),
                                   exclude_us_from_training=TRUE)
file_name <- paste0("data/ensemble_forecasts/df_ensemble_INV_4wk_noUS_", Sys.Date(), ".csv")
write.csv(df_ensembles, file_name, row.names=FALSE)


