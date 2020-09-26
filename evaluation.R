setwd("/home/dwolffram/covid19-ensembles")

source("functions.R")
source("ensemble_methods.R")
source("evaluation_functions.R")


# all_models <- list.dirs(path = "data-processed/", full.names = FALSE, recursive = FALSE)

models <- c("LANL-GrowthRate", "CovidAnalytics-DELPHI", "MOBS-GLEAM_COVID", 
            "YYG-ParamSearch", "UCLA-SuEIR", "COVIDhub-baseline")

exclude_locations <- c("11", "66", "69", "72", "78")


df <- load_df(models=models, exclude_locations=exclude_locations)


# not all targets are always available
available_targets <- df %>%
  group_by(target_end_date, target) %>%
  summarize(model_count = length(unique(model)))

# dates where all models are available for "1 wk ahead cum death"
possible_dates <- df %>%
  group_by(target_end_date, target) %>%
  summarize(model_count = length(unique(model))) %>%
  filter(target == "1 wk ahead cum death") %>%
  filter(model_count == length(models)) %>%
  pull(target_end_date)


# only consider 1 week ahead forecasts and only the dates with all models available
df <- df %>% 
  filter(target == "1 wk ahead cum death") %>%
  filter(target_end_date %in% possible_dates)

library(doParallel)
no_cores <- detectCores() - 1  
no_cores <- 32
registerDoParallel(cores=no_cores)  

ensembles <- c("EWA", "MED", "V2", "V3", "V4", "QRA2", "QRA3", 
               "QRA4", "GQRA2", "GQRA3", "GQRA4")
window_sizes <- 1:4

results <- evaluate_ensembles(df, possible_dates, window_sizes, ensembles)

file_name <- paste0("results/results_", Sys.Date(), ".csv")
write.csv(results, file_name, row.names=FALSE)

# write.csv(results, "results/results_2020-08-09.csv", row.names=FALSE)


### EXCLUDE US FROM TRAINING

ensembles <- c("EWA", "MED", "V2", "V3", "V4", "QRA2", "QRA3", 
               "QRA4", "GQRA2", "GQRA3", "GQRA4")
window_sizes <- 1:4

#dates <- as.Date(c("2020-07-11", "2020-07-18", "2020-07-25", "2020-08-01", "2020-08-08"))

results <- evaluate_ensembles(df, possible_dates, window_sizes, ensembles, exclude_us_from_training=TRUE)

file_name <- paste0("results/results_", Sys.Date(), "_train_without_us.csv")
file_name <- paste0("results/results_2020-08-08_train_without_us.csv")

write.csv(results, file_name, row.names=FALSE)
