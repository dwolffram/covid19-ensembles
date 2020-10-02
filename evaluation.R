setwd("/home/dwolffram/covid19-ensembles")

source("functions.R")
source("ensemble_methods.R")
source("evaluation_functions.R")


# all_models <- list.dirs(path = "data-processed/", full.names = FALSE, recursive = FALSE)

models <- c("LANL-GrowthRate", "CovidAnalytics-DELPHI", "MOBS-GLEAM_COVID", 
            "YYG-ParamSearch", "UCLA-SuEIR", "COVIDhub-baseline")

exclude_locations <- c("11", "60", "66", "69", "72", "78")


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

# a <- df %>%
#   group_by(target_end_date, location) %>%
#   summarize(model_count = length(unique(model))) %>%
#   subset(model_count < 6)
# 
# unique(a$location)

# only consider 1 week ahead forecasts and only the dates with all models available
df <- df %>% 
  filter(target == "1 wk ahead cum death") %>%
  filter(target_end_date %in% possible_dates)


# window_sizes <- 1:4
# dates <- possible_dates
# for (window_size in window_sizes){
#   print(paste0("Compute scores for window size ", window_size, "."))
#   
#   # possible test dates for given window size
#   if (window_size >= length(dates)){
#     print("Not enough dates for given window size.")
#     next
#   }
#   
#   #test_dates <- as.list(dates[(window_size+1):length(dates)])
#   test_dates <- as.list(dates[(max(window_sizes)+1):length(dates)])
# 
#   for (test_date in test_dates){
#     print(as.character(test_date))
#     
#     dfs <- train_test_split(df, test_date, window_size)
#     print(nrow(dfs$df_train))
#     print(nrow(dfs$df_test))
#   }
# }
# 
# EWA_df <- EWA(dfs$df_test )
# EWA_sort <- sort_quantiles(EWA_df)
# 
# row_index <- names(EWA_df)[!(names(EWA_df) %in% c("quantile", "value"))]
# df_wide <- reshape(EWA_df, direction = "wide", timevar = "quantile",
#                    v.names = "value", idvar = row_index)
# 
# class(EWA_sort)
# class(EWA_df)
# 
# wis_table(EWA_sort)
# wis_table(as.data.frame(EWA_sort))


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

results2 <- evaluate_ensembles(df, possible_dates, window_sizes, ensembles, exclude_us_from_training=TRUE)

file_name <- paste0("results/results_", Sys.Date(), "_train_without_us.csv")
#file_name <- paste0("results/results_2020-08-08_train_without_us.csv")

write.csv(results, file_name, row.names=FALSE)
