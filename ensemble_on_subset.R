setwd("/home/wolffram/covid19-ensembles")

source("data_loading.R")
source("ensemble_methods.R")
source("ensemble_functions.R")
source("scoring.R")

library(quantreg)

models <- c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", "JHU_IDD-CovidSP",
            "LANL-GrowthRate", "MOBS-GLEAM_COVID", "PSI-DRAFT", "UCLA-SuEIR", 
            "UMass-MechBayes", "YYG-ParamSearch")

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")

df <- load_forecasts(models=models, targets=c("1 wk ahead cum death"),
                     exclude_locations=exclude_locations, start_date="2020-05-23",
                     intersect_dates=TRUE)

dfs <- train_test_split(df, test_date=as.Date("2020-08-22"), horizon=1, window_size=4)
df_train <- dfs$df_train
df_test <- dfs$df_test

w <- wis(df_train)
top_n_models <- w %>% 
  group_by(model) %>%
  summarize(wis = mean(wis)) %>%
  slice_min(wis, n=3) %>%
  pull(model)

d <- df_train %>%
  filter(model %in% top_n_models)

get_top_n_models <- function(df, n=3){
  top_n_models <- wis(df) %>% 
    group_by(model) %>%
    summarize(wis = mean(wis)) %>%
    slice_min(wis, n=n) %>%
    pull(model)
  
  return(top_n_models)
}

top_n_models <- get_top_n_models(df_train)

df_train <- df_train %>%
  filter(model %in% top_n_models)

df_test <- df_test %>%
  filter(model %in% top_n_models)


r <- build_ensembles(df_train, df_test, ensembles=c("QRA2"), 3)
s <- build_ensembles(df_train, df_test, ensembles=c("QRA2"), 5)

mean_wis(r)
mean_wis(s)


no_cores <- 32
registerDoParallel(cores=no_cores)  

ensembles <- c("EWA", "MED", "INV", "V2", "V3")

#, "V4", "QRA2", "QRA3", 
 #              "QRA4", "GQRA2", "GQRA3", "GQRA4")
window_sizes <- 4

# dates <- as.Date(c("2020-07-25", "2020-08-01", "2020-08-08", "2020-08-15", "2020-08-22"))

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles=ensembles, n_models=3)
