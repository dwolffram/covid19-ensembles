setwd("/home/wolffram/covid19-ensembles")

source("data_loading.R")
source("scoring.R")
source("ensemble_methods.R")
source("ensemble_functions.R")

library(doParallel)


exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")


# 4 wk cum death
models <- c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", "Karlen-pypm", "LANL-GrowthRate", "LNQ-ens1",
                          "MOBS-GLEAM_COVID", "OliverWyman-Navigator", "PSI-DRAFT", "UA-EpiCovDA", "UMass-MechBayes") 
# < 2021-04-03
df <- load_forecasts(models=models, targets=c("4 wk ahead cum death"),
                     exclude_locations=exclude_locations, start_date="2020-09-05", end_date='2021-03-27')


# 1 wk inc death
models <- c("CEID-Walk", "CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", "Karlen-pypm", "LANL-GrowthRate", 
  "UA-EpiCovDA", "UCSD_NEU-DeepGLEAM", "UMass-MechBayes")
# < 2021-04-03
# c("CU-nochange", "CU-scenario_high", "CU-scenario_low", "CU-scenario_mid") singular design matrix

df <- load_forecasts(models=models, targets=c("1 wk ahead inc death"),
                     exclude_locations=exclude_locations, start_date="2020-10-24", end_date='2021-03-27')

# 4 wk inc death
models <- c("CovidAnalytics-DELPHI", "COVIDhub-baseline",
            "CU-select", "Karlen-pypm", "LANL-GrowthRate", "UA-EpiCovDA", 
            "UCSD_NEU-DeepGLEAM", "UMass-MechBayes")

df <- load_forecasts(models=models, targets=c("4 wk ahead inc death"),
                     exclude_locations=exclude_locations, start_date="2020-10-24", end_date='2021-03-27')

# 1 wk in case
models <- c("BPagano-RtDriven", "CEID-Walk", "CovidAnalytics-DELPHI", "COVIDhub-baseline", 
            "CU-select", "JHUAPL-Bucky", "Karlen-pypm", 
            "LANL-GrowthRate", "LNQ-ens1", "UMass-MechBayes")

df <- load_forecasts(models=models, targets=c("1 wk ahead inc case"),
                     exclude_locations=exclude_locations, start_date="2020-10-24", end_date='2021-03-27')

df <- df %>%
  filter(nchar(location) == 2)

unique(df$model)
length(unique(df$target_end_date))

unique(df$quantile)

b <- df %>%
  group_by(target, model, target_end_date, location) %>%
  mutate(n_quantiles = n()) %>%
  group_by(target, model) %>%
  mutate(n_quantiles = min(n_quantiles)) %>%
  filter(n_quantiles == 23 | (str_detect(target, 'inc case') & n_quantiles == 7)) %>%
  select(-n_quantiles)

b %>%
  group_by(model, target_end_date, location) %>%
  summarize(n_quantiles = n()) %>%
  group_by(model) %>%
  summarize(n_quantiles = min(n_quantiles))

a <- subset(df, target_end_date == '2021-03-27')
a %>%
  group_by(model, target_end_date) %>%
  summarize(n_distinct(forecast_date))

dfs <- train_test_split(df, as.Date('2021-03-27'), 1, 4)

df_train <- dfs$df_train
df_train <- subset(df_train, location != 'US')

df_test <- dfs$df_test
p <- qra3_fit(df_train)

dput(unique(df_train$model))
df_train <- df_train %>%
  filter(!(model %in% c("CU-nochange", "CU-scenario_high", "CU-scenario_low", 
                        "CU-scenario_mid")))

quantile_levels <- sort(unique(df_train$quantile))
df_params <- data.frame()
for (quantile_level in quantile_levels){
  print(quantile_level)
  df_temp <- subset(df_train, quantile==quantile_level) %>% 
    select(c("target_end_date", "location", "truth", "model", "value")) %>% 
    spread(model, value) %>% 
    drop_na()
  
  params <- rq(truth ~ . - target_end_date - location - truth -1, 
               tau = quantile_level, data = df_temp)$coefficients
  
  params <- data.frame(model=str_sub(names(params), 2, -2), quantile=quantile_level, 
                       param=params, row.names = NULL)
  df_params <- bind_rows(df_params, params)
}

x <- df_temp %>%
  select(- c(target_end_date, location, truth))

df_forecasts <- build_ensembles(df_train, df_test, 'QRA3')

# no_cores <- detectCores() - 1  
no_cores <- 32
registerDoParallel(cores=no_cores)  
registerDoSEQ()

ensembles <- c("EWA", "MED", "INV", "INVA", "V2", "V3", "V4", "QRA2", "QRA3", 
               "QRA4", "GQRA2", "GQRA3", "GQRA4", "QNA3")
window_sizes <- 4

ensembles <- c("EWA", "MED", "INV", "INVA", "V3", "QRA3", "GQRA3", "V3_iter")
window_sizes <- 4

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles=ensembles, 
                                   exclude_us_from_training=TRUE)


file_name <- paste0("data/ensemble_forecasts/evaluation_study/4wk_inc_death/df_ensembles_4wk_inc_death.csv")

file_name <- paste0("data/ensemble_forecasts/evaluation_study/1wk_inc_case/df_ensembles_1wk_inc_case.csv")

write.csv(df_ensembles, file_name, row.names=FALSE)


# Ensembles on Subset

df_ensembles <- ensemble_forecasts(df, window_sizes=4, ensembles=ensembles,
                                   exclude_us_from_training=TRUE, n_models = 3)

file_name <- paste0("data/ensemble_forecasts/evaluation_study/4wk_cum_death/df_ensembles_4wk_cum_death_top3.csv")
file_name <- paste0("data/ensemble_forecasts/evaluation_study/1wk_inc_death/df_ensembles_1wk_inc_death_top3_ws4.csv")
file_name <- paste0("data/ensemble_forecasts/evaluation_study/4wk_inc_death/df_ensembles_4wk_inc_death_top3_ws4.csv")


write.csv(df_ensembles, file_name, row.names=FALSE)

df1 <- subset(df_ensembles, window_size==4)
file_name <- paste0("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_top3_ws4.csv")
write.csv(df1, file_name, row.names=FALSE)


# Iterative training V3

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles="V3_iter", exclude_us_from_training=TRUE)

file_name <- paste0("data/ensemble_forecasts/evaluation_study/4wk_inc_death/df_ensembles_4wk_inc_death_v3-iter_refit.csv")
write.csv(df_ensembles, file_name, row.names=FALSE)


### Evaluate V3_iter

build_ensembles <- function(df_train, df_test, ensembles, n_models){
  df_ensembles <- data.frame()
  
  for (c in 1:10){
    print(c)
    p <- v3_iter_fit(df_train, c)
    df_forecast <- V3(df_test, params=p$params, models=p$models)
    df_forecast <- sort_quantiles(df_forecast)
    df_forecast$model <- paste0("V3_iter-", c)
    df_ensembles <- bind_rows(df_ensembles, df_forecast)
  }
  
  return(df_ensembles)
}

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles="V3_iter", exclude_us_from_training=TRUE)

file_name <- paste0("data/ensemble_forecasts/evaluation_study/1wk_inc_death/df_ensembles_1wk_inc_death_v3-iter.csv")
write.csv(df_ensembles, file_name, row.names=FALSE)

