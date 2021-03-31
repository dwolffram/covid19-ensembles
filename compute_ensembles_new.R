setwd("/home/wolffram/covid19-ensembles")

source("data_loading.R")
source("scoring.R")
source("ensemble_methods.R")
source("ensemble_functions.R")

library(doParallel)




# 4 wk cum death
models <- c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", "Karlen-pypm", "LANL-GrowthRate", "LNQ-ens1",
                          "MOBS-GLEAM_COVID", "OliverWyman-Navigator", "PSI-DRAFT", "UA-EpiCovDA", "UMass-MechBayes") 
# < 2021-04-03

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")

df <- load_forecasts(models=models, targets=c("4 wk ahead cum death"),
                     exclude_locations=exclude_locations, start_date="2020-09-05", end_date='2021-03-27')

unique(df$model)
length(unique(df$target_end_date))

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

# no_cores <- detectCores() - 1  
no_cores <- 32
registerDoParallel(cores=no_cores)  

ensembles <- c("EWA", "MED", "INV", "INVA", "V2", "V3", "V4", "QRA2", "QRA3", 
               "QRA4", "GQRA2", "GQRA3", "GQRA4", "QNA3")
window_sizes <- 1:4


df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles=ensembles, 
                                   exclude_us_from_training=TRUE)


file_name <- paste0("data/ensemble_forecasts/evaluation_study/4wk_cum_death/df_ensembles_4wk_cum_death.csv")
write.csv(df_ensembles, file_name, row.names=FALSE)


# Ensembles on Subset

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles=ensembles,
                                   exclude_us_from_training=TRUE, n_models = 3)

file_name <- paste0("data/ensemble_forecasts/evaluation_study/4wk_cum_death/df_ensembles_4wk_cum_death_top3.csv")
write.csv(df_ensembles, file_name, row.names=FALSE)

df1 <- subset(df_ensembles, window_size==4)
file_name <- paste0("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_top3_ws4.csv")
write.csv(df1, file_name, row.names=FALSE)


# Iterative training V3

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles="V3_iter", exclude_us_from_training=TRUE)

file_name <- paste0("data/ensemble_forecasts/evaluation_study/4wk_cum_death/df_ensembles_4wk_cum_death_v3-iter_refit.csv")
write.csv(df_ensembles, file_name, row.names=FALSE)

