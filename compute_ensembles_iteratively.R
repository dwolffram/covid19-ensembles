setwd("/home/wolffram/covid19-ensembles")

source("data_loading.R")
source("scoring.R")
source("ensemble_methods.R")
source("ensemble_functions.R")

library(doParallel)

models <- c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", "DDS-NBDS",             
            "JHU_IDD-CovidSP", "Karlen-pypm", "LANL-GrowthRate", "MOBS-GLEAM_COVID",
            "OliverWyman-Navigator", "PSI-DRAFT", "UA-EpiCovDA", "UCLA-SuEIR",           
            "UMass-MechBayes")

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")

df <- load_forecasts(models=models, targets=c("1 wk ahead cum death"),
                     exclude_locations=exclude_locations, start_date="2020-08-01", end_date="2021-02-06", 
                     intersect_dates=TRUE)

write.csv(df, "data/individual_models/df_evaluation_study.csv", row.names=FALSE)


length(unique(df$target_end_date))

no_cores <- 32
registerDoParallel(cores=no_cores)  

ensembles <- c("EWA", "MED", "INV", "INVA", "V2", "V3", "V4", "QRA2", "QRA3", 
               "QRA4", "GQRA2", "GQRA3", "GQRA4", "QNA3")

window_sizes <- 4

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles=ensembles, 
                                   exclude_us_from_training=TRUE, in_sample=TRUE, sort_crossings=FALSE)

file_name <- paste0("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_ws4_inSample_unsorted_order", Sys.Date(), ".csv")
write.csv(df_ensembles, file_name, row.names=FALSE)


dfs <- train_test_split(df, test_date=as.Date("2020-08-22"), horizon=1, window_size=4)
df_train <- dfs$df_train
df_test <- dfs$df_test


### Iterative training

dfs <- train_test_split(df, test_date=as.Date("2020-09-12"), horizon=1, window_size=4)
df_train <- dfs$df_train
df_test <- dfs$df_test

df_train <- subset(df_train, location != 'US')
df_test <- subset(df_test, location != 'US')


v3-iter_fit <- function(df_train){
  
}
scores <- wis(df_train)

scores %>%
  group_by(model) %>%
  summarize(meanWIS = mean(wis))

a <- df_train %>%
  select(-c(location_name, type, forecast_date))
s <- wis(a)

models_ranked <- scores %>%
  group_by(model) %>%
  summarize(meanWIS = mean(wis)) %>%
  arrange(meanWIS) %>%
  pull(model)


df_iter <- subset(df_train, model %in% models_ranked[1:2])

p <- v3_fit(df_iter)
df_iter <- V3(df_iter, params=p, models=models_ranked[1:2])
df_iter$model <- 'F'
weights <- p

for (m in models_ranked[-1:-2]){
  print(m)
  df_iter <- bind_rows(df_iter, subset(df_train, model == m))
  print(unique(df_iter$model))
  p <- v3_fit(df_iter)
  print(p)
  df_iter <- V3(df_iter, params=p, models=c("F", m))
  df_iter$model <- 'F'
  
  weights <- c(p[1]*weights, p[2])
}


p3 <- v3_fit(df_train)
df_v3 <- V3(df_test, params=p3)
mean_wis(df_v3)

df_v3 <- V3(df_test, params=weights, models=models_ranked)
mean_wis(df_v3)

df_v3 <- V3(df_train, params=p3)
mean_wis(df_v3)

df_v3 <- V3(df_train, params=weights, models=models_ranked)
mean_wis(df_v3)

a <- data.frame(model=models_ranked, param=weights)
