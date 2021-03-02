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

models <- c("Karlen-pypm",
            "OliverWyman-Navigator",     
            "UMass-MechBayes")

models <- c("Karlen-pypm",
            "COVIDhub-baseline",     
            "UMass-MechBayes")

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")

df <- load_forecasts(models=models, targets=c("1 wk ahead cum death"),
                     exclude_locations=exclude_locations, start_date="2020-08-01", end_date="2021-02-06", 
                     intersect_dates=TRUE)

length(unique(df$target_end_date))

no_cores <- 32
registerDoParallel(cores=no_cores)  

ensembles <- c("EWA", "MED", "INV", "INVA", "V2", "V3", "V4", "QRA2", "QRA3", 
               "QRA4", "GQRA2", "GQRA3", "GQRA4")
window_sizes <- 4

df_ensembles <- ensemble_forecasts(df, window_sizes=window_sizes, ensembles=ensembles, 
                                   exclude_us_from_training=TRUE)

file_name <- paste0("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_all_ws4_", Sys.Date(), ".csv")
write.csv(df_ensembles, file_name, row.names=FALSE)


ensemble_forecasts <- function(df, dates, window_sizes, ensembles=c("EWA"), 
                               exclude_us_from_training=FALSE, n_models='all', in_sample=FALSE){
  
  horizon <- as.numeric(substr(unique(df$target), 1, 1)) # first character of target is the horizon
  
  if(missing(dates)){
    dates <- unique(df$target_end_date)
  }
  
  df_ensembles <- data.frame()
  df_in_sample <- data.frame()
  
  for (window_size in window_sizes){
    print(paste0("Compute scores for window size ", window_size, "."))
    
    # possible test dates for given window size
    if (window_size + horizon > length(dates)){
      print("Not enough dates for given window size and horizon.")
      next
    }
    
    test_dates <- as.list(dates[(max(window_sizes) + horizon):length(dates)])
    
    all_forecasts <- foreach(test_date=test_dates) %dopar% {
      print(as.character(test_date))
      tryCatch(
        expr = {
          dfs <- train_test_split(df, test_date, horizon, window_size)
          
          df_train <- dfs$df_train
          if (exclude_us_from_training){
            df_train <- subset(df_train, location != 'US')
          }
          
          df_test <- dfs$df_test
          
          results <- build_ensembles(df_train, df_test, ensembles, n_models, in_sample)
          df_forecasts <- results[[1]]
          df_fit <- results[[2]]
          df_forecasts$window_size <- window_size
          df_fit$window_size <- window_size
          list(df_forecasts, df_fit)
        },
        error = function(e){
          message(test_date)
          message(e)
          df_forecasts <- NULL
          df_fit <- NULL
          list(df_forecasts, df_fit)
        },
        warning = function(w){
          message(test_date)
          message(w)
          df_forecasts <- NULL
          df_fit <- NULL
          list(df_forecasts, df_fit)
        }
        
      )
    }
    
    all_forecasts <- comb(all_forecasts)
    df_ensembles <- bind_rows(df_ensembles, all_forecasts[[1]])
    df_in_sample <- bind_rows(df_in_sample, all_forecasts[[2]])
  }
  
  return(list(df_ensembles, df_in_sample))
}


build_ensembles <- function(df_train, df_test, 
                            ensembles=c("EWA", "MED", "INV", "V2", "V3", "V4", 
                                        "QRA2", "QRA3", "QRA4", 
                                        "GQRA2", "GQRA3", "GQRA4"),
                            n_models='all', in_sample=FALSE){
  
  if(n_models != 'all'){
    # only use best n_models models for ensembles
    top_n_models <- get_top_n_models(df_train, n_models)
    
    df_train <- df_train %>%
      filter(model %in% top_n_models)
    
    df_test <- df_test %>%
      filter(model %in% top_n_models)
  }
  
  df_ensembles <- data.frame()
  df_in_sample <- data.frame()
  
  for (ensemble in ensembles){
    print(ensemble)
    switch (ensemble,
            "EWA" = {
              df_forecast <- EWA(df_test)
              if (in_sample) df_fit <- EWA(df_train)
            },
            "MED" = {
              df_forecast <- MED(df_test)
              if (in_sample) df_fit <- MED(df_train)
            },
            "INV" = {
              p <- inv_fit(df_train)
              df_forecast <- INV(df_test, params=p)
              if (in_sample) df_fit <- INV(df_train, params=p)
            },
            "INVA" = {
              p <- inva_fit(df_train)
              df_forecast <- INVA(df_test, params=p)
              if (in_sample) df_fit <- INVA(df_train, params=p)
            },
            "V2" = {
              p <- v2_fit(df_train)
              df_forecast <- V2(df_test, params=p)
              if (in_sample) df_fit <- V2(df_train, params=p)
            },
            "V3" = {
              p <- v3_fit(df_train)
              df_forecast <- V3(df_test, params=p)
              if (in_sample) df_fit <- V3(df_train, params=p)
            },
            "V4" = {
              p <- v4_fit(df_train)
              df_forecast <- V3(df_test, p$params, p$intercept)
              if (in_sample) df_fit <- V3(df_train, p$params, p$intercept)
            },
            "QRA2" = {
              p <- qra2_fit(df_train)
              df_forecast <- QRA2(df_test, params=p)
              if (in_sample) df_fit <- QRA2(df_train, params=p)
            },
            "QRA3" = {
              p <- qra3_fit(df_train)
              df_forecast <- QRA3(df_test, params=p)
              if (in_sample) df_fit <- QRA3(df_train, params=p)
            },
            "QRA4" = {
              p <- qra4_fit(df_train)
              df_forecast <- QRA4(df_test, p$params, p$intercepts)
              if (in_sample) df_fit <- QRA4(df_train, p$params, p$intercepts)
            },
            "GQRA2" = {
              groups <- get_quantile_groups()
              p <- gqra2_fit(df_train, groups)
              df_forecast <- GQRA_3(df_test, groups, p)
              if (in_sample) df_fit <- GQRA_3(df_train, groups, p)
            },
            "GQRA3" = {
              groups <- get_quantile_groups()
              p <- gqra3_fit(df_train, groups)
              df_forecast <- GQRA_3(df_test, groups, p)
              if (in_sample) df_fit <- GQRA_3(df_train, groups, p)
            },
            "GQRA4" = {
              groups <- get_quantile_groups()
              p <- gqra4_fit(df_train, groups)
              df_forecast <- GQRA_4(df_test, groups, p$params, p$intercepts)
              if (in_sample) df_fit <- GQRA_4(df_train, groups, p$params, p$intercepts)
            },
            "QNA3" = {
              p <- qna3_fit(df_train)
              df_forecast <- QNA3(df_test, p$params, p$intercepts)
              if (in_sample) df_fit <- QNA3(df_train, p$params, p$intercepts)
            }
    )
    df_forecast <- sort_quantiles(df_forecast)
    df_forecast$model <- ensemble
    df_ensembles <- bind_rows(df_ensembles, df_forecast)
    
    df_fit <- sort_quantiles(df_fit)
    df_fit$model <- ensemble
    df_fit$id_date <- min(df_train$target_end_date)
    df_in_sample <- bind_rows(df_in_sample, df_fit)
  }
  
  if (in_sample){
    return(list(df_ensembles, df_in_sample))
  }
  else{
    return(df_ensembles)
  }
}


dfs <- train_test_split(df, test_date=as.Date("2020-08-22"), horizon=1, window_size=4)
df_train <- dfs$df_train
df_test <- dfs$df_test

res <- build_ensembles(df_train, df_test, c("EWA", "MED"), in_sample=TRUE)
oos <- res[[1]]
is <- res[[2]]

is$target_end_date <- paste0(is$target_end_date, '_', is$id_date)

s <- score_forecasts(is, scores=c("wis"))

res2 <- res

rall <- list(res, res2)

comb <- function(x){
  lapply(transpose(x), function(l) do.call(rbind, l))
}

b <- comb(rall)

a <- lapply(transpose(rall), function(l) do.call(rbind, l))

e <- ensemble_forecasts(df, as.Date(c("2020-08-01", "2020-08-08", "2020-08-15", "2020-08-22", "2020-08-29",
                                      "2020-09-05", "2020-09-12")), window_sizes = 4, 
                        exclude_us_from_training=TRUE, ensembles=c("EWA", "MED", "INV", "V3", "INVA"), in_sample=TRUE)

oos <- e[[1]]
is <- e[[2]]

is$target_end_date <- paste0(is$target_end_date, '_', is$id_date)
so <- wis(is)
so %>% group_by(model) %>% summarize(wis = mean(wis))

unique(df$target_end_date)
