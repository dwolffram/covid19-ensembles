options(dplyr.summarise.inform=F)

sort_quantiles <- function(df_forecast){
  return(
    df_forecast %>% 
      group_by(target_end_date, location, target) %>%
      mutate(quantile = sort(quantile), value = sort(value)) %>%
      as.data.frame()
  )
}

train_test_split <- function(df, test_date, horizon, window_size){
  train_end = test_date - 7*horizon
  train_start = train_end - 7*(window_size - 1)

  ### TRAINING DATA
  df_train = subset(df, (target_end_date >= train_start) & (target_end_date <= train_end))
  
  # load historic truth data for training (possibly unrevised at forecast date)
  forecast_date <- max(df_train$target_end_date + 2)
  truth_at_forecast_date <- read.csv(paste0("data/jhu_historic_deaths/processed/truth_jhu_deaths_as_of_",
                                            forecast_date, ".csv"), 
                                     colClasses = c(location = "character", date = "Date"))
  
  df_train <- df_train %>%
    left_join(truth_at_forecast_date, by=c("target_end_date"="date", "location"="location")) %>%
    rename(truth = truth_at_forecast_date)
  
  ### TEST DATA
  df_test = subset(df, target_end_date == test_date)
  
  truth <- read.csv(paste0(path_hub, "data-truth/truth-Cumulative Deaths.csv"),
                    colClasses = c(location = "character", date = "Date")) %>%
    rename(truth = value) %>% 
    select(-location_name)
  
  df_test <- df_test %>%
    left_join(truth, by=c("target_end_date"="date", "location"="location"))
  
  return(list(df_train=df_train, df_test=df_test))
}

# dfs <- train_test_split(df, test_date=as.Date("2020-08-22"), horizon=1, window_size=4)
# df_train <- dfs$df_train
# df_test <- dfs$df_test


ensemble_forecasts <- function(df, dates, window_sizes, ensembles=c("EWA"), 
                               exclude_us_from_training=FALSE){
  
  horizon <- as.numeric(substr(unique(df$target), 1, 1)) # first character of target is the horizon

  if(missing(dates)){
    dates <- unique(df$target_end_date)
  }
   
  df_ensembles <- data.frame()
  
  for (window_size in window_sizes){
    print(paste0("Compute scores for window size ", window_size, "."))
    
    # possible test dates for given window size
    if (window_size + horizon > length(dates)){
      print("Not enough dates for given window size and horizon.")
      next
    }
    
    test_dates <- as.list(dates[(max(window_sizes) + horizon):length(dates)])

    all_forecasts <- foreach(test_date=test_dates, .combine=rbind) %dopar% {
      print(as.character(test_date))
      tryCatch(
        expr = {
          dfs <- train_test_split(df, test_date, horizon, window_size)
          
          df_train <- dfs$df_train
          if (exclude_us_from_training){
            df_train <- subset(df_train, location != 'US')
          }
          
          df_test <- dfs$df_test
          
          df_forecasts <- build_ensembles(df_train, df_test, ensembles)
          df_forecasts$window_size <- window_size
          df_forecasts
        },
        error = function(e){
          message(test_date)
          message(e)
          df_forecasts <- NULL
          df_forecasts
        },
        warning = function(w){
          message(test_date)
          message(w)
          df_forecasts <- NULL
          df_forecasts
        }
        
      )
    }
    
    df_ensembles <- bind_rows(df_ensembles, all_forecasts)
  }
  
  return(df_ensembles)
}


build_ensembles <- function(df_train, df_test, 
                            ensembles=c("EWA", "MED", "INV", "V2", "V3", "V4", 
                                        "QRA2", "QRA3", "QRA4", 
                                        "GQRA2", "GQRA3", "GQRA4")){
  df_ensembles <- data.frame()

  for (ensemble in ensembles){
    print(ensemble)
    switch (ensemble,
            "EWA" = {
              df_forecast <- EWA(df_test)
            },
            "MED" = {
              df_forecast <- MED(df_test)
            },
            "INV" = {
              p <- inv_fit(df_train)
              df_forecast <- INV(df_test, params=p)
            },
            "V2" = {
              p <- v2_fit(df_train)
              df_forecast <- V2(df_test, params=p)
            },
            "V3" = {
              p <- v3_fit(df_train)
              df_forecast <- V3(df_test, params=p)
            },
            "V4" = {
              p <- v4_fit(df_train)
              df_forecast <- V3(df_test, p$params, p$intercept)
            },
            "QRA2" = {
              p <- qra2_fit(df_train)
              df_forecast <- QRA2(df_test, params=p)
            },
            "QRA3" = {
              p <- qra3_fit(df_train)
              df_forecast <- QRA3(df_test, params=p)
            },
            "QRA4" = {
              p <- qra4_fit(df_train)
              df_forecast <- QRA4(df_test, p$params, p$intercepts)
            },
            "GQRA2" = {
              groups <- get_quantile_groups()
              p <- gqra2_fit(df_train, groups)
              df_forecast <- GQRA_3(df_test, groups, p)
            },
            "GQRA3" = {
              groups <- get_quantile_groups()
              p <- gqra3_fit(df_train, groups)
              df_forecast <- GQRA_3(df_test, groups, p)
            },
            "GQRA4" = {
              groups <- get_quantile_groups()
              p <- gqra4_fit(df_train, groups)
              df_forecast <- GQRA_4(df_test, groups, p$params, p$intercepts)
            }
    )
    df_forecast <- sort_quantiles(df_forecast)
    df_forecast$model <- ensemble
    df_ensembles <- bind_rows(df_ensembles, df_forecast)
  }
  
  return(df_ensembles)
}


# a <- build_ensembles(df_train, df_test, c("EWA", "MED", "V3", "QRA3", "GQRA3"))
