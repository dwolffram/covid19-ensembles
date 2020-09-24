train_test_split <- function(df, test_date, window_size){
  train_start = test_date - window_size*7
  df_test = subset(df, target_end_date == test_date)
  df_train = subset(df, (target_end_date < test_date) & (target_end_date >= train_start))
  return(list(df_train=df_train, df_test=df_test))
}

evaluate <- function(df_train, df_test, ensembles){
  scores_temp <- data.frame()
  
  if("EWA" %in% ensembles){
    print("EWA")
    df_forecast <- EWA(df_test)
    scores <- wis_table(df_forecast)
    scores$method <- "EWA"
    scores_temp <- bind_rows(scores_temp, scores)
  }
  
  if ("MED" %in% ensembles){
    print("MED")
    df_forecast <- MED(df_test)
    scores <- wis_table(df_forecast)
    scores$method <- "MED"
    scores_temp <- bind_rows(scores_temp, scores)
  }
  
  if("V2" %in% ensembles){
    print("V2")
    p <- v2_fit(df_train)
    df_forecast <- V2(df_test, params=p)
    scores <- wis_table(df_forecast)
    scores$method <- "V2"
    scores_temp <- bind_rows(scores_temp, scores)
  }
  
  if("V3" %in% ensembles){
    print("V3")
    p <- v3_fit(df_train)
    df_forecast <- V3(df_test, params=p)
    scores <- wis_table(df_forecast)
    scores$method <- "V3"
    scores_temp <- bind_rows(scores_temp, scores)  
  }
  
  if("V4" %in% ensembles){
    print("V4")
    p <- v4_fit(df_train)
    df_forecast <- V3(df_test, p$params, p$intercept)
    scores <- wis_table(df_forecast)
    scores$method <- "V4"
    scores_temp <- bind_rows(scores_temp, scores)  
  }
  
  if("QRA2" %in% ensembles){
    print("QRA2")
    p <- qra2_fit(df_train)
    df_forecast <- QRA2(df_test, params=p)
    scores <- wis_table(df_forecast)
    scores$method <- "QRA2"
    scores_temp <- bind_rows(scores_temp, scores)  }
  
  if("QRA3" %in% ensembles){
    print("QRA3")
    p <- qra3_fit(df_train)
    df_forecast <- QRA3(df_test, params=p)
    scores <- wis_table(df_forecast)
    scores$method <- "QRA3"
    scores_temp <- bind_rows(scores_temp, scores)
  }
  
  if("QRA4" %in% ensembles){
    print("QRA4")
    p <- qra4_fit(df_train)
    df_forecast <- QRA4(df_test, p$params, p$intercepts)
    scores <- wis_table(df_forecast)
    scores$method <- "QRA4"
    scores_temp <- bind_rows(scores_temp, scores)
  }
  
  if("GQRA2" %in% ensembles){
    print("GQRA2")
    groups <- get_quantile_groups()
    p <- gqra2_fit(df_train, groups)
    df_forecast <- GQRA_3(df_test, groups, p)
    scores <- wis_table(df_forecast)
    scores$method <- "GQRA2"
    scores_temp <- bind_rows(scores_temp, scores)
  }
  
  if("GQRA3" %in% ensembles){
    print("GQRA3")
    groups <- get_quantile_groups()
    p <- gqra3_fit(df_train, groups)
    df_forecast <- GQRA_3(df_test, groups, p)
    scores <- wis_table(df_forecast)
    scores$method <- "GQRA3"
    scores_temp <- bind_rows(scores_temp, scores)
  }
  
  if("GQRA4" %in% ensembles){
    print("GQRA4")
    groups <- get_quantile_groups()
    p <- gqra4_fit(df_train, groups)
    df_forecast <- GQRA_4(df_test, groups, p$params, p$intercepts)
    scores <- wis_table(df_forecast)
    scores$method <- "GQRA4"
    scores_temp <- bind_rows(scores_temp, scores)
  }
  
  return(scores_temp)
}


evaluate_ensembles <- function(df, dates, window_sizes, ensembles, 
                               exclude_us_from_training=FALSE, extendResults=NULL){
  old_test_dates <- unique(extendResults$target_end_date)
  
  df_scores <- data.frame()
  
  for (window_size in window_sizes){
    print(paste0("Compute scores for window size ", window_size, "."))
    
    # possible test dates for given window size
    if (window_size >= length(dates)){
      print("Not enough dates for given window size.")
      next
    }
    
    #test_dates <- as.list(dates[(window_size+1):length(dates)])
    test_dates <- as.list(dates[(max(window_sizes)+1):length(dates)])
    test_dates <- setdiff(test_dates, old_test_dates)
    
    all_scores <- foreach(test_date=test_dates, .combine=rbind) %dopar% {
      print(as.character(test_date))
      tryCatch(
        expr = {
          dfs <- train_test_split(df, test_date, window_size)
          
          df_train <- dfs$df_train
          if (exclude_us_from_training){
            df_train <- subset(df_train, location != 'US')
          }
          
          df_test <- dfs$df_test
          
          scores <- evaluate(df_train, df_test, ensembles)
          scores$window_size <- window_size
          scores
        },
        error = function(e){
          message(test_date)
          message(e)
          scores <- NULL
          scores
        },
        warning = function(w){
          message(test_date)
          message(w)
          scores <- NULL
          scores
        }
        
      )
    }
    
    df_scores <- bind_rows(df_scores, all_scores)
  }
  
  # append new results to old results given by extendResults
  df_scores <- bind_rows(extendResults, df_scores)
  
  return(df_scores)
}