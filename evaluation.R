setwd("/home/dwolffram/covid19-ensembles")

source("functions.R")
source("ensemble_methods.R")


# all_models <- list.dirs(path = "data-processed/", full.names = FALSE, recursive = FALSE)

models <- c("LANL-GrowthRate", "CovidAnalytics-DELPHI", "MOBS-GLEAM_COVID", 
            "YYG-ParamSearch", "UCLA-SuEIR")

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


train_test_split <- function(df, test_date, window_size){
  train_start = test_date - window_size*7
  df_test = subset(df, target_end_date == test_date)
  df_train = subset(df, (target_end_date < test_date) & (target_end_date >= train_start))
  return(list(df_train=df_train, df_test=df_test))
}

# for(window_size in window_sizes){
#   print(window_size)
#   df_temp <- setNames(data.frame(matrix(ncol = length(ensembles), nrow = 0)), ensembles)
#   
#   for(test_date in as.list(test_dates[(window_size+1):length(test_dates)])){
#     print(as.character(test_date))
#     dfs <- train_test_split(df, test_date, window_size)
#     df_train <- dfs$df_train
#     df_test <- dfs$df_test
#   }
# }


evaluate <- function(df_train, df_test, ensembles){
  scores <- numeric()
  
  alphas <- c(0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  quantile_levels <- sort(c(unique(c(alphas/2, 1-alphas/2)),0.5))
  
  if("EWA" %in% ensembles){
    print("EWA")
    scores <- c(scores, ewa_loss(df_test))
  }
  
  if ("MED" %in% ensembles){
    print("MED")
    scores <- c(scores, ewa_loss(df_test))
  }
  
  if("V2" %in% ensembles){
    print("V2")
    p_v2 <- v2_fit(df_train)
    scores <- c(scores, v2_loss(df_test, p_v2))
  }
  
  if("V3" %in% ensembles){
    print("V3")
    p_v3 <- v3_fit(df_train)
    scores <- c(scores, v3_loss(df_test, p_v3))
  }
  
  if("V4" %in% ensembles){
    print("V4")
    p_v4 <- v4_fit(df_train)
    scores <- c(scores, v3_loss(df_test, p_v4$params, p_v4$intercept))
  }
  
  if("QRA2" %in% ensembles){
    print("QRA2")
    p_qra2 <- qra2_fit(df_train)
    scores <- c(scores, qra2_loss(df_test, p_qra2))
  }
  
  if("QRA3" %in% ensembles){
    print("QRA3")
    p_qra3 <- qra3_fit(df_train)
    scores <- c(scores, qra3_loss(df_test, p_qra3))
  }
  
  if("QRA4" %in% ensembles){
    print("QRA4")
    p_qra4 <- qra4_fit(df_train)
    scores <- c(scores, qra4_loss(df_test, p_qra4$params, p_qra4$intercepts))
  }
  
  if("GQRA2" %in% ensembles){
    print("GQRA2")
    groups <- get_quantile_groups()
    p_qra2 <- gqra2_fit(df_train, groups)
    scores <- c(scores, gqra2_loss(df_test, groups, p_qra2))
  }
  
  if("GQRA3" %in% ensembles){
    print("GQRA3")
    groups <- get_quantile_groups()
    p_qra3 <- gqra3_fit(df_train, groups)
    scores <- c(scores, gqra3_loss(df_test, groups, p_qra3))
  }
  
  if("GQRA4" %in% ensembles){
    print("GQRA4")
    groups <- get_quantile_groups()
    p_qra4 <- gqra4_fit(df_train, groups)
    scores <- c(scores, gqra4_loss(df_test, groups, p_qra4$params, p_qra4$intercepts))
  }
  
  return(scores)
}


evaluate_ensembles <- function(df, dates, window_sizes, ensembles, extendResults=NULL){
  old_test_dates <- unique(extendResults$test_date)
  
  df_scores <- data.frame()
  
  for(window_size in window_sizes){
    print(window_size)
    df_temp <- setNames(data.frame(matrix(ncol = length(ensembles) + 2, nrow = 0)), 
                        c(ensembles, "test_date", "window_size"))
    
    # possible test dates for given window size
    test_dates <- as.list(dates[(window_size+1):length(dates)])
    test_dates <- setdiff(test_dates, old_test_dates)
    
    for(test_date in test_dates){
      print(as.character(test_date))
      dfs <- train_test_split(df, test_date, window_size)
      df_train <- dfs$df_train
      df_test <- dfs$df_test
      
      scores <- evaluate(df_train, df_test, ensembles)
      
      df_temp[nrow(df_temp) + 1, ] <- c(scores, test_date, window_size)
      
    }
    
    df_scores <- bind_rows(df_scores, df_temp)
    
  }
  
  df_scores <- df_scores %>% 
    select(window_size, test_date, everything())
  
  df_scores$test_date <- as.Date(df_scores$test_date, origin="1970-01-01")
  
  df_scores <- bind_rows(extendResults, df_scores)
  
  
  return(df_scores)
}


ensembles <- c("EWA", "MED", "V2", "V3", "V4", "QRA2", "QRA3", "QRA4", "GQRA2", "GQRA3", "GQRA4")
decomp <- c("wgt_iw", "wgt_pen_u", "wgt_pen_l", "wis")

result_cols <- paste(rep(ensembles, each = length(decomp)), decomp, sep = "_")

a <- evaluate_ensembles(df, c(as.Date("2020-05-09"), as.Date("2020-05-16"), as.Date("2020-05-23"),
                               as.Date("2020-05-30")), 
                        c(1, 2), c('EWA'))

a <- evaluate_ensembles(df, c(as.Date("2020-05-09"), as.Date("2020-05-16"), as.Date("2020-05-23")), 
                        c(1), c('EWA', 'V3'))

b <- evaluate_ensembles(df, c(as.Date("2020-05-09"), as.Date("2020-05-16"), as.Date("2020-05-23"),
                               as.Date("2020-05-30")), 
                        c(1), c('EWA', 'V3'), extendResults = a)


window_sizes <- 1:4
ensembles <- c("EWA", "V2", "V3", "V4", "QRA2", "QRA3", "QRA4", "GQRA2", "GQRA3", "GQRA4")


results <- evaluate_ensembles(df, test_dates, window_sizes, ensembles)

write.csv(results, "results/results_qra2.csv", row.names=FALSE)


results_qra2 <- results

results = read.csv('results/results3.csv',
                   colClasses = c(window_size = "factor", test_date = "Date"))

results <- results %>%
  merge(results_qra2)

results <- results[c("window_size", "test_date", "EWA", "V2", "V3", "V4", "QRA2", "QRA3", "QRA4", "GQRA2", "GQRA3", "GQRA4")]

last_date <- last(sort(unique(results$test_date)))

write.csv(results, paste0("results/results_", last_date, ".csv"), row.names=FALSE)
write.csv(results, "results/results.csv", row.names=FALSE)

results <- read.csv("results.csv")
View(t(colMeans(results[, 3:8])))



results = read.csv('results/results.csv',
                   colClasses = c(window_size = "factor", test_date = "Date"))





library(doParallel)
no_cores <- detectCores() - 1  
no_cores <- 16
registerDoParallel(cores=no_cores)  
#cl <- makeCluster(no_cores, type="FORK")  
#stopCluster(cl) 



evaluate_ensembles <- function(df, dates, window_sizes, ensembles, extendResults=NULL){
  old_test_dates <- unique(extendResults$test_date)
  
  df_scores <- data.frame()
  
  for (window_size in window_sizes){
    print(window_size)

    # possible test dates for given window size
    test_dates <- as.list(dates[(window_size+1):length(dates)])
    test_dates <- setdiff(test_dates, old_test_dates)
    
    all_scores <- foreach(test_date=test_dates, .combine=rbind) %dopar% {
      print(as.character(test_date))
      dfs <- train_test_split(df, test_date, window_size)
      df_train <- dfs$df_train
      df_test <- dfs$df_test
      
      scores <- evaluate(df_train, df_test, ensembles)
      
      c(scores, test_date, window_size)
    }
    
    df_temp <- setNames(data.frame(all_scores), c(ensembles, "test_date", "window_size"))
    df_scores <- bind_rows(df_scores, df_temp)
  }
  
  df_scores <- df_scores %>% 
    select(window_size, test_date, everything())
  
  df_scores$test_date <- as.Date(df_scores$test_date, origin="1970-01-01")
  
  # append new results to old results given by extendResults
  df_scores <- bind_rows(extendResults, df_scores)
  
  
  return(df_scores)
}

a <- evaluate_ensembles(df, c(as.Date("2020-05-09"), as.Date("2020-05-16"), as.Date("2020-05-23"),
                               as.Date("2020-05-30")), 
                        c(1, 2), c('EWA'))

a <- evaluate_ensembles(df1, possible_dates, 
                        1:4, c('EWA', 'V3'))

window_sizes <- 1:4
ensembles <- c("EWA", "V2", "V3", "V4", "QRA2", "QRA3", "QRA4", "GQRA2", "GQRA3", "GQRA4")

results <- evaluate_ensembles(df, possible_dates, window_sizes, ensembles)

write.csv(results, "results/results_parallel.csv", row.names=FALSE)



# evaluate_ensembles <- function(df, dates, window_sizes, ensembles, extendResults=NULL){
#   old_test_dates <- unique(extendResults$test_date)
#   
#   df_scores <- data.frame()
#   
#   df_scores <- foreach(window_size=window_sizes, .combine=rbind) %dopar% {
#     print(window_size)
#     df_temp <- setNames(data.frame(matrix(ncol = length(ensembles) + 2, nrow = 0)), 
#                         c(ensembles, "test_date", "window_size"))
#     
#     # possible test dates for given window size
#     test_dates <- as.list(dates[(window_size+1):length(dates)])
#     test_dates <- setdiff(test_dates, old_test_dates)
#     
#     all_scores <- foreach(test_date=test_dates, .combine=rbind) %dopar% {
#       print(as.character(test_date))
#       dfs <- train_test_split(df, test_date, window_size)
#       df_train <- dfs$df_train
#       df_test <- dfs$df_test
#       
#       scores <- evaluate(df_train, df_test, ensembles)
#       
#       c(scores, test_date, window_size)
#     }
#     df_temp <- bind_rows(df_temp, all_scores)
#     
#     #df_scores <- bind_rows(df_scores, df_temp)
#     df_temp
#   }
#   
#   df_scores <- df_scores %>% 
#     select(window_size, test_date, everything())
#   
#   df_scores$test_date <- as.Date(df_scores$test_date, origin="1970-01-01")
#   
#   df_scores <- bind_rows(extendResults, df_scores)
#   
#   
#   return(df_scores)
# }

df_test
EWA(df_test)

### NEW APPROACH
ensembles <- c("EWA", "MED")
df_scores <- data.frame()
for (ensemble in ensembles){
  print(ensemble)
  df_forecast <- do.call(ensemble, args=list(df=df_test))
  scores <- wis_table(df_forecast)
  scores$method <- ensemble
  df_scores <- bind_rows(df_scores, scores)
}

evaluate <- function(df_train, df_test, ensembles){
  scores_temp <- data.frame()
  
  # alphas <- c(0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  # quantile_levels <- sort(c(unique(c(alphas/2, 1-alphas/2)),0.5))
  
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


evaluate_ensembles <- function(df, dates, window_sizes, ensembles, extendResults=NULL){
  old_test_dates <- unique(extendResults$target_end_date)
  
  df_scores <- data.frame()
  
  for (window_size in window_sizes){
    print(paste0("Compute scores for window size ", window_size, "."))
    
    # possible test dates for given window size
    if (window_size >= length(dates)){
      print("Not enough dates for given window size.")
      next
    }
    test_dates <- as.list(dates[(window_size+1):length(dates)])
    test_dates <- setdiff(test_dates, old_test_dates)
    
    all_scores <- foreach(test_date=test_dates, .combine=rbind) %dopar% {
      print(as.character(test_date))
      tryCatch(
        expr = {
          dfs <- train_test_split(df, test_date, window_size)
          df_train <- dfs$df_train
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
a <- evaluate_ensembles(df, c(as.Date("2020-05-09"), as.Date("2020-05-16")), 
                        c(1, 2), c('EWA', 'MED'))

a <- evaluate_ensembles(df, possible_dates, 
                        c(1, 2, 3, 4), c('EWA', 'MED'))

ensembles <- c("EWA", "MED", "V2", "V3", "V4", "QRA2", "QRA3", "QRA4", "GQRA2", "GQRA3", "GQRA4")
a <- evaluate_ensembles(df, c(as.Date("2020-05-09"), as.Date("2020-05-16"), as.Date("2020-05-23"),
                              as.Date("2020-05-30")), 
                        c(1, 2), ensembles)

dates <- c(as.Date("2020-05-09"), as.Date("2020-05-16"))
test_dates <- as.list(dates[(window_size+1):length(dates)])

library(doParallel)
no_cores <- detectCores() - 1  
no_cores <- 16
registerDoParallel(cores=no_cores)  

ensembles <- c("EWA", "MED", "V2", "V3", "V4", "QRA2", "QRA3", "QRA4", "GQRA2", "GQRA3", "GQRA4")
window_sizes <- 1:4
results <- evaluate_ensembles(df, possible_dates, window_sizes, ensembles)

write.csv(results, "results/results_2020-08-09.csv", row.names=FALSE)

dates <- possible_dates
for (window_size in window_sizes){
  print(paste0("Compute scores for window size ", window_size, "."))
  
  # possible test dates for given window size
  if (window_size >= length(dates)){
    print("Not enough dates for given window size.")
    next
  }
  test_dates <- as.list(dates[(window_size+1):length(dates)])

  for (test_date in test_dates){
    print(as.character(test_date))
    dfs <- train_test_split(df, test_date, window_size)
    df_train <- dfs$df_train
    df_test <- dfs$df_test
    print(nrow(df_train))
    print(nrow(df_test))
  }
}


log_calculator <- function(x){
  tryCatch(
    expr = {
      message(log(x))
    },
    warning = function(w){
      message('Caught an warning!')
      print(w)
      return(NULL)
    }
  )    
}
log_calculator(-10)
log('bla')
