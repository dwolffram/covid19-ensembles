setwd("/home/dwolffram/covid19-ensembles")

#library(quantreg)

source("functions.R")
source("ensemble_methods.R")


# all_models <- list.dirs(path = "data-processed/", full.names = FALSE, recursive = FALSE)

models <- c("LANL-GrowthRate", "MIT_CovidAnalytics-DELPHI", "MOBS_NEU-GLEAM_COVID", 
            "YYG-ParamSearch", "UCLA-SuEIR")

exclude_locations <- c("11", "66", "69", "72", "78")


df <- load_df(models=models, exclude_locations=exclude_locations)

df <- df %>% 
  filter(target == "1 wk ahead cum death")


df %>%
  group_by(target_end_date) %>%
  summarize(model_count = length(unique(model)))

df %>%
  group_by(target_end_date) %>%
  summarize(model_count = length(unique(model))) %>%
  filter(model_count >= 5)

# target_end_dates with all 5 model predictions
possible_dates <- df %>%
  group_by(target_end_date) %>%
  summarize(model_count = length(unique(model))) %>%
  filter(model_count >= 5) %>% 
  pull(target_end_date)

# not all targets are always available...
available_targets <- subset(df, target_end_date %in% possible_dates) %>%
  group_by(target_end_date, target) %>%
  summarize(model_count = length(unique(model)))

subset(df, target_end_date %in% possible_dates) %>%
  group_by(target_end_date, target) %>%
  summarize(model_count = length(unique(model))) %>%   
  filter(model_count >= 5)



#"2020-05-09" "2020-05-16" "2020-05-23" "2020-05-30" "2020-06-06" "2020-06-13" "2020-06-20"

#test_dates <- possible_dates[5:length(possible_dates)]
test_dates <- possible_dates


#window_size = 4
#train_start = test_dates - window_size*7


train_test_split <- function(df, test_date, window_size){
  train_start = test_date - window_size*7
  df_test = subset(df, target_end_date == test_date)
  df_train = subset(df, (target_end_date < test_date) & (target_end_date >= train_start))
  return(list(df_train=df_train, df_test=df_test))
}

for(window_size in window_sizes){
  print(window_size)
  df_temp <- setNames(data.frame(matrix(ncol = length(ensembles), nrow = 0)), ensembles)
  
  for(test_date in as.list(test_dates[(window_size+1):length(test_dates)])){
    print(as.character(test_date))
    dfs <- train_test_split(df, test_date, window_size)
    df_train <- dfs$df_train
    df_test <- dfs$df_test
  }
}


for(test_date in as.list(test_dates)){
  print(test_date)
}

g1 <- c(0.01, 0.025, 0.05, 0.1)
g2 <- c(0.15, 0.2, 0.25, 0.3)
g3 <- c(0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65)
g4 <- c(0.7, 0.75, 0.8, 0.85)
g5 <- c(0.9, 0.95, 0.975, 0.99)

groups <- data.frame(quantile=c(g1, g2, g3, g4, g5), 
                     quantile_group=c(rep("g1", length(g1)), rep("g2", length(g2)), 
                                      rep("g3", length(g3)), rep("g4", length(g4)), 
                                      rep("g5", length(g5))))


ensembles <- c("EWA", "V2", "V3", "V4", "QRA2", "QRA3", "QRA4", "GQRA2", "GQRA3", "GQRA4")

ensembles <- c("QRA2")

window_sizes <- 1:4

evaluate_ensembles <- function(df, test_dates, window_sizes, ensembles){
  df_scores <- data.frame()
  
  for(window_size in window_sizes){
    print(window_size)
    df_temp <- setNames(data.frame(matrix(ncol = length(ensembles), nrow = 0)), ensembles)
    
    for(test_date in as.list(test_dates[(window_size+1):length(test_dates)])){
      print(as.character(test_date))
      dfs <- train_test_split(df, test_date, window_size)
      df_train <- dfs$df_train
      df_test <- dfs$df_test
      
      scores <- numeric()
      
      if("EWA" %in% ensembles){
        print("EWA")
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
        p_qra2 <- gqra2_fit(df_train, groups)
        scores <- c(scores, gqra2_loss(df_test, groups, p_qra2))
      }
      
      if("GQRA3" %in% ensembles){
        print("GQRA3")
        p_qra3 <- gqra3_fit(df_train, groups)
        scores <- c(scores, gqra3_loss(df_test, groups, p_qra3))
      }
      
      if("GQRA4" %in% ensembles){
        print("GQRA4")
        p_qra4 <- gqra4_fit(df_train, groups)
        scores <- c(scores, gqra4_loss(df_test, groups, p_qra4$params, p_qra4$intercepts))
      }
      
      df_temp[nrow(df_temp) + 1, ] <- scores
    }
    
    df_temp$test_date <- test_dates[(window_size+1):length(test_dates)]
    df_temp$window_size <- window_size
    df_scores <- bind_rows(df_scores, df_temp)
  }
  
  df_scores <- df_scores %>% 
    select(window_size, test_date, everything())
  
  return(df_scores)
}


results <- evaluate_ensembles(df, test_dates, window_sizes, ensembles)

write.csv(results, "results/results_qra2.csv", row.names=FALSE)


results <- read.csv("results.csv")
View(t(colMeans(results[, 3:8])))




names(results)





a <- train_test_split(df, as.Date("2020-06-06"), 2)
a_train <- a$df_train
a_test <- a$df_test


quantile_levels <- sort(unique(a_train$quantile))
n_models <- length(unique(a_train$model))
models <- unique(a_train$model)

#for linear inequality constraints: Rb >= r
R = rbind(diag(n_models - 1), rep(-1, n_models - 1))
r = c(rep(0, n_models - 1), -1)

df_params <- data.frame()
#drop one model
df2 <- subset(a_train, model!=tail(models, 1))
df_temp <- subset(df2, quantile==0.5) %>% 
  select(c("target_end_date", "location", "truth", "model", "value")) %>% 
  spread(model, value) %>% 
  drop_na()

params <- rq(truth ~ . - target_end_date - location -1, 
           tau = 0.65, R=R, r=r, data = df_temp, method="fnc")$coefficients

params <- data.frame(model=str_sub(names(params), 2, -2), quantile=0.01, 
                   param=params, row.names = NULL)
df_params <- bind_rows(df_params, params)

jitter(df_temp)


p_qra2 <- qra2_fit(a_train)
scores <- c(scores, qra2_loss(df_test, p_qra2))
