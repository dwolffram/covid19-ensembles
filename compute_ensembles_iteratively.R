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

# write.csv(df, "data/individual_models/df_evaluation_study.csv", row.names=FALSE)


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


v3_iter_fit <- function(df_train, c=2){
  scores <- wis(df_train)
  
  models_ranked <- scores %>%
    group_by(model) %>%
    summarize(meanWIS = mean(wis)) %>%
    arrange(meanWIS) %>%
    pull(model)
  
  n <- length(models_ranked)
  
  df_iter <- subset(df_train, model %in% models_ranked[1:2])
  
  p <- v3_fit(df_iter, models=models_ranked[1:2])
  df_iter <- V3(df_iter, params=p, models=models_ranked[1:2])
  df_iter$model <- 'F'
  weights <- p
  
  for (m in models_ranked[-1:-2]){
    df_iter <- bind_rows(df_iter, subset(df_train, model == m))
    #print(unique(df_iter$model))
    p <- v3_fit(df_iter, models=c("F", m))
    #print(p)
    
    # stopping criterion
    if (p[2] < 1/(c*n)){
      # refit
      models_active <- models_ranked[1:length(weights)]
      
      df_train <- df_train %>%
        filter(model %in% models_active)
      
      weights <- v3_fit(df_train, models=models_active, p0=weights)
      
      # assign zero weight to remaining models
      weights <- c(weights, numeric(n - length(weights)))
      break
    } 
    
    df_iter <- V3(df_iter, params=p, models=c("F", m))
    df_iter$model <- 'F'
    
    weights <- c(p[1]*weights, p[2])
  }
  
  return(list(models=models_ranked, params=weights))
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

c <- 2
n <- length(models_ranked)

df_iter <- subset(df_train, model %in% models_ranked[1:2])

p <- v3_fit(df_iter, models=models_ranked[1:2])
df_iter <- V3(df_iter, params=p, models=models_ranked[1:2])
df_iter$model <- 'F'
weights <- p

for (m in models_ranked[-1:-2]){
  print(m)
  df_iter <- bind_rows(df_iter, subset(df_train, model == m))
  print(unique(df_iter$model))
  p <- v3_fit(df_iter, models=c("F", m))
  print(p)
  
  if (p[2] < 1/(c*n)){
    models_active <- models_ranked[1:length(weights)]
    
    df_train <- df_train %>%
      filter(model %in% models_active)
    
    print('Refit')
    weights <- v3_fit(df_train, models=models_active, p0=weights)
    
    weights <- c(weights, numeric(n - length(weights)))
    break
  } 
  
  df_iter <- V3(df_iter, params=p, models=c("F", m))
  df_iter$model <- 'F'
  
  weights <- c(p[1]*weights, p[2])
}


weights2 <- v3_fit(df_train, models=models_active, p0=1.5*weights[1:length(models_active)])

weights <- c(weights, numeric(n - length(weights)))

a <- weights
weights2

for (i in 1:10){
  if (i > 3) break
  print(i)
}

weights <- c(weights, numeric(n - length(weights)))


p3 <- v3_fit(df_train)
p32 <- v3_fit(df_train)


df_v3 <- V3(df_test, params=p3)
mean_wis(df_v3)

df_it <- V3(df_test, params=weights, models=models_ranked)
mean_wis(df_it)

df_v3 <- V3(df_train, params=p3)
mean_wis(df_v3)

df_it <- V3(df_train, params=weights, models=models_ranked)
mean_wis(df_it)

a <- data.frame(model=models_ranked, param=weights)

w <- v3_iter_fit(df_train)
w[["models"]]
w[["params"]]
weights

df_it <- V3(df_train, params=w$params, models=w$models)
mean_wis(df_it)

df_it <- V3(df_test, params=w$params, models=w$models)
mean_wis(df_it)

df_ensembles <- ensemble_forecasts(df, window_sizes=4, ensembles="V3_iter", exclude_us_from_training=TRUE)

file_name <- paste0("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_ws4_v3-iter_refit.csv")
write.csv(df_ensembles, file_name, row.names=FALSE)

# compute scores

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_ws4_v3-iter.csv", 
                               add_baseline = FALSE)

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_ws4_v3-iter_stop.csv", 
                               add_baseline = FALSE)

ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_V3_iter_stop_ws4.csv", row.names=FALSE)

# combine scores

df1 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_all_ws4_oos.csv", remove_revisions=TRUE, long_format=TRUE)

df2 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_V3_iter_ws4.csv", remove_revisions=TRUE, long_format=TRUE)
df2 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_V3_iter_stop_ws4.csv", remove_revisions=TRUE, long_format=TRUE)

df <- bind_rows(df1, df2)

ggplot(subset(df, location != 'US' & window_size==4 & score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
       aes(x=reorder(model, value), y=value,
           fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  geom_bar(position="stack", stat="summary", fun=mean, width=0.7) +
  theme_gray(base_size=14) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.position = "right") +
  scale_fill_viridis(discrete=TRUE, name = NULL,
                     labels = c("Overprediction", "Dispersion", "Underprediction"))+
  #scale_x_discrete(labels = function(l) parse(text=l)) + 
  labs(x = NULL,
       y = "Mean WIS")

ggsave('plots/evaluation_study/mean_wis_1wk_v3-iter.png', width=20, height=15, dpi=500, unit='cm', device='png')


s <- df %>%
  filter(score=='wis' & location != 'US') %>%
  group_by(model) %>%
  summarize(meanWIS = mean(value))
