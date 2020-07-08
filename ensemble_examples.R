setwd("D:/Dokumente/Workspace2/covid19-ensembles")


source("functions.R")
source("ensemble_methods.R")

library(quantreg)


models <- c("LANL-GrowthRate", "MIT_CovidAnalytics-DELPHI", "MOBS_NEU-GLEAM_COVID", 
            "YYG-ParamSearch", "UCLA-SuEIR")

exclude_locations <- c("11", "66", "69", "72", "78")


df <- load_df(models=models, exclude_locations=exclude_locations)

df <- df %>% 
  filter(target == "1 wk ahead cum death")

# target_end_dates with all 5 model predictions
possible_dates <- df %>%
  group_by(target_end_date) %>%
  summarize(model_count = length(unique(model))) %>%
  filter(model_count >= 5) %>% 
  pull(target_end_date)


test_dates <- possible_dates[4:6]

window_size = 3
train_start = test_dates - window_size*7

df_test = subset(df, target_end_date == test_dates[1])
df_train = subset(df, (target_end_date < test_dates[1]) & (target_end_date >= train_start[1]))


### EWA
ewa_df <- EWA(df_test)

ewa_scores <- wis_table(ewa_df)
mean(ewa_scores$wis)
mean_wis(ewa_df)

ewa_loss(df_test)

### V3
v3_df <- V3(df_test, params=rep(0.2, 5))
v3_scores <- wis_table(v3_df)
mean(v3_scores$wis)

mean_wis(v3_df)
v3_loss(df_test, rep(0.2, 5))

# Estimation
p_v3 <- v3_fit(df_train)
v3_loss(df_test, p_v3)


### V4
# Set an intercept in V3
v4_df <- V3(df_test, params=rep(0.2, 5), intercept=2)
mean_wis(v4_df)
v3_loss(df_test, rep(0.2, 5), intercept=2)

p_v4 <- v4_fit(df_train)
v3_loss(df_test, p_v4$params, p_v4$intercept)


### V2

v2_df <- V2(df_test, params=rep(0.2, 4))
mean_wis(v2_df)
v2_loss(df_test, rep(0.2, 4))

p_v2 <- v2_fit(df_train)
v2_loss(df_test, p_v2)

### QRA3


alphas <- c(0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
quantile_levels <- sort(c(unique(c(alphas/2, 1-alphas/2)),0.5))

#quantile_levels1 <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)

params <- data.frame(model=rep(models, each=23), quantile=quantile_levels, param=rep(0.2, 5*23))

qra3_df <- QRA3(df_test, params=params)
qra3_loss(df_test, params)

p_qra3 <- qra3_fit(df_train)
qra3_loss(df_test, p_qra3)

### QRA4

intercepts <- data.frame(quantile=quantile_levels, intercept=rep(0, 23))

qra4_df <- QRA4(df_test, params=params, intercepts=intercepts)
qra4_loss(df_test, params, intercepts)

p_qra4 <- qra4_fit(df_train)
qra4_loss(df_test, p_qra4$params, p_qra4$intercepts)

### QRA2

p_qra2 <- qra2_fit(df_train)


qra2_loss <- function(df, params){
  params <- data.frame(model=rep(models[-length(models)], each=23), quantile=quantile_levels, param=params)
  df_temp <- QRA2(df, params)
  return(mean_wis(df_temp))
}


qra2_fit <- function(df){
  quantile_levels <- sort(unique(df$quantile))
  models <- unique(df$model)
  models <- models[-length(models)]  # drop last model
  n_models <- length(models)
  
  #for linear inequality constraints: Rb >= r
  R = rbind(diag(n_models - 1), rep(-1, n_models - 1))
  r = c(rep(0, n_models - 1), -1)
  
  df_params <- data.frame()
  #drop one model # TODO: CHECK THIS, shouldnt be dropped completely?
  #df2 <- subset(df, model!=tail(models, 1))
  for (quantile_level in quantile_levels){
    df_temp <- subset(df, quantile==quantile_level) %>% 
      select(c("target_end_date", "location", "truth", "model", "value")) %>% 
      spread(model, value) %>% 
      drop_na()
    
    params <- try(rq(truth ~ . - target_end_date - location -1, 
                     tau = quantile_level, R=R, r=r, data = df_temp, method="fnc")$coefficients)
    # in case of singular design matrix user jitter
    if("try-error" %in% class(params)){
      params <- rq(jitter(truth) ~ . - target_end_date - location -1, 
                   tau = quantile_level, R=R, r=r, data = df_temp, method="fnc")$coefficients
    }
    
    params <- data.frame(model=str_sub(names(params), 2, -2), quantile=quantile_level, 
                         param=params, row.names = NULL)
    df_params <- bind_rows(df_params, params)
  }
  
  return(df_params)
}

quantile_levels <- sort(unique(df_train$quantile))
n_quantiles <- length(quantile_levels)
models <- unique(df_train$model)
models <- models[-length(models)]  # drop last model
n_models <- length(models)
# feasible region: ui %*% theta - ci >= 0
r <- c(rep(0, n_models*n_quantiles), rep(-1, n_quantiles))
R <- diag(n_models*n_quantiles)

R_l <- matrix(0, nrow=n_quantiles, ncol=n_models*n_quantiles)
for (i in 1:n_quantiles){
  for (j in 1:n_models){
    R_l[i, i + (j-1)*n_quantiles] <- - 1
  }
}
R <- rbind(R, R_l)


p_optim <- constrOptim(theta = rep(1/(n_models*n_quantiles), n_models*n_quantiles), 
                       f = function(x){
                         return(qra2_loss(df_train, params = x))},
                       ui=R, ci=r, method="Nelder-Mead")$par

qra2_loss(df_test, p_optim)

params2$param <- p_optim
params2 %>% 
  group_by(model, quantile) %>% 
  summarize(sum(param))

qra2_df <- QRA2(df_test, params2)
mean_wis(qra2_df)

params2 <- params %>% filter(model!=tail(models, 1))
qra2_df <- QRA2(df_test, params2)

qra2_loss(df_test, params2)
qra2_loss(df_test, params2$param)



p_qra2 <- qra2_fit(df_train)
qra2_loss(df_test, p_qra2)

### GQRA_3
params <- data.frame(model=rep(models, each=5), 
                     quantile_group=rep(c("g1", "g2", "g3", "g4", "g5"), 5), 
                     param=rep(0.2, 5*5))

gqra53_df <- GQRA_3(df_test, groups, params)
mean_wis(gqra53_df)
gqra3_loss(df_test, groups, params=rep(0.2, 5*5))

p_gqra3 <- gqra3_fit(df_train, groups)
gqra3_loss(df_test, groups, p_gqra3)

# View(data.frame(model=rep(unique(df_test$model), each=5), 
#                 quantile_group=rep(c("g1", "g2", "g3", "g4", "g5"), 5), 
#                 param=rep(0.2, 5*5)))

### GQRA_4
intercepts <- data.frame(quantile_group=c("g1", "g2", "g3", "g4", "g5"), intercept=rep(0, 5))
gqra54_df <- GQRA_4(df_test, groups, params, intercepts)
mean_wis(gqra54_df)
gqra4_loss(df_test, groups, params$param, intercepts$intercept)

p_gqra4 <- gqra4_fit(df_train, groups)
gqra4_loss(df_test, groups, p_gqra4$params, p_gqra4$intercept)


### GQRA_2
params2 <- params %>% filter(model!=tail(models, 1))

gqra2_df <- GQRA_2(df_test, groups, params2)
mean_wis(gqra2_df)

gqra2_loss(df_test, groups, params2$param)

p_gqra2 <- gqra2_fit(df_train, groups)
gqra2_loss(df_test, groups, p_gqra2)

gqra2_df <- GQRA_2(df_test, groups,p_gqra2)


### Verify that parameters for each group sum to 1
group_names <- unique(groups$quantile_group)
models <- unique(df$model)
n_groups <- length(group_names)
n_models <- length(models)
params4 <- data.frame(model=rep(models[-length(models)], each=n_groups), 
                     quantile_group=rep(group_names, n_models-1), 
                     param=p_gqra2)

last_param <- models[!(models %in% unique(params4$model))] 
# infer parameters for each quantile group from given parameters 1-sum(given params)
params3 <- rbind(params4, params4 %>% 
                  group_by(quantile_group) %>% 
                  summarize(param=1-sum(param), .groups="keep") %>% 
                  mutate(model=last_param)) 


params3 %>% group_by(quantile_group) %>% summarize(sum(param))


# View(df_temp %>% select(c(location, location_name)))
# 
# df_temp %>% mutate(location_name = replace(location_name, 
#                                            location=="US" & location_name=="None",
#                                            "US"))
# 
# df_temp %>% filter(location=="US" & location_name=="None")
