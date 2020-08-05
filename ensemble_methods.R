library(quantreg)

### EWA
EWA <- function(df){
  ewa <- data.frame(
    df %>%
      group_by(target_end_date, location, target, quantile, truth) %>% 
      summarize(value = mean(value), .groups="keep"))
  return(ewa)
}

ewa_loss <- function(df){
  df_temp <- EWA(df)
  return(mean_wis(df_temp))
}

### Vincentization
# V3 and V4 together
V3 <- function(df, params, intercept=0){
  params <- data.frame(model=models, param=params)
  df_temp <- merge(df, params, by.x = "model", by.y = "model")
  v3 <- data.frame(
    df_temp %>%
      mutate(weighted_values = value * param) %>%
      group_by(target_end_date, location, target, quantile, truth) %>% 
      summarize(value = sum(weighted_values) + intercept, .groups="keep"))
  return(v3)
}

v3_loss <- function(df, params, intercept=0){
  df_temp <- V3(df, params, intercept)
  return(mean_wis(df_temp))
}

v3_fit <- function(df, method="BFGS"){
  n_models = length(unique(df$model))
  
  p_optim <- optim(par = rep(1/n_models, n_models), 
                   fn = function(x){
                     return(v3_loss(df, params = x, intercept = 0))},
                   method = method)$par
  return(p_optim)
}

v4_fit <- function(df, method="BFGS"){
  n_models = length(unique(df$model))
  
  p_optim <- optim(par = c(0, rep(1/n_models, n_models)), 
                   fn = function(x){
                     return(v3_loss(df, params = x[2:length(x)], intercept = x[1]))},
                   method = method)$par
  
  intercept <- p_optim[1]
  p_optim <- p_optim[2:length(p_optim)]
  return(list(params=p_optim, intercept=intercept))
}

V2 <- function(df, params){
  params = c(params, 1-sum(params)) # the last parameter is given by the others
  v2 <- V3(df, params)
  return(v2)
}

v2_loss <- function(df, params){
  df_temp <- V2(df, params)
  return(mean_wis(df_temp))
}

v2_fit <- function(df){
  n_models <- length(unique(df$model))
  
  # ui %*% theta - ci >= 0  
  ui <- rbind(diag(n_models-1), rep(-1, n_models-1))
  ci <- c(rep(0, n_models-1), -1)
  
  p_optim <- constrOptim(theta = rep(1/n_models, n_models-1), 
                         f = function(x){
                           return(v2_loss(df, params = x))},
                         ui = ui, ci = ci,
                         method="Nelder-Mead")$par
  return(p_optim)
}



### QRA 3

# params: data.frame with columns model, quantile, param
QRA3 <- function(df, params){
  df_temp <- merge(df, params, by.x = c("model", "quantile"), by.y = c("model", "quantile"))
  qra3 <- data.frame(
    df_temp %>%
      mutate(weighted_values = value * param) %>%
      group_by(target_end_date, location, target, quantile, truth) %>% 
      summarize(value = sum(weighted_values), .groups="keep"))
  return(qra3)
}

# QRA3 <- function(df, params){
#   df_temp <- merge(df, params, by.x = c("model", "quantile"), by.y = c("model", "quantile"))
#   qra3 <- data.frame(
#     df_temp %>%
#       mutate(weighted_values = value * param) %>%
#       group_by(target_end_date, location, quantile, truth) %>% 
#       summarize(value = sum(weighted_values)/n_distinct(target), .groups="keep"))
#   return(qra3)
# }


qra3_fit <- function(df){
  quantile_levels <- sort(unique(df$quantile))
  df_params <- data.frame()
  for (quantile_level in quantile_levels){
    df_temp <- subset(df, quantile==quantile_level) %>% 
      select(c("target_end_date", "location", "truth", "model", "value")) %>% 
      spread(model, value) %>% 
      drop_na()
    
    params <- rq(truth ~ . - target_end_date - location - truth -1, 
                 tau = quantile_level, data = df_temp)$coefficients
    
    params <- data.frame(model=str_sub(names(params), 2, -2), quantile=quantile_level, 
                         param=params, row.names = NULL)
    df_params <- bind_rows(df_params, params)
  }
  
  return(df_params)
}


qra3_loss <- function(df, params){
  df_temp <- QRA3(df, params)
  temp_scores <- wis_table(df_temp)
  loss <- mean(temp_scores$wis)
  return(loss)
}

### QRA4

# params: data.frame with columns: model, quantile, param
# intercepts: data.frame with columns: quantile, intercept
QRA4 <- function(df, params, intercepts){
  df_temp <- merge(df, params, by.x = c("model", "quantile"), by.y = c("model", "quantile"))
  qra4 <- data.frame(
    df_temp %>%
      mutate(weighted_values = value * param) %>%
      group_by(target_end_date, location, target, quantile, truth) %>% 
      summarize(value = sum(weighted_values), .groups="keep"))
  qra4 <- merge(qra4, intercepts, by.x = "quantile", by.y = "quantile") %>% 
    mutate(value = value + intercept) %>% 
    select(-intercept)
  return(qra4)
}


qra4_fit <- function(df){
  quantile_levels <- sort(unique(df$quantile))
  
  # dataframe to store: model, quantile and param value
  df_params <- data.frame()
  intercepts <- c()
  
  for (quantile_level in quantile_levels){
    df_temp <- subset(df, quantile==quantile_level) %>% 
      select(c("target_end_date", "location", "truth", "model", "value")) %>% 
      spread(model, value) %>% 
      drop_na()
    
    params <- rq(truth ~ . - target_end_date - location - truth, 
                 tau = quantile_level, data = df_temp)$coefficients
    
    intercepts <- c(intercepts, params[1])
    params <- params[2: length(params)]
    
    #str_sub to remove single quotes around model name
    params <- data.frame(model=str_sub(names(params), 2, -2), quantile=quantile_level, 
                         param=params, row.names = NULL) 
    df_params <- bind_rows(df_params, params)
  }
  df_intercepts <- data.frame(quantile=quantile_levels, intercept=intercepts)
  return(list(params=df_params, intercepts=df_intercepts))
}

qra4_loss <- function(df, params, intercepts){
  df_temp <- QRA4(df, params, intercepts)
  return(mean_wis(df_temp))
}


### QRA2

# asymmetric piecewise linear scoring function
L <- function(alpha, x, y){
  return(((x >= y) - alpha)*(x - y))
}

# Function to minimize for QRA2
fn <- function(alpha, df, params){
  #quantile_levels <- sort(unique(df$quantile))
  params <- data.frame(model=rep(models[-length(models)], each=23), 
                       quantile=rep(quantile_levels, length(models)-1),
                       param=params)
  df_temp <- QRA2(df, params)
  df_temp <- subset(df_temp, quantile==alpha)
  L_alpha <- L(alpha, df_temp$value, df_temp$truth)
  return(mean(L_alpha))
}


# one parameter less, the last one is 1- sum(given params)
QRA2 <- function(df, params){
  models <- unique(df$model)
  last_param <- models[!(models %in% unique(params$model))]

  params <- rbind(params, params %>% 
                    group_by(quantile) %>% 
                    summarize(param=1-sum(param)) %>% 
                    mutate(model=last_param))
  
  df_temp <- merge(df, params, by.x = c("model", "quantile"), by.y = c("model", "quantile"))
  qra2 <- data.frame(
    df_temp %>%
      mutate(weighted_values = value * param) %>%
      group_by(target_end_date, location, target, quantile, truth) %>% 
      summarize(value = sum(weighted_values)))
  return(qra2)
}

qra2_loss <- function(df, params){
  df_temp <- QRA2(df, params)
  return(mean_wis(df_temp))
}

qra2_fit <- function(df){
  models <- unique(df$model)
  n_models <- length(models) 
  quantile_levels <- sort(unique(df$quantile))
  
  # ui %*% theta - ci >= 0  
  ui <- rbind(diag(n_models-1), rep(-1, n_models-1))
  ci <- c(rep(0, n_models-1), -1)
  
  df_params <- data.frame()
  for (quantile_level in quantile_levels){
    params <- constrOptim(theta = rep(1/n_models, n_models - 1), 
                          f = function(x){
                            return(fn(quantile_level, df, params = x))},
                          ui=ui, ci=ci, method="Nelder-Mead")$par
    
    params <- data.frame(model=models[-length(models)], quantile=quantile_level, 
                         param=params, row.names = NULL)
    df_params <- bind_rows(df_params, params)
  }
  return(df_params)
}

# qra2_loss <- function(df, params){
#   quantile_levels <- sort(unique(df$quantile))
#   params <- data.frame(model=rep(models[-length(models)], each=23), 
#                        quantile=rep(quantile_levels, length(models)-1),
#                        param=params)
#   df_temp <- QRA2(df, params)
#   return(mean_wis(df_temp))
# }


# qra2_fit <- function(df){
#   quantile_levels <- sort(unique(df$quantile))
#   n_quantiles <- length(quantile_levels)
#   models <- unique(df$model)
#   #models <- models[-length(models)]  # drop last model
#   n_models <- length(models) - 1
#   
#   # feasible region: ui %*% theta - ci >= 0
#   r <- c(rep(0, n_models*n_quantiles), rep(-1, n_quantiles))
#   R <- diag(n_models*n_quantiles)
#   
#   R_l <- matrix(0, nrow=n_quantiles, ncol=n_models*n_quantiles)
#   for (i in 1:n_quantiles){
#     for (j in 1:n_models){
#       R_l[i, i + (j-1)*n_quantiles] <- - 1
#     }
#   }
#   R <- rbind(R, R_l)
#   
#   p_optim <- constrOptim(theta = rep(1/(n_models*n_quantiles), n_models*n_quantiles), 
#                          f = function(x){
#                            return(qra2_loss(df, params = x))},
#                          ui=R, ci=r, method="Nelder-Mead")$par
#     
#   params <- data.frame(model=rep(models, each=23), quantile=rep(quantile_levels, n_models), param=params)
#   
#   return(params)
# }


### GQRA

get_quantile_groups <- function(){
  g1 <- c(0.01, 0.025, 0.05, 0.1)
  g2 <- c(0.15, 0.2, 0.25, 0.3)
  g3 <- c(0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65)
  g4 <- c(0.7, 0.75, 0.8, 0.85)
  g5 <- c(0.9, 0.95, 0.975, 0.99)
  
  groups <- data.frame(quantile=c(g1, g2, g3, g4, g5), 
                       quantile_group=c(rep("g1", length(g1)), rep("g2", length(g2)), 
                                        rep("g3", length(g3)), rep("g4", length(g4)), 
                                        rep("g5", length(g5))))
  return(groups)
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

# params: data.frame with columns model, quantile_group, param
GQRA_3 <- function(df, groups, params){
  df_temp <- merge(df, groups, by.x = c("quantile"), by.y = c("quantile"))
  df_temp <- merge(df_temp, params, by.x = c("model", "quantile_group"), 
                   by.y = c("model", "quantile_group"))  
  gqra3 <- data.frame(
    df_temp %>%
      mutate(weighted_values = value * param) %>%
      group_by(target_end_date, location, target, quantile, truth) %>% 
      summarize(value = sum(weighted_values), .groups="keep"))
  return(gqra3)
}

gqra3_loss <- function(df, groups, params){
  params <- data.frame(model=rep(unique(df$model), each=5), 
                       quantile_group=rep(unique(groups$quantile_group), 5), 
                       param=params)
  df_temp <- GQRA_3(df, groups, params)
  return(mean_wis(df_temp))
}

gqra3_fit <- function(df, groups, method="BFGS"){
  n_models = length(unique(df$model))
  n_groups = length(unique(groups$quantile_group))
  
  p_optim <- optim(par = rep(1/(n_models*n_groups), n_models*n_groups), fn = function(x){
    return(gqra3_loss(df, groups, params = x))}, method=method)$par
  
  return(p_optim)
}

# params: data.frame with columns model, quantile_group, param
# intercepts: data.frame with columns: quantile_group, intercept
GQRA_4 <- function(df, groups, params, intercepts){
  df_temp <- merge(df, groups, by.x = c("quantile"), by.y = c("quantile"))
  df_temp <- merge(df_temp, params, by.x = c("model", "quantile_group"), 
                   by.y = c("model", "quantile_group"))  
  gqra4 <- data.frame(
    df_temp %>%
      mutate(weighted_values = value * param) %>%
      group_by(target_end_date, location, target, quantile_group, quantile, truth) %>% 
      summarize(value = sum(weighted_values), .groups="keep"))
  gqra4 <- merge(gqra4, intercepts, by.x = "quantile_group", by.y = "quantile_group") %>% 
    mutate(value = value + intercept) %>% 
    select(-c(intercept, quantile_group))
  return(gqra4)
}

gqra4_loss <- function(df, groups, params, intercepts){
  params <- data.frame(model = rep(models, each=5), 
                       quantile_group = rep(c("g1", "g2", "g3", "g4", "g5"), 5), 
                       param = params)
  intercepts <- data.frame(quantile_group = c("g1", "g2", "g3", "g4", "g5"), 
                       intercept = intercepts)
  df_temp <- GQRA_4(df, groups, params, intercepts)
  return(mean_wis(df_temp))
}

gqra4_fit <- function(df, groups, method="BFGS"){
  n_models = length(unique(df$model))
  n_groups = length(unique(groups$quantile_group))
  
  # the first n_groups entries are the values of the intercepts
  p_optim <- optim(par = c(rep(0, n_groups), rep(1/(n_models*n_groups), n_models*n_groups)), 
                   fn = function(x){
                     return(gqra4_loss(df, groups, params = x[(n_groups+1):length(x)], 
                                       intercepts = x[1:n_groups]))},
                   method = method)$par
  intercepts <- p_optim[1 : n_groups]
  p_optim <- p_optim[(n_groups+1) : length(p_optim)]
  return(list(params=p_optim, intercepts=intercepts))
}


### GQRA_2

GQRA_2 <- function(df, groups, params){
  models <- unique(df$model) 
  # missing model
  last_param <- models[!(models %in% unique(params$model))] 
  # infer parameters for each quantile group from given parameters 1-sum(given params)
  params <- rbind(params, params %>% 
                    group_by(quantile_group) %>% 
                    summarize(param=1-sum(param)) %>% 
                    mutate(model=last_param)) 
  
  df_temp <- merge(df, groups, by.x = c("quantile"), by.y = c("quantile"))
  df_temp <- merge(df_temp, params, by.x = c("model", "quantile_group"), 
                   by.y = c("model", "quantile_group")) 
  
  gqra2 <- data.frame(
    df_temp %>%
      mutate(weighted_values = value * param) %>%
      group_by(target_end_date, location, target, quantile, truth) %>% 
      summarize(value = sum(weighted_values), .groups="keep"))
  return(gqra2)

}

gqra2_loss <- function(df, groups, params){
  models <- unique(df$model)
  models <- models[-length(models)]  # drop last model
  n_models <- length(models)
  
  group_names <- unique(groups$quantile_group)
  n_groups <- length(group_names)
  
  params <- data.frame(model=rep(models, each=n_groups), 
                       quantile_group=rep(group_names, n_models), 
                       param=params)
  df_temp <- GQRA_2(df, groups, params)
  return(mean_wis(df_temp))
}



gqra2_fit <- function(df, groups){
  models <- unique(df$model)
  models <- models[-length(models)]  # drop last model
  n_models <- length(models)
  
  group_names <- unique(groups$quantile_group)
  n_groups <- length(group_names)
  
  # feasible region: ui %*% theta - ci >= 0
  r <- c(rep(0, n_models*n_groups), rep(-1, n_groups))
  R <- rbind(diag(n_models*n_groups))
  
  R_l <- matrix(0, nrow=n_groups, ncol=n_models*n_groups)
  for (i in 1:n_groups){
    for (j in 1:n_models){
      R_l[i, i + (j-1)*n_groups] <- - 1
    }
  }
  R <- rbind(R, R_l)
  

  p_optim <- constrOptim(theta = rep(1/(n_models*n_groups), n_models*n_groups), 
                   f = function(x){
                     return(gqra2_loss(df, groups, params = x))},
                   ui=R, ci=r, method="Nelder-Mead")$par
  return(p_optim)
}

# R_l <- matrix(0, nrow=n_groups, ncol=n_models*n_groups)
# for (i in 1:n_groups){
#  R_l[i, (1+(i-1)*4) : (1+(i-1)*4 + n_models -1)] <- -1
# }

# n_models <- 4
# n_groups <- 5
# r <- c(rep(0, n_models*n_groups), rep(-1, n_groups))
# R <- rbind(diag(n_models*n_groups))
# 
# R_l <- matrix(0, nrow=n_groups, ncol=n_models*n_groups)
# for (i in 1:n_groups){
#   for (j in 1:n_models){
#     R_l[i, i + (j-1)*n_groups] <- - 1
#   }
# }
# R <- rbind(R, R_l)
