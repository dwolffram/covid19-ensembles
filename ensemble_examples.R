setwd("/home/dwolffram/covid19-ensembles")

source("data_loading.R")
source("ensemble_methods.R")
source("ensemble_functions.R")
source("scoring.R")

library(quantreg)

models <- c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", "JHU_IDD-CovidSP",
            "LANL-GrowthRate", "MOBS-GLEAM_COVID", "PSI-DRAFT", "UCLA-SuEIR", 
            "UMass-MechBayes", "YYG-ParamSearch")

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")

df <- load_forecasts(models=models, targets=c("1 wk ahead cum death"),
                     exclude_locations=exclude_locations, start_date="2020-05-23",
                     intersect_dates=TRUE)

dfs <- train_test_split(df, test_date=as.Date("2020-08-22"), horizon=1, window_size=4)
df_train <- dfs$df_train
df_test <- dfs$df_test


### EWA
ewa_df <- EWA(df_test)

ewa_scores <- wis(ewa_df)
mean(ewa_scores$wis)
mean_wis(ewa_df)

ewa_loss(df_test)

# ewa_df <- aggregate(formula = value ~ target + target_end_date + location + type + quantile,
#                       data = df_test, FUN = mean)

### MED
med_df <- MED(df_test)
mean_wis(med_df)

med_scores <- wis(med_df)

med_loss(df_test)

#colMeans(med_scores[c("wgt_iw", "wgt_pen_u", "wgt_pen_l", "wis")])

# for (e in c(EWA, MED)){
#   print(deparse(quote(e)))
#   a <- e(df_test)
#   print(mean_wis(a))
# }


### INV
params <- inv_fit(df_train)
inv_df <- INV(df_test, params)

mean_wis(inv_df)

s <- params %>% 
  group_by(location) %>%
  summarize(sum=sum(param))


### V3
v3_df <- V3(df_test, params=rep(0.1, 10))
v3_scores <- wis(v3_df)
mean(v3_scores$wis)

mean_wis(v3_df)
v3_loss(df_test, rep(0.2, 5))

# Estimation
p_v3 <- v3_fit(df_train)
v3_loss(df_test, p_v3)


### V4

# Set an intercept in V3
v4_df <- V3(df_test, params=rep(0.1, 10), intercept=0)
mean_wis(v4_df)
v3_loss(df_test, rep(0.1, 10), intercept=2)

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

params <- data.frame(model=rep(models, each=23), quantile=quantile_levels, param=rep(0.1, 10*23))

qra3_df <- QRA3(df_test, params=params)

#qra3_df[qra3_df$value == sort_quantiles((qra3_df))$value, ]
qra3_loss(df_test, params)

p_qra3 <- qra3_fit(df_train)
qra3_loss(df_test, p_qra3)
qra3_df <- QRA3(df_test, params=p_qra3)


### QRA4

intercepts <- data.frame(quantile=quantile_levels, intercept=rep(0, 23))

qra4_df <- QRA4(df_test, params=params, intercepts=intercepts)
qra4_loss(df_test, params, intercepts)

p_qra4 <- qra4_fit(df_train)
qra4_loss(df_test, p_qra4$params, p_qra4$intercepts)

qra4_df <- QRA4(df_test, p_qra4$params, p_qra4$intercepts)


### QRA2

params2 <- params %>% filter(model!=tail(models, 1))
qra2_df <- QRA2(df_test, params2)
qra2_loss(df_test, params2)


mean(L(0.5, subset(qra2_df, quantile==0.5)$value, subset(qra2_df, quantile==0.5)$truth))
fn(0.5, df_test, params2$param)

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
gqra53_df <- GQRA_3(df_test, groups, p_gqra3)


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
gqra54_df <- GQRA_4(df_test, groups, p_gqra4$params, p_gqra4$intercept)



### GQRA_2
params2 <- params %>% filter(model!=tail(models, 1))

gqra2_df <- GQRA_2(df_test, groups, params2)
mean_wis(gqra2_df)

gqra2_loss(df_test, groups, params2$param)

p_gqra2 <- gqra2_fit(df_train, groups)
gqra3_loss(df_test, groups, p_gqra2)

gqra2_df <- GQRA_3(df_test, groups,p_gqra2)
mean_wis(gqra2_df)

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

# SORT QUANTILE CROSSINGS

ewa_df <- EWA(df_test)

ewa_crossed <- ewa_df %>% 
  group_by(target_end_date, location, target) %>%
  mutate(value = value[sample(row_number())])

ewa_crossed <- ewa_crossed %>% 
  group_by(target_end_date, location, target) %>%
  mutate(quantile = quantile[sample(row_number())])

ewa_sorted <- ewa_crossed %>% 
  group_by(target_end_date, location, target) %>%
  mutate(quantile = sort(quantile), value = sort(value))

a <- subset(ewa_df, target_end_date=='2020-05-30' & location=='01')
b <- subset(ewa_crossed, target_end_date=='2020-05-30' & location=='01')
c <- subset(ewa_sorted, target_end_date=='2020-05-30' & location=='01')

ewa_df[ewa_df$value != ewa_crossed$value, ]
ewa_df[ewa_df$value != ewa_sorted$value, ]



sort_quantiles <- function(df_forecast){
  return(
    df_forecast %>% 
      group_by(target_end_date, location, target) %>%
      mutate(value = sort(value)) %>%
      as.data.frame()
  )
}
b
d <- sort_quantiles(b)
class(d)
