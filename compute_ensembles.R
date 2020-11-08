setwd("/home/dwolffram/covid19-ensembles")

source("data_loading.R")
source("scoring.R")
source("ensemble_methods.R")
source("ensemble_functions.R")

library(doParallel)


models <- c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", "JHU_IDD-CovidSP",
            "LANL-GrowthRate", "MOBS-GLEAM_COVID", "PSI-DRAFT", "UCLA-SuEIR", 
            "UMass-MechBayes", "YYG-ParamSearch")

# DC,11,District of Columbia
# AS,60,American Samoa
# GU,66,Guam
# MP,69,Northern Mariana Islands
# PR,72,Puerto Rico
# UM,74,U.S. Minor Outlying Islands
# VI,78,Virgin Islands

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")

df <- load_forecasts(models=models, targets=c("1 wk ahead cum death"),
                     exclude_locations=exclude_locations, start_date="2020-05-23",
                     intersect_dates=TRUE)



no_cores <- detectCores() - 1  
no_cores <- 32
registerDoParallel(cores=no_cores)  

ensembles <- c("EWA", "MED", "V2", "V3", "V4", "QRA2", "QRA3", 
               "QRA4", "GQRA2", "GQRA3", "GQRA4")
window_sizes <- 1:4

dates <- as.Date(c("2020-07-25", "2020-08-01", "2020-08-08", "2020-08-15", "2020-08-22"))


df_ensembles <- ensemble_forecasts(df, dates, window_sizes=window_sizes, ensembles)

file_name <- paste0("data/ensemble_forecasts/df_ensembles_", Sys.Date(), ".csv")
write.csv(df_ensembles, file_name, row.names=FALSE)




# write.csv(results, "results/results_2020-08-09.csv", row.names=FALSE)


### EXCLUDE US FROM TRAINING

ensembles <- c("EWA", "MED", "V2", "V3", "V4", "QRA2", "QRA3", 
               "QRA4", "GQRA2", "GQRA3", "GQRA4")
window_sizes <- 1:4

#dates <- as.Date(c("2020-07-11", "2020-07-18", "2020-07-25", "2020-08-01", "2020-08-08"))

results2 <- evaluate_ensembles(df, possible_dates, window_sizes, ensembles, exclude_us_from_training=TRUE)

file_name <- paste0("results/results_", Sys.Date(), "_train_without_us.csv")
#file_name <- paste0("results/results_2020-08-08_train_without_us.csv")

write.csv(results, file_name, row.names=FALSE)




### FIX SORTING
gqra4_df <- subset(results, method=='GQRA4')
gqra4_df <- gqra4_df %>%
  select(-c(starts_with('wgt') | wis))
gqra4_df <- pivot_longer(gqra4_df, -c(target_end_date, location, target, truth, method, window_size),
                         names_to = 'quantile')

gqra4_df <- gqra4_df %>% 
  group_by(target_end_date, location, target, window_size) %>%
  mutate(value = sort(value)) %>%
  as.data.frame()

gqra4_df$quantile <- str_remove(gqra4_df$quantile, 'value.')
scores <- wis_table(gqra4_df)

scores <- scores %>% 
  select(-c(method, window_size), c(method, window_size))
  
  
results_new <- subset(results, method!='GQRA4')
results_new <- bind_rows(results_new, scores)

sum(results$wgt_iw_0.3 < 0, na.rm=TRUE)
sum(results_new$wgt_iw_0.3 < 0, na.rm=TRUE)

sum(results$wgt_iw < 0, na.rm=TRUE)

write.csv(results_new, 'results/results_2020-10-02_fixed.csv', row.names=FALSE)

