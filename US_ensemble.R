setwd("/home/wolffram/covid19-ensembles")

source("data_loading.R")
source("scoring.R")
source("ensemble_methods.R")
source("ensemble_functions.R")

forecast_date <- '2021-05-03'

df_train <- read_csv(paste0("data/ensemble_data/df_train_", forecast_date, ".csv"), col_types = cols(
              forecast_date = col_date(format = ""),
              target = col_character(),
              target_end_date = col_date(format = ""),
              location = col_character(),
              type = col_character(),
              quantile = col_double(),
              value = col_double()
              )) %>%
  # filter(location != 'US') %>%
  as.data.frame()


df_test <- read_csv(paste0("data/ensemble_data/df_test_", forecast_date, ".csv"), col_types = cols(
              forecast_date = col_date(format = ""),
              target = col_character(),
              target_end_date = col_date(format = ""),
              location = col_character(),
              type = col_character(),
              quantile = col_double(),
              value = col_double()
            )) %>%
  filter(location == 'US') %>%
  as.data.frame()

df_ensemble <- data.frame()

unique(df_test$forecast_date)

# for (t in c("1 wk ahead cum death", "1 wk ahead inc death", "2 wk ahead cum death", "2 wk ahead inc death", "3 wk ahead cum death",
#             "3 wk ahead inc death", "4 wk ahead cum death", "4 wk ahead inc death")){
  
for (t in unique(df_test$target)){
    
  print(t)
  train <- df_train %>%
    filter(target == t)
  test <- df_test %>%
    filter(target == t)
  
  train <- add_truth(train)
  test$truth <- 0
  
  
  p <- v3_iter_fit(train)
  print(p)
  ensemble <- V3(test, params=p$params, models=p$models)
  ensemble <- sort_quantiles(ensemble)
  df_ensemble <- bind_rows(df_ensemble, ensemble)
}

df_ensemble <- df_ensemble %>%
  select(-truth)

file_name <- paste0("data/ensemble_data/df_pred_", forecast_date, ".csv")
file_name <- paste0("data/ensemble_data/df_pred_US_", forecast_date, ".csv")

write.csv(df_ensemble, file_name, row.names=FALSE)

wis(train)

wis(df_train2)

row_index <- names(train)[!(names(train) %in% c("quantile", "value"))]
df_wide <- reshape(train, direction = "wide", timevar = "quantile",
                   v.names = "value", idvar = row_index)





# PLOT
Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_ALL", "C")

df <- read_csv("data/ensemble_data/df_pred_2021-05-03.csv")
df2 <- read_csv("data/ensemble_data/df_pred_US_2021-05-03.csv")
df <- bind_rows(df, df2)

df <- read_csv("data/ensemble_data/submissions/2021-04-26-KITmetricslab-select_ensemble.csv")
df <- read_csv("data/ensemble_data/submissions/2021-05-03-KITmetricslab-select_ensemble.csv")


plot_submission <- function(df, target="inc death", start_date="2020-11-01"){
  cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1]
  
  if (str_detect(target, 'cum death')){
    title <- 'Cumulative Deaths'
  } else if (str_detect(target, 'inc death')){
    title <- 'Incident Deaths'
  } else if (str_detect(target, 'cum case')){
    title <- 'Cumulative Cases'
  } else if (str_detect(target, 'inc case')){
    title <- 'Incident Cases'
  }
  
  df <- df %>%
    filter(str_detect(target, !!target)) %>%
    filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975))
  
  df <- pivot_wider(df, names_from=quantile, names_prefix="value.", values_from=value)
  
  exclude_locations <- c("60", "66", "69", "72", "74", "78")
  
  truth <- load_truth(title, min(df$target_end_date) - 5) %>%
    #filter(location != 'US' & !(location %in% exclude_locations)) %>%
    filter(!(location %in% exclude_locations)) %>%
    rename(target_end_date = date, truth = value)
  
  df <- bind_rows(truth, df)
  
  df <- df %>% 
    mutate(across(starts_with('value'), ~ coalesce(., truth))) %>%
    filter(target_end_date >= start_date)
  
  df <- add_location_names(df)
  

  
  ggplot(df, aes(x=target_end_date, y=value)) +
    facet_wrap('location_name', scales='free_y') +
    geom_smooth(aes(y = value.0.5, ymin = value.0.025, ymax = value.0.975), 
                linetype=3, size=0.2, colour="white", fill=cols[2], alpha=1, stat = "identity") +
    geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
                linetype=3, size=0.2, colour="white", fill=cols[1], alpha=1, stat = "identity") +
    geom_line(aes(y = truth)) +
    geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white", size=0.7) +
    labs(title=title, x=NULL, y=NULL) +
    theme_grey(base_size=14)
}

plot_submission(df, "inc death")
plot_submission(df, "cum death")

plot_submission(df, "inc case")


ggsave('data/ensemble_data/inc_death_2021-05-03.png', width=40, height=25, dpi=500, unit='cm', device='png')
ggsave('data/ensemble_data/cum_death_2021-05-03.png', width=40, height=25, dpi=500, unit='cm', device='png')
ggsave('data/ensemble_data/inc_case_2021-05-03.png', width=40, height=25, dpi=500, unit='cm', device='png')


### FORMAT SUBMISSION

df$type <- 'quantile'

df_point <- df %>%
  filter(quantile == 0.5) %>%
  mutate(type = 'point', quantile = NA)

df <- bind_rows(df, df_point)

df$forecast_date <- as.Date(forecast_date)

df$value <- pmax(df$value, 0)

df <- df %>%
  select(forecast_date, target, target_end_date, location, type, quantile, value)

write.csv(df, paste0("data/ensemble_data/submissions/", forecast_date, "-KITmetricslab-select_ensemble.csv"), row.names=FALSE)




truth <- load_truth('Incident Deaths')




df <- df %>%
  filter(str_detect(target, 'inc death')) %>%
  filter(quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975))

df <- pivot_wider(df, names_from=quantile, names_prefix="value.", values_from=value)


cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1] # length(levels_coverage) + 1

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")

truth <- load_truth("Incident Deaths", min(df$target_end_date) - 5) %>%
  filter(!(location %in% exclude_locations)) %>%
  rename(target_end_date = date, truth = value)



a <- bind_rows(truth, df)

a <- a %>% 
  mutate(across(starts_with('value'), ~ coalesce(., truth)))

a <- a %>%
  filter(target_end_date >= '2020-11-01')

a <- add_location_names(a)

truth <- load_truth('Incident Deaths')
truth <- add_location_names(truth)%>%
  filter(date == '2021-04-17' & !(location %in% exclude_locations))

ggplot(a, aes(x=target_end_date, y=value)) +
  facet_wrap('location_name', scales='free_y') +
  geom_smooth(aes(y = value.0.5, ymin = value.0.025, ymax = value.0.975), 
              linetype=3, size=0.2, colour="white", fill=cols[2], alpha=1, stat = "identity") +
  geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
              linetype=3, size=0.2, colour="white", fill=cols[1], alpha=1, stat = "identity") +
  geom_line(aes(y = truth)) +
  geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white", size=0.7) +
  labs(title='Incident Deaths', x=NULL, y=NULL) +
  geom_point(data = truth, aes(x=date, y=value), pch = 4, size=0.7) +
  facet_wrap('location_name', scales='free_y')

ggsave('data/ensemble_data/inc_death_obs1.png', width=40, height=25, dpi=500, unit='cm', device='png')


### Compare to individual models

df_individual <- load_forecasts(targets=c("1 wk ahead inc death"),
                     exclude_locations=exclude_locations, start_date="2021-04-17")

df_individual <- df_individual %>% 
  select(-c(forecast_date, type)) %>%
  select(target_end_date, everything())

df_individual <- df_individual %>%
  select(- location_name)

df <- df %>%
  filter(target == '1 wk ahead inc death')

df$model <- 'V3_iter'

df_all <- bind_rows(df, df_individual) %>%
  as.data.frame()

scores_all <- score_forecasts(df_all)

s <- scores_all %>%
  group_by(model) %>%
  summarize(meanWIS = mean(wis), meanAE = mean(ae))
