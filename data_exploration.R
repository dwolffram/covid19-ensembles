source("data_loading.R")

library(ggplot2)

df <- load_forecasts()

# we need to check if there are missing forecasts over time, we only consider models without gaps
max_diff_target_end_date <- df %>%
  #filter(target_end_date >= "2020-05-30") %>%
  group_by(target, model) %>%
  summarize(max_diff_target_end_date = max(diff.Date(target_end_date)))

# next we check how many forecasts are available for all models without gaps
model_availability <- df %>%
  group_by(target, model) %>%
  filter(max(diff.Date(target_end_date)) == 7) %>%
  summarize(count = length(unique(target_end_date)), 
            start = min(target_end_date), end = max(target_end_date))

relevant_models <- model_availability %>%
  filter(count > 20)

model_locations <- df %>% 
  group_by(target, model, target_end_date) %>%
  summarize(locations = length(unique(location)))

all_states_available <- df %>% 
  group_by(target, model, target_end_date) %>%
  summarize(locations = length(unique(location))) %>%
  group_by(model) %>%
  filter(min(locations) >= 50)


plot_availability <- function(df, target="1 wk ahead cum death", exclude_gaps=FALSE){
  # pick target
  temp <- df %>%
    filter(target == !!target)
  
  # exclude models with missing forecasts over time
  if (exclude_gaps==TRUE){
    temp <- temp %>%
      group_by(model) %>%
      filter(max(diff.Date(target_end_date)) == 7)
  }
  
  # all states available
  temp <- temp %>% 
    group_by(model, target_end_date) %>%
    summarize(locations = length(unique(location))) %>%
    group_by(model) %>%
    filter(min(locations) >= 50)

  # mark missing forecasts
  temp <- temp %>% 
    group_by(model) %>%
    complete(target_end_date = seq.Date(min(target_end_date), max(target_end_date), by = "week"))
  temp$forecast_missing <- 1*is.na(temp$locations)
  
  # sort by number of available forecasts per model
  temp <- temp %>%
    group_by(model) %>%
    mutate(count=sum(!forecast_missing))
  
  temp$model <-  factor(temp$model, levels = unique(temp$model[order(temp$count)]))
  
  ggplot(temp, aes(target_end_date, model, fill= factor(forecast_missing))) + 
    geom_tile() +
    scale_fill_manual(values=c("1"="red", "0"="darkgreen"), 
                      name = "Forecast Available", labels = c("Yes", "No")) +
    labs(x="Target End Date", y="Model", title=target)
}

plot_availability(df, target = "1 wk ahead cum death")
plot_availability(df, target = "1 wk ahead cum death", exclude_gaps = TRUE)
plot_availability(df, target="4 wk ahead cum death")
plot_availability(df, target="4 wk ahead cum death", exclude_gaps = TRUE)
