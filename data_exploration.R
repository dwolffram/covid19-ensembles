setwd("/home/dwolffram/covid19-ensembles")
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
  filter(min(locations) >= 30)

get_available_models <- function(df, target="1 wk ahead cum death", exclude_gaps=FALSE){
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
  
  return(temp)
}

available_models <- get_available_models(df, exclude_gaps=TRUE)

available_models %>%
  summarize(count = length(unique(target_end_date)), 
            start = min(target_end_date), end = max(target_end_date))
relevant_models <- available_models %>%
  filter(count > 20)

a <- available_models %>%
  group_by(target_end_date) %>%
  summarize(count = length(unique(model))) 

temp <- df %>%
  filter(target == "1 wk ahead cum death")

# all states available
temp <- temp %>% 
  group_by(model, target_end_date) %>%
  summarize(locations = length(unique(location)))

temp <- temp %>%
  filter(locations >= 50)

temp <- temp %>%
  group_by(model) %>%
  summarize(count = length(unique(target_end_date)), 
            start = min(target_end_date), end = max(target_end_date))

temp <- temp %>% 
  group_by(model) %>%
  complete(target_end_date = seq.Date(min(target_end_date), max(target_end_date), by = "week"))
temp$forecast_missing <- -1*(temp$locations < 50)
temp <- temp %>% replace_na(list(forecast_missing = 1))

temp$forecast_missing <- 1*is.na(temp$locations)

plot_availability <- function(df, target="1 wk ahead cum death", exclude_gaps=FALSE, 
                              min_no_locations=50, drop_incomplete=FALSE){
  # pick target
  temp <- df %>%
    filter(target == !!target)
  
  # number of locations
  temp <- temp %>% 
    group_by(model, target_end_date) %>%
    summarize(locations = length(unique(location))) 
  
  if (drop_incomplete){
    temp <- temp %>%
      filter(locations >= min_no_locations | is.na(locations))
  }
  
  # fill dates
  temp <- temp %>% 
    group_by(model) %>%
    complete(target_end_date = seq.Date(min(target_end_date), max(target_end_date), by = "week"))
  
  # mark missing forecasts
  if (drop_incomplete){
    temp$forecast_missing <- 1*is.na(temp$locations)
  }
  
  # mark forecasts with too few location or that are missing
  else{
    temp$forecast_missing <- -1*(temp$locations < min_no_locations)
    temp <- temp %>% replace_na(list(forecast_missing = 1))
  }
  
  # exclude models with missing forecasts over time
  if (exclude_gaps==TRUE){
    temp <- temp %>%
      group_by(model) %>%
      filter(all(forecast_missing == 0))
  }
  
  # sort by number of available forecasts per model
  temp <- temp %>%
    group_by(model) %>%
    mutate(count=sum(forecast_missing != 1))
  
  temp$model <-  factor(temp$model, levels = unique(temp$model[order(temp$count)]))
  temp$forecast_missing <-  factor(temp$forecast_missing, levels = c(0, 1, -1))
  
  plot(ggplot(temp, aes(target_end_date, model, fill= factor(forecast_missing))) + 
      geom_tile(colour = "grey50") +
      scale_fill_manual(values=c("1"="red", "0"="darkgreen", "-1"="orange"), 
                        name = element_blank(), labels = c("Available", "Missing", "< 50 Locations")) +
      scale_x_date(breaks = seq(min(temp$target_end_date), max(temp$target_end_date), by="week")[c(FALSE, TRUE)]) +
      labs(x="Target End Date", y="Model", title= paste0("Forecast Availability (", target, ")")))
}

plot_availability(df, target = "1 wk ahead cum death")
plot_availability(df, target = "1 wk ahead cum death", exclude_gaps = TRUE)
plot_availability(df, target = "1 wk ahead cum death", drop_incomplete = TRUE)
plot_availability(df, target = "1 wk ahead cum death", drop_incomplete = TRUE, exclude_gaps = TRUE)

plot_availability(df, target="4 wk ahead cum death")
plot_availability(df, target="4 wk ahead cum death", exclude_gaps = TRUE)
