setwd("/home/dwolffram/covid19-ensembles")

path_hub <- "../covid19-forecast-hub/"


library(tidyverse)
library(dplyr)
library(readr)

get_all_models <- function(path_hub="../covid19-forecast-hub/"){
  all_models <- list.dirs(path = file.path(path_hub, 'data-processed'), 
                          full.names = FALSE, recursive = FALSE)
  return(all_models)
}

# all_models <- get_all_models()

get_filenames <- function(model){
  model_files <- list.files(path=paste0(path_hub, "data-processed/", model), 
                            pattern=".csv$", full.names = TRUE)
  return(model_files)
}

# get_filenames('YYG-ParamSearch')

load_forecasts <- function(models='all', exclude_locations=c(), targets=paste(1:4, "wk ahead cum death")){
  
  if(models == 'all'){
    models <- get_all_models()
  }
  
  df <- data.frame()
  
  for (m in models){
    print(m)
    
    # all files in model folder
    model_files <- get_filenames(m)
    
    if (length(model_files) == 0) {
      print(paste0("No forecast files found for: ", m, "."))
      next
    }
    
    df_temp <- model_files %>% 
      lapply(read_csv, 
             col_types = cols(
               forecast_date = col_date(format = ""),
               target = col_character(),
               target_end_date = col_date(format = ""),
               location = col_character(),
               type = col_character(),
               quantile = col_double(),
               value = col_double())) %>%
      bind_rows
    

    # filter forecasts
    df_temp <- df_temp %>%
      filter(target %in% targets,
             !(location %in% exclude_locations),
             type == 'quantile')
    
    if (nrow(df_temp) == 0) {
      print(paste0("No relevant forecasts available for: ", m))
      next
    }    
    
    # remove multiple forecasts for same target (only keep newest one)
    df_temp <- df_temp %>%
      group_by(target_end_date, location, target, quantile) %>%
      slice(which.max(forecast_date))%>%
      as.data.frame()
    
    df_temp$model <- m
    df <- bind_rows(df, df_temp)
  }
  
  return(df)
}

#a <- load_forecasts(c("YYG-ParamSearch", "IBF-TimeSeries"))
#df <- load_forecasts()

load_truth <- function(as_of=''){
  if(as_of==''){
    truth <- read.csv(paste0(path_hub, "data-truth/truth-Cumulative Deaths.csv"),
             colClasses = c(location="character", date ="Date"))
  }
  else{
    truth <- read.csv(paste0("data/jhu_historic_deaths/processed/truth_jhu_deaths_as_of_",
                    as_of, ".csv"), 
             colClasses = c(location="character", date ="Date"))
  }
}

