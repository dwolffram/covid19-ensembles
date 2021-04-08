setwd("/home/wolffram/covid19-ensembles")
source("data_loading.R")
library(ggplot2)

exclude_locations <- c("11", "60", "66", "69", "72", "74", "78")


df <- load_forecasts(exclude_locations=exclude_locations)


df <- load_forecasts(targets = c("1 wk ahead cum death"), exclude_locations=exclude_locations)
df <- load_forecasts(targets = c("4 wk ahead cum death"), exclude_locations=exclude_locations)
df <- load_forecasts(targets = c("1 wk ahead inc case"), exclude_locations=exclude_locations)
df <- load_forecasts(targets = c("1 wk ahead inc death"), exclude_locations=exclude_locations)
df <- load_forecasts(targets = c("4 wk ahead inc death"), exclude_locations=exclude_locations)



get_available_models <- function(df, target="1 wk ahead cum death", exclude_gaps=FALSE, 
                                 min_no_submission=3, min_no_locations=51, drop_incomplete=FALSE){
  # pick target
  temp <- df %>%
    filter(target == !!target)
  
  temp <- temp %>%
    group_by(model) %>%
    mutate(n = n_distinct(target_end_date)) %>%
    filter(n >= min_no_submission)%>%
    select(-n)
  
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
      filter(all(forecast_missing != 1))
  }
  
  return(temp)
}


plot_availability <- function(df, target="1 wk ahead cum death", exclude_gaps=FALSE, 
                              min_no_locations=51, drop_incomplete=FALSE, title){
  
  temp <- get_available_models(df, target=target, exclude_gaps=exclude_gaps, 
                                   min_no_locations=min_no_locations, drop_incomplete=drop_incomplete)
  
  # sort by number of available forecasts per model
  temp <- temp %>%
    group_by(model) %>%
    mutate(count=sum(forecast_missing != 1))
  
  temp$model <-  factor(temp$model, levels = unique(temp$model[order(temp$count)]))
  temp$forecast_missing <-  factor(temp$forecast_missing, levels = c(0, 1, -1))
  
  if(missing(title)){
    title= paste0("Forecast Availability (", target, ")")
  }
  
  plot(ggplot(temp, aes(target_end_date, model, fill= factor(forecast_missing))) + 
      geom_tile(colour = "grey50") +
      scale_fill_manual(values=c("1"="red", "0"="darkgreen", "-1"="orange"), 
                        name = element_blank(), labels = c("Available", "Missing", "< 51 Locations")) +
      scale_x_date(breaks = seq(min(temp$target_end_date), max(temp$target_end_date), by="week")[c(FALSE, TRUE)]) +
      labs(x="Target End Date", y="Model", title=title))+
      theme_gray(base_size=10) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
}

plot_availability(df, target = "1 wk ahead cum death", drop_incomplete=TRUE, exclude_gaps = TRUE, min_no_locations = 51)


plot_availability(df, target = "1 wk ahead cum death", title=NULL)
plot_availability(df, target = "1 wk ahead cum death", exclude_gaps = TRUE)
plot_availability(df, target = "1 wk ahead cum death", drop_incomplete = TRUE)
plot_availability(df, target = "1 wk ahead cum death", drop_incomplete = TRUE, exclude_gaps = TRUE, title=NULL)

plot_availability(df, target="4 wk ahead cum death", min_no_locations = 51)
plot_availability(df, target="4 wk ahead cum death", drop_incomplete = TRUE, exclude_gaps = TRUE)

plot_availability(df, target = "1 wk ahead inc death")
plot_availability(df, target = "1 wk ahead inc death", exclude_gaps = TRUE)
plot_availability(df, target = "1 wk ahead inc death", drop_incomplete = TRUE, exclude_gaps = TRUE)

plot_availability(df, target = "4 wk ahead inc death")
plot_availability(df, target = "4 wk ahead inc death", exclude_gaps = TRUE)
plot_availability(df, target = "4 wk ahead inc death", drop_incomplete = TRUE, exclude_gaps = TRUE)

ggsave('plots/model_availability.png', width=15.5, height=19, dpi=500, unit='cm', device='png')
ggsave('plots/model_availability_filtered.png', width=15.5, height=12, dpi=500, unit='cm', device='png')



temp <- df %>% 
  group_by(model, target_end_date) %>%
  summarize(locations = length(unique(location))) 

a <- df %>%
  filter(model == "RobertWalraven-ESG" & target_end_date == "2020-08-01") %>%
  summarize(locations = (unique(location)))

b <- df %>%
  filter(model == "UCLA-SuEIR" & target_end_date == "2020-08-01") %>%
  summarize(locations = (unique(location)))


a <- df %>%
  group_by(model) %>%
  summarize(n = n_distinct(target_end_date))

b <- df %>%
  group_by(model) %>%
  mutate(n = n_distinct(target_end_date)) %>%
  filter(n>2) %>%
  select(-n)%>%
  group_by(model) %>%
  summarize(n = n_distinct(target_end_date))

available_models <- get_available_models(df, target="1 wk ahead cum death", 
                                         drop_incomplete=TRUE, exclude_gaps=TRUE)

available_models <- get_available_models(df, target="4 wk ahead cum death", 
                                         drop_incomplete=TRUE, exclude_gaps=TRUE, min_no_locations=51)

available_models <- get_available_models(df, target="4 wk ahead cum death", 
                                         drop_incomplete=FALSE, exclude_gaps=FALSE, min_no_locations=51)

available_models <- get_available_models(df, target="1 wk ahead inc death", 
                                         drop_incomplete=TRUE, exclude_gaps=TRUE, min_no_locations=51)

available_models <- get_available_models(df, target="4 wk ahead inc death", 
                                         drop_incomplete=TRUE, exclude_gaps=TRUE, min_no_locations=51)

relevant_models <- available_models %>%
  summarize(count = length(unique(target_end_date)), 
            start = min(target_end_date), end = max(target_end_date))

relevant_models <- relevant_models %>%
  filter(start <= "2020-05-23") %>%
  pull(model)

# 4wk cum death
relevant_models <- relevant_models %>%
  filter(start <= "2020-09-05" & end > '2021-01-10' & end < '2021-04-03') %>%
  pull(model)

# 1wk inc death
relevant_models <- relevant_models %>%
  filter(start <= "2020-10-03" & end > '2021-01-10') %>%
  pull(model)

# 4wk inc death
relevant_models <- relevant_models %>%
  filter(start <= "2020-10-24" & end > '2021-01-10') %>%
  pull(model)

relevant_models
dput(relevant_models)

# "CovidAnalytics-DELPHI" "COVIDhub-baseline"     "CU-scenario_high"      "CU-select"            
# [5] "Karlen-pypm"           "LANL-GrowthRate"       "LNQ-ens1"              "MOBS-GLEAM_COVID"     
# [9] "OliverWyman-Navigator" "PSI-DRAFT"             "UA-EpiCovDA"           "UCSD_NEU-DeepGLEAM"   
# [13] "UMass-MechBayes" 

models_4wk_cum_death <- c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", "Karlen-pypm", "LANL-GrowthRate", "LNQ-ens1",
"MOBS-GLEAM_COVID", "OliverWyman-Navigator", "PSI-DRAFT", "UA-EpiCovDA", "UCSD_NEU-DeepGLEAM", "UMass-MechBayes") 
# < 2021-04-03



relevant_models
# [1] "CovidAnalytics-DELPHI" "COVIDhub-baseline"     "CU-scenario_high"      "CU-select"            
# [5] "JHU_IDD-CovidSP"       "LANL-GrowthRate"       "MOBS-GLEAM_COVID"      "PSI-DRAFT"            
# [9] "UCLA-SuEIR"            "UMass-MechBayes"       "YYG-ParamSearch"


relevant_models <- relevant_models %>%
  filter(start <= "2020-08-01" & end > '2020-10-10') %>%
  pull(model)

# [1] "CovidAnalytics-DELPHI" "COVIDhub-baseline"     "CU-scenario_high"      "CU-select"             "DDS-NBDS"             
# [6] "JHU_IDD-CovidSP"       "Karlen-pypm"           "LANL-GrowthRate"       "MOBS-GLEAM_COVID"      "NotreDame-mobility"   
# [11] "OliverWyman-Navigator" "PSI-DRAFT"             "RobertWalraven-ESG"    "UA-EpiCovDA"           "UCLA-SuEIR"           
# [16] "UMass-MechBayes" 

ggsave('plots/model_availability_filtered_new.png', width=15.5, height=12, dpi=500, unit='cm', device='png')



