source("data_loading.R")
source("scoring.R")
source("ensemble_methods.R")
source("ensemble_functions.R")



df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_2020-11-13.csv", add_baseline = TRUE)

df_wide <- pivot_wider(df, names_from=quantile, names_prefix="value.", values_from=value)
df_wide <- add_truth(df_wide)
df_wide <- add_location_names(df_wide)


plot_forecast <- function(data, models, locations, window_sizes,
                          facet, facet_row=location_name, facet_col=model,
                          incidence=FALSE, center=FALSE, title=NULL,
                          start_date='1900-01-01', end_date='3000-01-01',
                          ncol=4, dir='v', scales='fixed'){
  cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1] # length(levels_coverage) + 1
  facet <- ensym(facet)
  
  if(missing(models)){
    models <- unique(data$model)
  }  
  
  if(missing(locations)){
    locations <- unique(data$location)
  }
  
  if(missing(window_sizes) & 'window_size' %in% colnames(data)){
    window_sizes <- unique(data$window_size)
  }
  
  # filter forecasts
  data <- data %>%
    filter(model %in% models,
           location %in%  locations,
           target_end_date >= start_date,
           target_end_date <= end_date
    )
  
  if('window_size'  %in% colnames(data)){
    data <- data %>%
      filter(window_size %in% window_sizes)
  }
  
  if (incidence){
    if (missing(facet)){
      grouping_vars <- quos(model, location)
    }
    else{
      grouping_vars <- facet
    }
    
    horizon = as.numeric(substr(unique(data$target), 1, 1))
    
    data <- data %>% 
      group_by(!!!grouping_vars) %>%
      mutate(value.0.5 = value.0.5 - lag(truth, horizon)) %>%
      mutate(value.0.05 = value.0.05 - lag(truth, horizon)) %>%
      mutate(value.0.95 = value.0.95 - lag(truth, horizon)) %>%
      mutate(value.0.25 = value.0.25 - lag(truth, horizon)) %>%
      mutate(value.0.75 = value.0.75 - lag(truth, horizon)) %>%
      mutate(truth=c(NA, diff(truth))) %>%
      drop_na()
  }
  
  if (center){
    if (missing(facet)){
      grouping_vars <- quos(model, location)
    }
    else{
      grouping_vars <- facet
    }
    
    data <- data %>% 
      group_by(!!!grouping_vars) %>%
      mutate(value.0.5 = value.0.5 - truth) %>%
      mutate(value.0.05 = value.0.05 - truth) %>%
      mutate(value.0.95 = value.0.95 - truth) %>%
      mutate(value.0.25 = value.0.25 - truth) %>%
      mutate(value.0.75 = value.0.75 - truth) %>%
      mutate(truth=0) %>%
      drop_na()
  }
  
  # default title
  if(missing(title)){
    horizon = substr(unique(data$target), 1, 1)
    title = paste(horizon, "wk ahead forecasts with 50% and 90% prediction intervals")
    }
  
  ggplot(data, aes(x=target_end_date, y=truth)) +
    {if(!missing(facet)) facet_wrap(facet, ncol=ncol, dir=dir, scales=scales)} +
    {if(missing(facet)) facet_grid(rows=enquos(facet_row), cols=enquos(facet_col), scales=scales)} +
    geom_smooth(aes(y = value.0.5, ymin = value.0.05, ymax = value.0.95), 
                linetype=2, size=0.5, fill=cols[2], alpha=1, stat = "identity") +
    geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
                linetype=2, size=0.5, fill=cols[1], alpha=1, stat = "identity") +
    geom_line() +
    geom_point() +
    geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white") +
    theme_bw() +
    #theme(axis.text.x=element_text(angle=45,hjust=1)) +
    labs(title=title,
         x = "Date",
         y = "Deaths") +
     theme_grey(base_size=8)+
     theme(plot.title= element_text(size=9),
           axis.text = element_text(size = 5)) 
}

plot_forecast(df_wide, window_sizes=4, models=c('V4', 'QRA4'),
              locations=c('US', 36), incidence=TRUE,
              scales='free_y')

plot_forecast(df_wide, window_sizes=4,
              locations=c('US', 36), incidence=TRUE,
              facet_row=location_name, facet_col=model,
              scales='free_y')

# plot_forecast(df_wide, window_sizes=4, models=c('V4', 'QRA4'),
#               incidence=TRUE,
#               facet_row=location_name, facet_col=model,
#               scales='free_y')



plot_forecast(df_wide, locations='US', facet=model)
plot_forecast(df_wide, window_sizes=4, locations='US', models='QRA3', incidence=FALSE, ncol=4)
plot_forecast(df_wide, window_sizes=4, locations='US', facet=model, incidence=TRUE, ncol=4)

plot_forecast(df_wide, window_sizes=4, models='V4', facet=location_name, incidence=TRUE, 
              ncol=8, dir='h', scales='free_y')

plot_forecast(df_wide, window_sizes=4, locations=c('US', 36), models='QRA3', 
              facet=location, incidence=TRUE, ncol=4)









### INDIVIDUAL MODELS

df_individual <- load_forecasts(models = c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", 
                                           "JHU_IDD-CovidSP", "LANL-GrowthRate", "MOBS-GLEAM_COVID", 
                                           "PSI-DRAFT", "UCLA-SuEIR", "UMass-MechBayes", "YYG-ParamSearch"),
                                targets = "4 wk ahead cum death",
                                exclude_locations = c("11", "60", "66", "69", "72", "74", "78"), 
                                start_date = "2020-06-20", 
                                end_date = "2020-10-10") %>% 
  select(-c(forecast_date, type)) %>%
  select(target_end_date, everything())

df_individual <- pivot_wider(df_individual, names_from=quantile, names_prefix="value.", values_from=value)
df_individual <- add_truth(df_individual)
df_individual <- add_location_names(df_individual)

plot_forecast(df_individual, locations='US', facet=model, incidence=TRUE, ncol=4)

plot_forecast(df_individual, locations='US', facet=model, center=TRUE,
              ncol=4, dir='h', scales='free_y')

plot_forecast(df_wide, window_sizes=4, locations='US', facet=model, center=TRUE,
              ncol=4, dir='v', scales='free_y')

plot_forecast(df_wide, window_sizes=4, locations=27, facet=model, center=TRUE,
              ncol=4, dir='v', scales='free_y')

plot_forecast(df_individual, locations=27, facet=model, center=TRUE,
              ncol=4, dir='v', scales='free_y')

plot_forecast(df_individual, models='YYG-ParamSearch', facet=location_name, incidence=TRUE, 
              ncol=8, dir='h', scales='free_y')

plot_forecast(df_individual, models='UCLA-SuEIR', facet=location_name, incidence=TRUE, 
              ncol=8, dir='h', scales='free_y')

plot_forecast(df_individual, locations='US', facet=model, incidence=FALSE, ncol=4)


unique(df_individual$model)






plot_forecast(data, incidence=TRUE, 
              title="YYG-ParamSearch forecasts with 50%- and 90%-prediction intervals")
ggsave('plots/individual_models/individual_incidence_us.png', width=11, height=7, dpi=500, unit='cm', device='png')


