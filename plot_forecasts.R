setwd("/home/dwolffram/covid19-ensembles")

source("data_loading.R")
source("plot_functions.R")


df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_noUS.csv", 
                     add_baseline = TRUE, add_truth = TRUE, add_location_names = TRUE)

# df <- load_ensembles("data/ensemble_forecasts/df_ensembles_4wk.csv", add_baseline = TRUE)


plot_forecast(df, window_sizes=4, models=c('V4', 'QRA4'),
              locations=c('US', 36), incidence=TRUE,
              scales='free_y')

plot_forecast(df, window_sizes=4,
              locations=c('US', 36), incidence=TRUE,
              facet_row=location_name, facet_col=model,
              scales='free_y')

# plot_forecast(df, window_sizes=4, models=c('V4', 'QRA4'),
#               incidence=TRUE,
#               facet_row=location_name, facet_col=model,
#               scales='free_y')


plot_forecast(df, locations='US', window_sizes=4, facet=model, center=TRUE, 
              scales='free_y', ncol=3, dir='h')

plot_forecast(df, locations='US', facet=model)

plot_forecast(df, window_sizes=4, locations='US', models='QRA3', incidence=FALSE, ncol=4)

plot_forecast(df, locations='US', facet=model, incidence=TRUE, ncol=3, dir='h')

plot_forecast(df, window_sizes=4, models='V4', facet=location_name, incidence=TRUE, 
              ncol=8, dir='h', scales='free_y')

plot_forecast(df, window_sizes=4, locations=c('US', 36), models='QRA3', 
              facet=location, incidence=TRUE, ncol=4)




unique(df$target_end_date)


### INDIVIDUAL MODELS

df_individual <- load_forecasts(models = c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", 
                                           "JHU_IDD-CovidSP", "LANL-GrowthRate", "MOBS-GLEAM_COVID", 
                                           "PSI-DRAFT", "UCLA-SuEIR", "UMass-MechBayes", "YYG-ParamSearch"),
                                targets = "1 wk ahead cum death",
                                exclude_locations = c("11", "60", "66", "69", "72", "74", "78"), 
                                start_date = "2020-06-20", 
                                end_date = "2020-10-10",
                                add_truth = TRUE) %>% 
  select(-c(forecast_date, type)) %>%
  select(target_end_date, everything())


plot_forecast(df_individual, locations='US', facet=model, incidence=TRUE, ncol=4)

plot_forecast(df_individual, locations='US', facet=model, center=TRUE,
              ncol=4, dir='h', scales='free_y')

plot_forecast(df_individual, locations=27, facet=model, center=TRUE,
              ncol=4, dir='v', scales='free_y')

plot_forecast(df_individual, models='YYG-ParamSearch', facet=location_name, incidence=TRUE, 
              ncol=8, dir='h', scales='free_y')

plot_forecast(df_individual, models='UCLA-SuEIR', facet=location_name, incidence=TRUE, 
              ncol=8, dir='h', scales='free_y')

plot_forecast(df_individual, locations='US', facet=model, incidence=FALSE, ncol=4)





plot_forecast(data, incidence=TRUE, 
              title="YYG-ParamSearch forecasts with 50%- and 90%-prediction intervals")
ggsave('plots/individual_models/individual_incidence_us.png', width=11, height=7, dpi=500, unit='cm', device='png')


