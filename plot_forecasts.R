setwd("/home/wolffram/covid19-ensembles")

source("data_loading.R")
source("plot_functions.R")

df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_noUS.csv", 
                     add_baseline = TRUE, add_truth = TRUE, add_location_names = TRUE)

# df <- load_ensembles("data/ensemble_forecasts/df_ensembles_4wk.csv", add_baseline = TRUE)


plot_forecast(df, window_sizes=4, models=c('V4', 'QRA4'),
              locations=c('US', 36), incidence=TRUE,
              scales='free_y', base_size=12)

plot_forecast(df, window_sizes=4, models=c('EWA', 'QRA2', 'Baseline'),
              locations=c(34, 36, 48), incidence = FALSE, end_date = "2020-08-20",
              scales='free_y', base_size=10, title=NULL, ylab="Cumulative Deaths")

ggsave('plots/forecasts/revision_forecasts.png', width=15.5, height=10, dpi=500, unit='cm', device='png')


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



Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_ALL", "C")


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

plot_forecast(df_individual, models='YYG-ParamSearch', facet=location_name, incidence=TRUE, 
              ncol=8, dir='h', scales='free_y')

plot_forecast(df_individual, locations='US', facet=model, incidence=FALSE, ncol=4)


df %>%
  filter(location_name %in% c("Alaska", "California", "Georgia", "Iowa", "Mississippi", "Missouri", "New Hampshire",
                              "New York", "Vermont", "Florida")) %>%
  distinct(location)

locs <- c("02", "06", "13", "19", "28", "29", "33", "36", "50")
locs <- c("02", "06", "13", "19")



locs <- c("06", "29", "28", "12", "19")

df_individual$model <- factor(df_individual$model, levels = c('COVIDhub-baseline', "CU-select", "CovidAnalytics-DELPHI", "JHU_IDD-CovidSP", 
                                        "LANL-GrowthRate", "MOBS-GLEAM_COVID", "PSI-DRAFT", "UCLA-SuEIR", 
                                        "UMass-MechBayes", "YYG-ParamSearch"),
                   labels = c('Baseline', "CU", "DELPHI",  "JHU_IDD", 
                              "LANL", "MOBS", "PSI", "UCLA", 
                              "UMass", "YYG")) 

m <- levels(df_individual$model)

plot_forecast(df_individual, locations=locs, scales="free_y", incidence=TRUE)

plot_forecast(df_individual, locations=locs, models=m[1:5], scales="free_y", incidence=TRUE, ncol=5, base_size = 10, 
              title=NULL)
ggsave('plots/individual_incidence_us1.png', width=15.7, height=19, dpi=600, unit='cm', device='png')


plot_forecast(df_individual, locations=locs, models=m[6:10], scales="free_y", incidence=TRUE, ncol=5, base_size = 10,
              title=NULL)

ggsave('plots/individual_incidence_us2.png', width=15.7, height=19, dpi=600, unit='cm', device='png')




### Ensembles

df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_noUS.csv", 
                     add_baseline = TRUE, add_truth = TRUE, add_location_names = TRUE)

df$model <- factor(df$model, levels=c('EWA', 'MED', 'INV', 'V2', 'V3', 'V4',
                                      'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4',
                                      'Baseline'),
                   labels=c("EWA", "MED", "INV", "V[2]", "V[3]", "V[4]", "GQRA[2]", "GQRA[3]", "GQRA[4]",
                            "QRA[2]", "QRA[3]", "QRA[4]", "Baseline"))


m <- levels(df$model)
locs <- c("06", "29", "28", "12", "19")


plot_forecast(subset(df, window_size==4), locations=c("06"), models=c("EWA", "MED", "QRA[2]"), 
              scales="free_y", incidence=TRUE, ncol=5, base_size = 10, 
              title=NULL)

a <- subset(df, location != "US" & window_size==4 & model %in% c("EWA", "MED", "QRA[2]"))
View(subset(a, model=="QRA[2]"))


upit_histogram(subset(df, location != "US" & window_size==4 & model %in% c("EWA", "MED", "QRA[2]")), model, target_end_date, location, 
               facet=model, scales="fixed",
               breaks=seq(0, 1, 0.1), base_size=10) + ylim(0, 2)

ggsave('plots/poster_ensemble_upit.png', width=11, height=5, dpi=500, unit='cm', device='png')


ggsave('plots/ensemble_incidence1.png', width=11, height=6, dpi=600, unit='cm', device='png')



plot_forecast(subset(df, window_size==4), locations=locs, models=m[1:6], scales="free_y", incidence=TRUE, ncol=5, base_size = 10, 
              title=NULL)
ggsave('plots/ensemble_incidence1.png', width=15.7, height=19, dpi=600, unit='cm', device='png')


plot_forecast(subset(df, window_size==4), locations=locs, models=m[7:12], scales="free_y", incidence=TRUE, ncol=5, base_size = 10,
              title=NULL)

ggsave('plots/ensemble_incidence2.png', width=15.7, height=19, dpi=600, unit='cm', device='png')




plot_forecast(data, incidence=TRUE, 
              title="YYG-ParamSearch forecasts with 50%- and 90%-prediction intervals")
ggsave('plots/individual_models/individual_incidence_us.png', width=11, height=7, dpi=500, unit='cm', device='png')


