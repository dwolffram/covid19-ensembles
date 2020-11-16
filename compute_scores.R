source("data_loading.R")
source("scoring.R")

### ENSEMBLES

# 1 wk ahead

df_ensembles <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_2020-11-13.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/ensemble_scores_1wk.csv", row.names=FALSE)

# 4 wk ahead

df_ensembles <- load_ensembles("data/ensemble_forecasts/df_ensembles_4wk_2020-11-11.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/ensemble_scores_4wk.csv", row.names=FALSE)

## trained without US

# 1 wk ahead

df_ensembles <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_noUS_2020-11-14.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/ensemble_scores_1wk_noUS.csv", row.names=FALSE)

# 4 wk ahead

df_ensembles <- load_ensembles("data/ensemble_forecasts/df_ensembles_4wk_noUS_2020-11-15.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/ensemble_scores_4wk_noUS.csv", row.names=FALSE)



### INDIVIDUAL MODELS

# 1 wk ahead

df_individual <- load_forecasts(models = c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", 
                                           "JHU_IDD-CovidSP", "LANL-GrowthRate", "MOBS-GLEAM_COVID", 
                                           "PSI-DRAFT", "UCLA-SuEIR", "UMass-MechBayes", "YYG-ParamSearch"),
                                targets = "1 wk ahead cum death",
                                exclude_locations = c("11", "60", "66", "69", "72", "74", "78"), 
                                start_date = "2020-06-20", 
                                end_date = "2020-10-10") %>% 
  select(-c(forecast_date, type)) %>%
  select(target_end_date, everything())

individual_scores <- score_forecasts(df_individual)

write.csv(individual_scores, "scores/individual_scores_1wk.csv", row.names=FALSE)

# 4 wk ahead

df_individual <- load_forecasts(models = c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", 
                                           "JHU_IDD-CovidSP", "LANL-GrowthRate", "MOBS-GLEAM_COVID", 
                                           "PSI-DRAFT", "UCLA-SuEIR", "UMass-MechBayes", "YYG-ParamSearch"),
                                targets = "4 wk ahead cum death",
                                exclude_locations = c("11", "60", "66", "69", "72", "74", "78"), 
                                start_date = "2020-08-01", 
                                end_date = "2020-10-31") %>% 
  select(-c(forecast_date, type)) %>%
  select(target_end_date, everything())

individual_scores <- score_forecasts(df_individual)

write.csv(individual_scores, "scores/individual_scores_4wk.csv", row.names=FALSE)
