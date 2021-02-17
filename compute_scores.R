source("data_loading.R")
source("scoring.R")

### ENSEMBLES

# 1 wk ahead

df_ensembles <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/ensemble_scores_1wk.csv", row.names=FALSE)

# 4 wk ahead

df_ensembles <- load_ensembles("data/ensemble_forecasts/df_ensembles_4wk.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/ensemble_scores_4wk.csv", row.names=FALSE)

## trained without US

# 1 wk ahead

df_ensembles <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_noUS.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/ensemble_scores_1wk_noUS.csv", row.names=FALSE)

# 4 wk ahead

df_ensembles <- load_ensembles("data/ensemble_forecasts/df_ensembles_4wk_noUS.csv", add_baseline = TRUE)
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


#### NEW EVALUATION STUDY

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_all.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_all.csv", row.names=FALSE)


df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_KarOlUM.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_KarOlUM.csv", row.names=FALSE)

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_KarUMBa.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_KarUMBa.csv", row.names=FALSE)


df <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_all.csv", remove_revisions=TRUE, long_format=TRUE)

df <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarOlUM.csv", remove_revisions=TRUE, long_format=TRUE)

df <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarUMBa.csv", remove_revisions=TRUE, long_format=TRUE)


plot_wis(df, locations='states', window_sizes=4, x=model, facet=NULL, angle=90, vjust=0.5)

df$model <- paste0(df$model, '-3')

df2 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_all.csv", remove_revisions=TRUE, long_format=TRUE)

df <- bind_rows(df2, df)
plot_wis(df, locations='states', window_sizes=4, x=model, facet=NULL, angle=90, vjust=0.5)

df3 <- load_scores("scores/evaluation_study/individual_scores_1wk.csv", long_format=TRUE)

df <- subset(df, window_size==4)

df <- bind_rows(df, df3)

plot_wis(df, locations='states', x=model, facet=NULL, angle=90, vjust=0.5)

ggplot(subset(df, location != "US" & score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
       aes(x=reorder(model, value), y=value,
           fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  geom_bar(position="stack", stat="summary", fun=mean, width=0.7) +
  theme_gray(base_size=10) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.position = "right") +
  scale_fill_viridis(discrete=TRUE, name = NULL,
                     labels = c("Overprediction", "Dispersion", "Underprediction"))+
  scale_x_discrete(labels = function(l) parse(text=l)) + 
  labs(x = NULL,
       y = "Mean WIS")


ggsave('plots/evaluation_study/mean_wis_1wk.png', width=30, height=15, dpi=500, unit='cm', device='png')


# combine all versions

df1 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_all.csv", remove_revisions=TRUE, long_format=TRUE)

df2 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarOlUM.csv", remove_revisions=TRUE, long_format=TRUE)
df2$model <- paste0(df2$model, '-3')

df3 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarUMBa.csv", remove_revisions=TRUE, long_format=TRUE)
df3$model <- paste0(df3$model, '-3B')

df <- bind_rows(df1, df2, df3)

ggplot(subset(df, location != "US" & score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
       aes(x=reorder(model, value), y=value,
           fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  geom_bar(position="stack", stat="summary", fun=mean, width=0.7) +
  theme_gray(base_size=10) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.position = "right") +
  scale_fill_viridis(discrete=TRUE, name = NULL,
                     labels = c("Overprediction", "Dispersion", "Underprediction"))+
  #scale_x_discrete(labels = function(l) parse(text=l)) + 
  labs(x = NULL,
       y = "Mean WIS")

## individual scores

df_individual <- load_forecasts(models = c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", "DDS-NBDS",             
                                           "JHU_IDD-CovidSP", "Karlen-pypm", "LANL-GrowthRate", "MOBS-GLEAM_COVID",
                                           "OliverWyman-Navigator", "PSI-DRAFT", "RobertWalraven-ESG", "UA-EpiCovDA", "UCLA-SuEIR",           
                                           "UMass-MechBayes"),
                                targets = "1 wk ahead cum death",
                                exclude_locations = c("11", "60", "66", "69", "72", "74", "78"), 
                                start_date = "2020-08-29", 
                                end_date = "2020-11-14") %>% 
  select(-c(forecast_date, type)) %>%
  select(target_end_date, everything())

individual_scores <- score_forecasts(df_individual)

write.csv(individual_scores, "scores/evaluation_study/individual_scores_1wk.csv", row.names=FALSE)

unique(df$target_end_date)
