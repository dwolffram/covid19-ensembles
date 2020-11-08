df_ensembles <- read_csv("data/ensemble_forecasts/df_ensembles_", 
                         col_types = cols(
                           forecast_date = col_date(format = ""),
                           target = col_character(),
                           target_end_date = col_date(format = ""),
                           location = col_character(),
                           type = col_character(),
                           quantile = col_double(),
                           value = col_double()))

ensemble_scores <- wis_table(df_ensembles)

ensemble_scores$window_size <- as.factor(ensemble_scores$window_size)
ensemble_scores$model <- factor(ensemble_scores$model, levels=c('EWA', 'MED', 'V2', 'V3', 'V4', 
                                                         'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4'))

ggplot(data = subset(ensemble_scores, location == 'US'), 
       aes(x = model, y = wis, fill=model)) +
  facet_wrap(~window_size) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "WIS by window size (national level)",
       x = "Model",
       y = "WIS")
