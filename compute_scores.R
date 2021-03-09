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

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_all_QNA3_2021-03-01.csv", 
                               add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_all_QNA3_ws4.csv", row.names=FALSE)

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_all_ws4.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_all_ws4.csv", row.names=FALSE)

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_all.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_all.csv", row.names=FALSE)


df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_KarOlUM.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_KarOlUM.csv", row.names=FALSE)

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_KarUMBa.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_KarUMBa.csv", row.names=FALSE)

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_KarUMBa_INVA.csv", 
                               add_baseline = FALSE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_KarUMBa_INVA.csv", row.names=FALSE)

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_KarOlUM_INVA.csv", 
                               add_baseline = FALSE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_KarOlUM_INVA.csv", row.names=FALSE)


df <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_all.csv", remove_revisions=TRUE, long_format=TRUE)

df <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarOlUM.csv", remove_revisions=TRUE, long_format=TRUE)

df <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarUMBa.csv", remove_revisions=TRUE, long_format=TRUE)

df <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_all_QNA3_ws4.csv", remove_revisions=TRUE, long_format=TRUE)


plot_wis(df, locations='states', window_sizes=4, x=model, facet=NULL, angle=90, vjust=0.5)

ggplot(subset(df, location !='US' & window_size==4 & score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
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
plot_wis(df1, locations='states', x=model, facet=NULL, angle=90, vjust=0.5)


df1 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_all.csv", remove_revisions=TRUE, long_format=TRUE)

df2 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarOlUM.csv", remove_revisions=TRUE, long_format=TRUE)
df2$model <- paste0(df2$model, '-3')

df3 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarUMBa.csv", remove_revisions=TRUE, long_format=TRUE)
df3$model <- paste0(df3$model, '-3B')

df <- bind_rows(df1, df2, df3)
df <- subset(df, window_size==4)

df4 <- load_scores("scores/evaluation_study/individual_scores_1wk.csv", long_format=TRUE)
df <- bind_rows(df, df4)


df5 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarUMBa_INVA.csv", remove_revisions=TRUE, long_format=TRUE)
df5$model <- paste0(df5$model, '-3B')

df5 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarOlUM_INVA.csv", remove_revisions=TRUE, long_format=TRUE)
df5$model <- paste0(df5$model, '-3')

df_3B <- bind_rows(df3, df5)
unique(df_3B$model)

df_3B <- subset(df_3B, window_size=="4" & location != 'US')

df_3B %>%
  group_by(model) %>%
  summarize(n = n())

df2 %>%
  filter(window_size == 4) %>%
  group_by(model) %>%
  summarize(n = n())

a <- df_3 %>% 
  filter(score == 'wis') %>%
  group_by(model) %>%
  summarize(mean_wis = mean(value))

df_3B %>% 
  filter(score == 'wis' & location != 'US') %>%
  group_by(model) %>%
  summarize(mean_wis = sum(value))

df_3B %>% 
  filter(score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u") & location != 'US') %>%
  group_by(model) %>%
  summarize(mean_wis = sum(value))


df6 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarOlUM_INVA.csv", remove_revisions=TRUE, long_format=TRUE)
df6$model <- paste0(df6$model, '-3')

df_3 <- bind_rows(df2, df6)
unique(df_3$model)

df_3 <- subset(df_3, window_size=="4" & location != 'US')

ggplot(subset(df_3, location != "US" & score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
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

ggplot(subset(df_3B, score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
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

ggsave('plots/evaluation_study/mean_wis_1wk_3.png', width=20, height=15, dpi=500, unit='cm', device='png')

ggsave('plots/evaluation_study/mean_wis_1wk_all.png', width=30, height=15, dpi=500, unit='cm', device='png')

View(subset(df_3B, model %in% c('INV-3B', 'INVA-3B')) )
View(subset(df_3B, model %in% c('INVA-3B')) )
View(subset(df_3B, model %in% c('INV-3B')) )


b <- subset(df_3B, model %in% c('INV-3B') & score == 'wis')
c <- subset(df_3B, model %in% c('INVA-3B') & score == 'wis')
d <- subset(df_3B, model %in% c('Baseline-3B') & score == 'wis')

e <- anti_join(d, b, by=c("target_end_date", "location", "target", "window_size", "location_name"))

mean(b$wis)

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

### Flexible subset of models

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_top3_ws4.csv", add_baseline = TRUE)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_top3_ws4.csv", row.names=FALSE)

df3 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_top3_ws4.csv", remove_revisions=TRUE, long_format=TRUE)

ggplot(subset(df3, location !='US' & window_size==4 & score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
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

df1 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_all_ws4.csv", remove_revisions=TRUE, long_format=TRUE)
df1 <- load_scores("scores/evaluation_study/", remove_revisions=TRUE, long_format=TRUE)

df2 <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_KarOlUM.csv", remove_revisions=TRUE, long_format=TRUE)
df2$model <- paste0(df2$model, '-3')

df3$model <- paste0(df3$model, '-3F')

df_all <- bind_rows(df1, df2, df3)

ggplot(subset(df_all, location != 'US' & window_size==4 & score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
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

df_all <- bind_rows(df1, df3)

ggplot(subset(df_all, location != 'US' & window_size==4 & score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
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

length(unique(df$model))

ggsave('plots/evaluation_study/mean_wis_1wk_3F.png', width=20, height=15, dpi=500, unit='cm', device='png')

scores <- df_all %>%
  filter(location != 'US' & window_size==4 & score == 'wis') %>%
  group_by(model) %>%
  summarize(mean_wis = mean(value))

scores <- df %>%
  filter(location != 'US' & window_size==4 & score == 'wis') %>%
  group_by(model) %>%
  summarize(mean_wis = mean(value))

### INDIVIDUAL MODELS

# 1 wk ahead

df_individual <- load_forecasts(models = c("COVIDhub-ensemble"),
                                targets = "1 wk ahead cum death",
                                exclude_locations = c("11", "60", "66", "69", "72", "74", "78"), 
                                start_date = "2020-08-29", 
                                end_date = "2021-02-06") %>% 
  select(-c(forecast_date, type)) %>%
  select(target_end_date, everything())

individual_scores <- score_forecasts(df_individual)

write.csv(individual_scores, "scores/evaluation_study/COVIDhub-ensemble_scores_1wk.csv", row.names=FALSE)

individual_scores <- load_scores("scores/evaluation_study/COVIDhub-ensemble_scores_1wk.csv", remove_revisions=TRUE, long_format=TRUE)

individual_scores$window_size = '4'
df_all <- bind_rows(df_all, individual_scores)

ggplot(subset(df_all, location != 'US' & window_size==4 & score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
       aes(x=reorder(model, value), y=value,
           fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  geom_bar(position="stack", stat="summary", fun=mean, width=0.7) +
  theme_gray(base_size=12) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.position = "right") +
  scale_fill_viridis(discrete=TRUE, name = NULL,
                     labels = c("Overprediction", "Dispersion", "Underprediction"))+
  #scale_x_discrete(labels = function(l) parse(text=l)) + 
  labs(x = NULL,
       y = "Mean WIS")

ggsave('plots/evaluation_study/mean_wis_1wk_ensembles.png', width=20, height=15, dpi=500, unit='cm', device='png')


### in-sample and out-of-sample scores

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_all_ws4_is.csv", 
                               add_baseline = FALSE, in_sample=TRUE)
df_ensembles <- subset(df_ensembles, window_size==4)
df_ensembles <- add_truth(df_ensembles)
df_ensembles$target_end_date <- paste0(df_ensembles$target_end_date, '_', df_ensembles$id_date)

ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_all_ws4_is.csv", row.names=FALSE)

df_ensembles <- load_ensembles("data/ensemble_forecasts/evaluation_study/df_ensembles_1wk_noUS_all_ws4_oos.csv", 
                               add_baseline = TRUE)
df_ensembles <- subset(df_ensembles, window_size==4)
df_ensembles <- add_truth(df_ensembles)
ensemble_scores <- score_forecasts(df_ensembles)
write.csv(ensemble_scores, "scores/evaluation_study/ensemble_scores_1wk_noUS_all_ws4_oos.csv", row.names=FALSE)

df <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_all_ws4_is_new2.csv", 
                  remove_revisions=TRUE, long_format=TRUE, in_sample=TRUE)

df <- load_scores("scores/evaluation_study/ensemble_scores_1wk_noUS_all_ws4_oos.csv", 
                  remove_revisions=TRUE, long_format=TRUE, in_sample=FALSE)

df_hubEnsemble <- load_scores("scores/evaluation_study/COVIDhub-ensemble_scores_1wk.csv", remove_revisions=TRUE, long_format=TRUE)

df_hubEnsemble$window_size = '4'
df <- bind_rows(df, df_hubEnsemble)

df$model <- factor(df$model, levels = c('EWA', 'MED', 'INV', 'V2', 'V3', 'V4',
                                        'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4', 'QNA3', 'INVA',
                                        "CovidAnalytics-DELPHI", "CU-select", "JHU_IDD-CovidSP", 
                                        "LANL-GrowthRate", "MOBS-GLEAM_COVID", "PSI-DRAFT", "UCLA-SuEIR", 
                                        "UMass-MechBayes", "YYG-ParamSearch", "Baseline", "COVIDhub-ensemble"),
                   labels = c("EWA", "MED", "INV", "V[2]", "V[3]", "V[4]", 
                              "GQRA[2]", "GQRA[3]", "GQRA[4]", "QRA[2]", "QRA[3]", "QRA[4]", "QNA[3]", "INVA",
                              "DELPHI", "CU", "JHU_IDD", 
                              "LANL", "MOBS", "PSI", "UCLA", 
                              "UMass", "YYG", 'Baseline', "COVIDhub-ensemble"))

ggplot(subset(df, location != 'US' & window_size==4 & score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
       aes(x=reorder(model, value), y=value,
           fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  geom_bar(position="stack", stat="summary", fun=mean, width=0.7) +
  theme_gray(base_size=12) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.position = "right") +
  scale_fill_viridis(discrete=TRUE, name = NULL,
                     labels = c("Overprediction", "Dispersion", "Underprediction"))+
  scale_x_discrete(labels = function(l) parse(text=l)) + 
  labs(x = NULL,
       y = "Mean WIS") +
  ggtitle("In-sample performance")

ggsave('plots/evaluation_study/mean_wis_1wk_ensembles_outOfSample.png', width=20, height=15, dpi=500, unit='cm', device='png')


scores <- df %>%
  filter(location != 'US' & window_size==4 & score == 'wis') %>%
  group_by(model) %>%
  summarize(mean_wis = mean(value))
