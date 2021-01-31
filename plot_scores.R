### SETTINGS
# theme_set(theme_gray(base_size = 8))
# theme_set(theme(plot.title= element_text(size=9),
#         axis.text = element_text(size = 5)))


source("data_loading.R")
source("plot_functions.R")

### LOAD FILES

df <- load_scores("scores/ensemble_scores_1wk.csv", remove_revisions=TRUE, long_format=TRUE)
df <- load_scores("scores/ensemble_scores_1wk_noUS.csv", remove_revisions=TRUE, long_format=TRUE)

df <- load_scores("scores/ensemble_scores_4wk.csv", remove_revisions=TRUE, long_format=TRUE)
df <- load_scores("scores/ensemble_scores_4wk_noUS.csv", remove_revisions=TRUE, long_format=TRUE)


df <- load_scores("scores/ensemble_scores_1wk_noUS.csv", remove_revisions=FALSE, long_format=TRUE)
df$model <- factor(df$model, labels=c("EWA", "MED", "INV", "V[2]", "V[3]", "V[4]", "GQRA[2]", "GQRA[3]", "GQRA[4]", 
                                          "QRA[2]", "QRA[3]", "QRA[4]", "Baseline"))

plot_state_contribution(subset(df, window_size==4), 3, title=NULL)
ggsave('plots/revisions_percentage.png', width=14, height=7, dpi=500, unit='cm', device='png')


a <- df %>%
  filter(location!="US" & window_size==4 & score=='wis' & model %in% c("EWA", "MED", "INV", "QRA2", "GQRA2")) %>%
  group_by(target_end_date, model) %>%
  summarize(value = mean(value))

ggplot(a, aes(x=target_end_date, y=value, group=model)) +
  geom_line(aes(color=model))

### WIS DECOMPOSITION

## window size comparison
17.7734

plot_wis(df, locations='states', x=window_size, facet=model, 
         ncol=3, hjust=0.5, dir='h', title=NULL)

plot_wis(df, locations='states', x=window_size, facet=model, 
         ncol=3, hjust=0.5, dir='h', title=NULL, yintercept = 96.58747)

ggsave('plots/1wk_ahead/1wk_wis_windowSizes.png', width=15.5, height=18, dpi=500, unit='cm', device='png')
ggsave('plots/4wk_ahead/4wk_wis_windowSizes.png', width=15.5, height=18, dpi=500, unit='cm', device='png')

df %>%
  filter(window_size == 4 & model == "Baseline" & score == "wis" & location != "US") %>%
  summarize(meanWIS = mean(value))


plot_wis(df, locations='states', x=model, facet=window_size, 
         ncol=2, dir='h', angle=90, vjust=0.5, scales='free_x')


plot_wis(df, locations='national', x=window_size, facet=model, 
         ncol=3, dir='h', scales='free_y')

plot_wis(df, locations='national', x=model, facet=window_size, 
         ncol=2, dir='h', angle=90, vjust=0.5, scales='free_x')


## window size 4

plot_wis(df, locations='states', window_sizes=4, x=model, facet=NULL, angle=90, vjust=0.5)

plot_wis(subset(df, location_name != "California"), locations='states', window_sizes=4, x=model, facet=NULL, angle=90, vjust=0.5)


plot_wis(df, locations='national', window_sizes=4, x=model, facet=NULL, angle=90, vjust=0.5)

# plot_wis(df, kind='box', locations='states', window_sizes=4, x=model, facet=NULL)


## wis decomposition by state

plot_wis(df, locations='states', window_sizes=4, x=model, facet=location_name,
         ncol=8, angle=90, vjust=0.5)

plot_wis(df, locations='states', window_sizes=4, x=model, facet=location_name,
         ncol=8, angle=90, vjust=0.5, scales='free_y')

plot_wis(df, locations='states', window_sizes=4, x=location_name, facet=model,
         ncol=2, angle=90, vjust=0.5, scales='free_y')

# plot_wis(df, window_sizes=4, x=model, facet=location_name,
#          ncol=8, angle=90, vjust=0.5)


## wis decomposition over time

plot_wis(df, locations='states', window_sizes=4, x=model, facet=target_end_date,
         ncol=3, dir='h', angle=90, vjust=0.5)

plot_wis(df, locations='national', window_sizes=4, x=model, facet=target_end_date,
         ncol=3, dir='h', angle=90, vjust=0.5, scales='free_y')

# plot_wis(df, locations='states', window_sizes=4, x=target_end_date, facet=model,
#          ncol=3, dir='h', angle=90, vjust=0.5)



### BOXPLOTS

plot_wis(df, locations='national', x=model, facet=window_size, kind='box',
         ncol=2, angle=90, vjust=0.5, scales='free_y')


plot_wis(df, locations='national', x=window_size, facet=model, kind='box', 
         ncol=3, dir='h', scales='free_y')

# plot_wis(df, locations='national', window_sizes=1:4, x=model, facet=window_size, kind='box', 
#          ncol=2, angle=90, vjust=0.5, scales='free_y', export=TRUE)


## window_size 4

plot_wis(df, kind='box', locations='national', window_sizes=4, x=model, facet=location)

# plot_wis(df, kind='box', locations='states', window_sizes=4, x=model, facet=NULL)

# plot_wis(df, locations='states', window_sizes=4, x=model, facet=window_size, kind='box') + 
#   coord_cartesian(ylim=c(0, 50))


### INDIVIDUAL MODELS


df <- load_scores("scores/individual_scores_1wk.csv", long_format=TRUE)
df$model <- factor(df$model)
plot_state_contribution(df, 3, title=NULL)

ggsave('plots/revisions_percentage.png', width=14, height=8, dpi=500, unit='cm', device='png')
plot_wis(df, locations='states', x=model, facet=NULL, angle=90, vjust=0.5, hjust=1, title=NULL)


### COMBINED

df1 <- load_scores("scores/ensemble_scores_1wk_noUS.csv", remove_revisions=TRUE, long_format=TRUE)
df2 <- load_scores("scores/individual_scores_1wk.csv", remove_revisions=TRUE, long_format=TRUE)

df1 <- load_scores("scores/ensemble_scores_4wk_noUS.csv", remove_revisions=TRUE, long_format=TRUE)
df2 <- load_scores("scores/individual_scores_4wk.csv", remove_revisions=TRUE, long_format=TRUE)

df1$type <- "Ensemble"
df1[df1$model == "Baseline", ]$type <- "Individual Model"
df2$type <- "Individual Model"

df <- bind_rows(subset(df1, window_size == 4), df2) %>%
  filter(location != "US" & model != "COVIDhub-baseline") %>%
  select(-window_size)

df$model <- factor(df$model, levels = c('EWA', 'MED', 'INV', 'V2', 'V3', 'V4',
                                        'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4',
                                        "CovidAnalytics-DELPHI", "CU-select", "JHU_IDD-CovidSP", 
                                        "LANL-GrowthRate", "MOBS-GLEAM_COVID", "PSI-DRAFT", "UCLA-SuEIR", 
                                        "UMass-MechBayes", "YYG-ParamSearch", 'Baseline'),
                   labels = c("EWA", "MED", "INV", "V[2]", "V[3]", "V[4]", 
                              "GQRA[2]", "GQRA[3]", "GQRA[4]", "QRA[2]", "QRA[3]", "QRA[4]", 
                              "DELPHI", "CU", "JHU_IDD", 
                              "LANL", "MOBS", "PSI", "UCLA", 
                              "UMass", "YYG", 'Baseline')) 

# toString(shQuote(unique(df2$model), type = "cmd"))

ggplot(subset(df, score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
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
       y = "Mean WIS")# +
  #coord_flip()

ggsave('plots/1wk_ahead/1wk_wis_all.png', width=15.5, height=9, dpi=500, unit='cm', device='png')
ggsave('plots/4wk_ahead/4wk_wis_all.png', width=15.5, height=9, dpi=500, unit='cm', device='png')




ggplot(subset(df, score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u") & location %in% locs), 
       aes(x=reorder(model, value), y=value,
           fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap("location_name", ncol=2, scales="free") +
  geom_bar(position="stack", stat="summary", fun=mean, width=0.7) +
  theme_gray(base_size=10) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.position = c(0.9, 0), 
        legend.justification = c(1, 0)) +
  scale_fill_viridis(discrete=TRUE, name = NULL,
                     labels = c("Overprediction", "Dispersion", "Underprediction"))+
  scale_x_discrete(labels = function(l) parse(text=l)) + 
  labs(x = NULL,
       y = "Mean WIS")# +

ggsave('plots/examples_wis.png', width=15.5, height=15, dpi=500, unit='cm', device='png')


df_rank <- df %>%
  filter(score == "wis") %>%
  group_by(target_end_date, location, model, type) %>%
  summarize(meanWIS = mean(value)) %>%
  group_by(target_end_date, location) %>%
  arrange(model, meanWIS) %>% 
  mutate(rank=rank(meanWIS)) %>%
  arrange(target_end_date)

df_rank <- df_rank %>%
  group_by(model) %>%
  mutate(meanRank=mean(rank))

ggplot(df_rank, aes(x=reorder(model, meanRank), fill=type, y=rank)) + 
  geom_boxplot(alpha=0.5, outlier.size = 0.6) +
  scale_fill_manual(values=c("#009E73", "#D55E00")) +
  stat_summary(fun=mean, geom="point", shape=3, size=2) +
  #xlab("Model") +
  ylab("Rank") +
  scale_x_discrete(NULL, labels = parse(text = levels(reorder(df_rank$model, df_rank$meanRank))))+
  theme_gray(base_size=10) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.title = element_blank(), legend.position="top") 

ggsave('plots/1wk_ahead/1wk_rank_all.png', width=15.5, height=10, dpi=500, unit='cm', device='png')
ggsave('plots/4wk_ahead/4wk_rank_all_byState.png', width=15.5, height=10, dpi=500, unit='cm', device='png')

