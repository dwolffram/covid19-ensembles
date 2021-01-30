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




df1 <- load_scores("scores/ensemble_scores_1wk_noUS.csv", remove_revisions=TRUE, long_format=TRUE)
df2 <- load_scores("scores/individual_scores_1wk.csv", remove_revisions=TRUE, long_format=TRUE)

df1 <- load_scores("scores/ensemble_scores_4wk_noUS.csv", remove_revisions=TRUE, long_format=TRUE)
df2 <- load_scores("scores/individual_scores_4wk.csv", remove_revisions=TRUE, long_format=TRUE)

df1 <- load_scores("scores/ensemble_scores_1wk_noUS.csv", remove_revisions=TRUE, long_format=TRUE)
df2 <- load_scores("scores/ensemble_scores_4wk_noUS.csv", remove_revisions=TRUE, long_format=TRUE)

df1$type <- "Ensemble"
df1[df1$model == "Baseline", ]$type <- "Individual Model"
df2$type <- "Individual Model"

df <- bind_rows(subset(df1, window_size == 4), df2) %>%
  filter(location != "US" & model != "COVIDhub-baseline") %>%
  select(-window_size)

df <- subset(df, window_size == 4) %>%
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

ggsave('plots/1wk_ahead/1wk_wis_ensembles.png', width=10, height=7, dpi=500, unit='cm', device='png')
ggsave('plots/4wk_ahead/4wk_wis_ensembles.png', width=15.5, height=9, dpi=500, unit='cm', device='png')




df_rank <- df %>%
  filter(score == "wis") %>%
  group_by(target_end_date, location, model) %>%
  summarize(meanWIS = mean(value)) %>%
  group_by(target_end_date, location) %>%
  arrange(model, meanWIS) %>% 
  mutate(rank=rank(meanWIS)) %>%
  arrange(target_end_date)

df_rank <- df_rank %>%
  group_by(model) %>%
  mutate(meanRank=mean(rank))

ggplot(df_rank, aes(x=reorder(model, meanRank), fill="#009E73", y=rank)) + 
  geom_boxplot(alpha=0.5, outlier.size = 0.6) +
  scale_fill_manual(values=c("#009E73", "#D55E00")) +
  stat_summary(fun=mean, geom="point", shape=3, size=2) +
  #xlab("Model") +
  ylab("Rank") +
  scale_x_discrete(NULL, labels = parse(text = levels(reorder(df_rank$model, df_rank$meanRank))))+
  theme_gray(base_size=10) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.position="none") 

ggsave('plots/1wk_ahead/1wk_rank_ensembles_byState.png', width=10, height=7, dpi=500, unit='cm', device='png')
ggsave('plots/4wk_ahead/4wk_rank_ensembles_byState.png', width=15.5, height=10, dpi=500, unit='cm', device='png')


### 1 and 4 weeks

df1 <- load_scores("scores/ensemble_scores_1wk_noUS.csv", remove_revisions=TRUE, long_format=TRUE)
df2 <- load_scores("scores/ensemble_scores_4wk_noUS.csv", remove_revisions=TRUE, long_format=TRUE)

df1 <- load_scores("scores/individual_scores_1wk.csv", remove_revisions=TRUE, long_format=TRUE)
df2 <- load_scores("scores/individual_scores_4wk.csv", remove_revisions=TRUE, long_format=TRUE)

df <- bind_rows(df1, df2)

df <- subset(df, window_size == 4) %>%
  filter(location != "US" & model != "COVIDhub-baseline") %>%
  select(-window_size)

df <- subset(df, window_size == 4) %>%
  filter(location != "US") %>%
  select(-window_size)

df <- df %>%
  filter(location != "US")

#df[df$model == "COVIDhub-baseline", ]$model <- "Baseline"


df$model <- factor(df$model, levels = c('EWA', 'MED', 'INV', 'V2', 'V3', 'V4',
                                        'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4',
                                        "CovidAnalytics-DELPHI", "CU-select", "JHU_IDD-CovidSP", 
                                        "LANL-GrowthRate", "MOBS-GLEAM_COVID", "PSI-DRAFT", "UCLA-SuEIR", 
                                        "UMass-MechBayes", "YYG-ParamSearch", 'COVIDhub-baseline'),
                   labels = c("EWA", "MED", "INV", "V[2]", "V[3]", "V[4]", 
                              "GQRA[2]", "GQRA[3]", "GQRA[4]", "QRA[2]", "QRA[3]", "QRA[4]", 
                              "DELPHI", "CU", "JHU_IDD", 
                              "LANL", "MOBS", "PSI", "UCLA", 
                              "UMass", "YYG", 'Baseline')) 

# toString(shQuote(unique(df2$model), type = "cmd"))

library(tidytext)

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) parse(text=gsub(reg, "", x)), ...)
}

ggplot(subset(df, score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
       aes(x=reorder_within(model, value, target), y=value,
           fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  scale_x_reordered() +
  geom_bar(position="stack", stat="summary", fun=mean, width=0.7) +
  facet_wrap("target", scales="free", drop=TRUE) +
  theme_gray(base_size=10) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.position = "right") +
  scale_fill_viridis(discrete=TRUE, name = NULL,
                     labels = c("Overprediction", "Dispersion", "Underprediction"))+
   labs(x = NULL,
       y = "Mean WIS")# +
#coord_flip()

ggsave('plots/1and4wk_wis_ensembles.png', width=15.5, height=7, dpi=500, unit='cm', device='png')
ggsave('plots/1and4wk_wis_individual.png', width=15.5, height=7, dpi=500, unit='cm', device='png')



df <- left_join(df, t1, by = "location")


df_rank <- df %>%
  filter(score == "wis") %>%
  group_by(target, mortality, target_end_date, model) %>%
  summarize(meanWIS = mean(value)) %>%
  group_by(target, mortality, target_end_date) %>%
  arrange(model, meanWIS) %>% 
  mutate(rank=rank(meanWIS)) %>%
  arrange(target_end_date)

df_rank <- df_rank %>%
  group_by(target, mortality, model) %>%
  mutate(meanRank=mean(rank))

ggplot(subset(df_rank, target=="1 wk ahead cum death"), aes(x=reorder_within(model, meanRank, mortality), fill="#D55E00", y=rank)) + 
  geom_boxplot(alpha=0.5, outlier.size = 0.6) +
  scale_x_reordered(NULL)+
  facet_wrap('mortality', scales="free") +
  #scale_fill_manual(values=c("#009E73", "#D55E00")) +
  stat_summary(fun=mean, geom="point", shape=3, size=2) +
  #xlab("Model") +
  ylab("Rank") +
  scale_y_continuous(breaks=c(2, 4, 6, 8, 10)) +
  #scale_x_discrete(NULL, labels = parse(text = levels(reorder(df_rank$model, df_rank$meanRank))))+
  theme_gray(base_size=10) +
  theme(axis.text.x=element_text(vjust=0.5, angle=90, hjust=1), 
        legend.position="none")
  
ggsave('plots/1and4wk_rank_individual.png', width=15.5, height=7, dpi=500, unit='cm', device='png')





### LOW MEDIUM HIGH MORTALITY

a <- left_join(df, t1, by = "location")

plot_wis(a, locations='states', x=model, facet=mortality, 
         ncol=3, hjust=0.5, dir='h', title=NULL, scales="fixed")

plot_wis(df, locations='states', x=model, facet=mortality, 
         ncol=3, hjust=0.5, dir='h', title=NULL, scales="fixed")
