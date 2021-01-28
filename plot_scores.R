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

plot_wis(df, locations='states', x=window_size, facet=model, 
         ncol=3, dir='h')

plot_wis(df, locations='states', x=model, facet=window_size, 
         ncol=2, dir='h', angle=90, vjust=0.5, scales='free_x')


plot_wis(df, locations='national', x=window_size, facet=model, 
         ncol=3, dir='h', scales='free_y')

plot_wis(df, locations='national', x=model, facet=window_size, 
         ncol=2, dir='h', angle=90, vjust=0.5, scales='free_x')


## window size 4

plot_wis(df, locations='states', window_sizes=4, x=model, facet=NULL, angle=90, vjust=0.5)
plot_wis(df, locations='national', window_sizes=4, x=model, facet=NULL, angle=90, vjust=0.5)

# plot_wis(df, kind='box', locations='states', window_sizes=4, x=model, facet=NULL)


## wis decomposition by state

plot_wis(df, locations='states', window_sizes=4, x=model, facet=location_name,
         ncol=8, angle=90, vjust=0.5)

plot_wis(df, locations='states', window_sizes=4, x=model, facet=location_name,
         ncol=8, angle=90, vjust=0.5, scales='free_y')

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

