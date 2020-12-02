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

df_individual <- load_scores("scores/individual_scores_1wk.csv", long_format=TRUE)


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
