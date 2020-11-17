source("data_loading.R")
library(ggplot2)

df <- load_scores("scores/ensemble_scores_1wk.csv")
df <- load_scores("scores/ensemble_scores_4wk.csv")

df <- load_scores("scores/individual_scores_1wk.csv")


# requires long format
plot_wis <- function(df, facet, x='window_size', window_sizes=1:4, locations='all', 
                     start_date='1900-01-01', end_date='3000-01-01',
                     kind='bar', ncol=4, dir='v'){
  x <- ensym(x)
  facet <- ensym(facet)
  
  # consider national or state level (or all)
  switch(locations,
         "national" = {
           df <- subset(df, location == 'US')
         },
         "states" = {
           df <- subset(df, location != 'US')
         }
         )
  
  # filter scores
  df <- df %>%
    filter(target_end_date >= start_date,
           target_end_date <= end_date) %>%
           {if("window_size" %in% names(.)) filter(., window_size %in% window_sizes) else .}

  
  xlabel <- str_to_title(str_replace_all(x, "_", " "))
  
  switch(locations,
         'all'={
           plot_title <- "WIS"
           },
         'states' = {
           plot_title <- "WIS (state level)"
           },
         'national' = {
           plot_title <- "WIS (national level)"
         })
  
  start_date <- min(df$target_end_date)
  end_date <- max(df$target_end_date)
  
  plot_title <- paste0(plot_title, " - from ", start_date, " until ", end_date)
  
  switch(kind,
         'bar' = {
           g <- ggplot(subset(df, score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
                       aes(x=!!x, y=value, fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
             geom_bar(position="stack", stat="summary", fun.y="sum", width=0.7) +
             theme_gray(base_size=18) +
             theme(axis.text.x=element_text(angle=0, hjust=0)) +
             scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                                labels = c("Overprediction", "Dispersion", "Underprediction")) 
         },
         'box' = {
           g <- ggplot(subset(df, score=='wis'), 
                       aes(x=!!x, y=value, fill=!!x)) +
             geom_boxplot(width=0.5) +
             stat_summary(fun.y=mean, geom="point", shape=3) +
             scale_fill_viridis(discrete = TRUE, alpha=0.5) +
             theme_gray(base_size=18) +
             theme(axis.text.x=element_text(angle=0, hjust=0), legend.position = "none")
         })
  
  g + {if(!missing(facet)) facet_wrap(facet, ncol=ncol, dir=dir)} + # if facet is given
    labs(title= plot_title,
         x = xlabel,
         y = "WIS") #+
    # theme(plot.title= element_text(size=9),
    #       axis.text = element_text(size = 4))
      
  }

plot_wis(df, x=window_size)
plot_wis(df, locations='states', x=window_size, facet="model")
plot_wis(df, locations='states', x=window_size, window_sizes=c(1, 4), facet="model")
plot_wis(df, locations='national', x=window_size, facet=model)
plot_wis(df, locations='national', x=window_size, facet=model, start_date='2020-08-08')
plot_wis(df, x=model, facet=location)
plot_wis(df, location='states', x=model, facet=location)
plot_wis(df, location='states', x=model, facet=location_name, start_date='2020-08-08', 
         ncol=8, dir='h')
plot_wis(df, location='all', x=model, facet=location, start_date='2020-08-08', 
         ncol=8, dir='h')
plot_wis(df, location='states', x=target_end_date, facet=model)

plot_wis(df, locations='national', x=window_size, facet=model, start_date='2020-08-08')

plot_wis(df, x=model, window_sizes=4, locations='national')

plot_wis(df, x=model)
plot_wis(df, x=model, locations='national', start_date='2020-08-08')
plot_wis(df, x=model, locations='national', kind='box')



plot_wis(df, x=window_size, locations='national', kind='box')
plot_wis(df, locations='national', x=window_size, facet=model, kind='box')


