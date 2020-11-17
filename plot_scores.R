source("data_loading.R")
library(ggplot2)

# requires long format
plot_wis <- function(df, models, locations, window_sizes, x='window_size',
                     facet, facet_row=location_name, facet_col=model,
                     start_date='1900-01-01', end_date='3000-01-01',
                     kind='bar', ncol=4, dir='v', scales='fixed', angle=0, vjust=0){
  x <- ensym(x)
  facet <- ensym(facet)
  
  if(missing(models)){
    models <- unique(df$model)
  }  
  
  if(missing(window_sizes) & 'window_size' %in% colnames(df)){
    window_sizes <- unique(df$window_size)
  }

  # consider national or state level (or all)
  if(!missing(locations)){
    if(all(locations == 'national')){
      plot_title <- 'WIS (national level)'
      df <- subset(df, location == 'US')
      locations <- unique(df$location)
    } 
    else if (all(locations == 'states')){
      plot_title <- 'WIS (state level)'
      df <- subset(df, location != 'US')
      locations <- unique(df$location)
    }
    else{
      plot_title <- 'WIS'
    }
  }
  else{
    plot_title <- 'WIS'
    locations <- unique(df$location) # if not given, use all locations
  }
  
  # filter scores
  df <- df %>%
    filter(model %in% models,
           location %in%  locations,
           target_end_date >= start_date,
           target_end_date <= end_date) %>%
           {if("window_size" %in% names(.)) filter(., window_size %in% window_sizes) else .}

  
  xlabel <- str_to_title(str_replace_all(x, "_", " "))
  
  start_date <- min(df$target_end_date)
  end_date <- max(df$target_end_date)
  
  plot_title <- paste0(plot_title, " - from ", start_date, " until ", end_date)
  
  switch(kind,
         'bar' = {
           g <- ggplot(subset(df, score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
                       aes(x=!!x, y=value, fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
             geom_bar(position="stack", stat="summary", fun.y="sum", width=0.7) +
             theme_gray(base_size=18) +
             theme(axis.text.x=element_text(vjust=vjust, angle=angle)) +
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
             theme(axis.text.x=element_text(vjust=vjust, angle=angle), legend.position = "none")
         })
  
  g + {if(!missing(facet)) facet_wrap(facet, ncol=ncol, dir=dir, scales=scales)} + # if facet is given
    {if(missing(facet)) facet_grid(rows=enquos(facet_row), cols=enquos(facet_col), scales=scales)} +
    labs(title= plot_title,
         x = xlabel,
         y = "WIS") #+
    # theme(plot.title= element_text(size=9),
    #       axis.text = element_text(size = 4))
      
}


df <- load_scores("scores/ensemble_scores_1wk.csv")
df <- load_scores("scores/ensemble_scores_4wk.csv")
df <- load_scores("scores/individual_scores_1wk.csv")

#plot_wis(df, x=window_size, facet=NULL)
plot_wis(df, locations='states', x=window_size, facet="model")
plot_wis(df, locations='states', x=window_size, window_sizes=c(1, 4), facet="model")
plot_wis(df, locations='national', x=window_size, facet=model)
plot_wis(df, locations='national', x=window_size, facet=model, start_date='2020-08-08')

plot_wis(df, x=model, facet=location)
plot_wis(df, x=window_size, locations=c('US', 24, 36, 27), scales='free_y')
plot_wis(df, x=model, locations=c('US', 24, 36, 27), facet=location_name, scales='free_y', ncol=2,
         angle=90, vjust=0.5)

plot_wis(df, x=model, locations=c('US', 24, 36, 27), facet=location_name, scales='free_y', ncol=2,
         angle=90, vjust=0.5, kind='box')


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


