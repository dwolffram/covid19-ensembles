library(ggplot2)
library(viridis)

### PLOT FORECASTS

plot_forecast <- function(df, models, locations, window_sizes,
                          facet, facet_row=location_name, facet_col=model,
                          incidence=FALSE, center=FALSE, title=NULL,
                          start_date='1900-01-01', end_date='3000-01-01',
                          ncol=4, dir='v', scales='fixed', base_size=18){
  cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1] # length(levels_coverage) + 1
  facet <- ensym(facet)
  
  df <- pivot_wider(df, names_from=quantile, names_prefix="value.", values_from=value)
  
  if(missing(models)){
    models <- unique(df$model)
  }  
  
  if(missing(locations)){
    locations <- unique(df$location)
  }
  
  if(missing(window_sizes) & 'window_size' %in% colnames(df)){
    window_sizes <- unique(df$window_size)
  }
  
  # filter forecasts
  df <- df %>%
    filter(model %in% models,
           location %in%  locations,
           target_end_date >= start_date,
           target_end_date <= end_date
    )
  
  if('window_size'  %in% colnames(df)){
    df <- df %>%
      filter(window_size %in% window_sizes)
  }
  
  if (incidence){
    if (missing(facet)){
      grouping_vars <- quos(model, location)
    }
    else{
      grouping_vars <- facet
    }
    
    horizon = as.numeric(substr(unique(df$target), 1, 1))
    
    df <- df %>% 
      group_by(!!!grouping_vars) %>%
      mutate(value.0.5 = value.0.5 - lag(truth, horizon)) %>%
      mutate(value.0.05 = value.0.05 - lag(truth, horizon)) %>%
      mutate(value.0.95 = value.0.95 - lag(truth, horizon)) %>%
      mutate(value.0.25 = value.0.25 - lag(truth, horizon)) %>%
      mutate(value.0.75 = value.0.75 - lag(truth, horizon)) %>%
      mutate(truth=c(NA, diff(truth))) %>%
      drop_na()
  }
  
  if (center){
    if (missing(facet)){
      grouping_vars <- quos(model, location)
    }
    else{
      grouping_vars <- facet
    }
    
    df <- df %>% 
      group_by(!!!grouping_vars) %>%
      mutate(value.0.5 = value.0.5 - truth) %>%
      mutate(value.0.05 = value.0.05 - truth) %>%
      mutate(value.0.95 = value.0.95 - truth) %>%
      mutate(value.0.25 = value.0.25 - truth) %>%
      mutate(value.0.75 = value.0.75 - truth) %>%
      mutate(truth=0) %>%
      drop_na()
  }
  
  # default title
  if(missing(title)){
    horizon = substr(unique(df$target), 1, 1)
    title = paste(horizon, "wk ahead forecasts with 50% and 90% prediction intervals")
  }
  
  ggplot(df, aes(x=target_end_date, y=truth)) +
    {if(!missing(facet)) facet_wrap(facet, ncol=ncol, dir=dir, scales=scales)} +
    {if(missing(facet)) facet_grid(rows=enquos(facet_row), cols=enquos(facet_col), scales=scales)} +
    geom_smooth(aes(y = value.0.5, ymin = value.0.05, ymax = value.0.95), 
                linetype=2, size=0.5, fill=cols[2], alpha=1, stat = "identity") +
    geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
                linetype=2, size=0.5, fill=cols[1], alpha=1, stat = "identity") +
    geom_line() +
    geom_point() +
    geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white") +
    theme_bw() +
    #theme(axis.text.x=element_text(angle=45,hjust=1)) +
    labs(title=title,
         x = "Date",
         y = "Deaths") +
    theme_grey(base_size=base_size)#+
  # theme(plot.title= element_text(size=9),
  #       axis.text = element_text(size = 5)) 
}


### PLOT SCORES

plot_wis <- function(df, models, locations, window_sizes, x='window_size',
                     facet, facet_row=location_name, facet_col=model,
                     start_date='1900-01-01', end_date='3000-01-01',
                     kind='bar', outlier.shape=NA,
                     ncol=4, dir='v', scales='fixed', angle=0, vjust=0, hjust=0,
                     title, export=FALSE){
  x <- ensym(x)
  
  try(facet <- ensym(facet), silent=TRUE) # if NULL: no facetting
  
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
  
  if(!missing(title)){
    plot_title <- title
  }
  
  if(export){
    filename <- str_replace_all(plot_title, ' ', '_')
    filename <- paste(kind,
                      tolower(filename),
                      paste0('x-', deparse(substitute(x))),
                      paste0('fct-', deparse(substitute(facet))),
                      paste0('ws-', deparse(as.numeric(window_sizes))),
                      paste0('h-', substr(unique(df$target), 1, 1)),
                      paste0('scl-', scales),
                      sep='_')
    print(filename)
  }

  switch(kind,
         'bar' = {
           g <- ggplot(subset(df, score %in% c("wgt_pen_l", "wgt_iw", "wgt_pen_u")), 
                       aes(x=!!x, y=value,
                           fill=factor(score, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
             geom_bar(position="stack", stat="summary", fun=mean, width=0.7) +
             theme_gray(base_size=18) +
             theme(axis.text.x=element_text(vjust=vjust, angle=angle, hjust=hjust)) +
             scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                                labels = c("Overprediction", "Dispersion", "Underprediction"))
         },
         'box' = {
           g <- ggplot(subset(df, score=='wis'), 
                       aes(x=!!x, y=value, fill=!!x)) +
             geom_boxplot(outlier.shape=outlier.shape, width=0.5) +
             stat_summary(fun.y=mean, geom="point", shape=3) +
             scale_fill_viridis(discrete = TRUE, alpha=0.5) +
             theme_gray(base_size=18) +
             theme(axis.text.x=element_text(vjust=vjust, angle=angle, hjust=hjust), legend.position = "none")
         })
  
  g <- g + 
    {if(!missing(facet) & !is.null(facet)) facet_wrap(facet, ncol=ncol, dir=dir, scales=scales)} + # if facet is given
    {if(missing(facet)) facet_grid(rows=enquos(facet_row), cols=enquos(facet_col), scales=scales)} +
    labs(title= plot_title,
         x = xlabel,
         y = "Mean WIS") #+
  # theme(plot.title= element_text(size=9),
  #       axis.text = element_text(size = 4))
  
  if(export){
    ggsave(paste0('plots/evaluation/ensembles/', filename, '.png'), 
           width=45, height=30, dpi=72, unit='cm', device='png')
  }
  
  return(g)
  
}


plot_state_contribution <- function(df, n_highest=5){
  df <- df %>%
    filter(location!='US', score=='wis', window_size==4) %>%
    group_by(model, location) %>%
    summarize(value = sum(value))
  
  df <- df %>% 
    group_by(model) %>% 
    arrange(desc(value)) %>%  
    mutate(Index = 1:n(), location = ifelse(Index > n_highest, "Other", location))
  
  df <- df %>%
    group_by(model, location) %>%
    summarize(value = sum(value))
  
  df <- add_location_names(df) 
  
  df$location_name <- as.character(df$location_name)
  
  df <- df%>%
    replace_na(list(location_name='Other'))
  
  ggplot(df, aes(x = model, y = value, fill=reorder(location_name, value))) + 
    geom_bar(stat = "summary", position='fill', fun.y='sum') + 
    scale_fill_viridis(discrete=TRUE, name = "State") +
    labs(title = "Relative Contribution to WIS ", x = "Model", y = "Percentage of WIS") +
    theme_gray(base_size=18)
}
