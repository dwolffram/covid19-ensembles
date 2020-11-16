source("data_loading.R")
source("scoring.R")
source("ensemble_methods.R")
source("ensemble_functions.R")
library(ggplot2)

#load_scores <- function(target, horizon, scores=c('ae', 'wis'))

load_scores <- function(filename, scores=c('ae', 'wis'), long_format=FALSE){
  df <- read_csv(filename, 
                 col_types = cols(
                   target = col_character(),
                   target_end_date = col_date(format = ""),
                   location = col_character(),
                   window_size = col_factor(c("1", "2", "3","4")),
                   model = col_factor(c('Baseline', 'EWA', 'MED', 'V2', 'V3', 'V4', 'GQRA2', 
                                        'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4')))
  ) %>% as.data.frame()
  
  return(df)
}

add_truth <- function(df){
  truth <- read.csv(paste0(path_hub, "data-truth/truth-Cumulative Deaths.csv"),
                    colClasses = c(location = "character", date = "Date")) %>%
    rename(truth = value) %>% 
    select(-location_name)
  
  df <- df %>%
    left_join(truth, by=c("target_end_date"="date", "location"="location"))
  
  return(df)
}


df <- load_scores("scores/ensemble_scores_1wk.csv")
# df <- add_truth(df) # already contained in csv

results_long <- pivot_longer(subset(df, select=-c(ae)), cols=c("wgt_pen_u", "wgt_iw", "wgt_pen_l"),
                             names_to="penalty")

results_long <- pivot_longer(subset(individual_scores, select=-c(ae)), cols=c("wgt_pen_u", "wgt_iw", "wgt_pen_l"),
                             names_to="penalty")


plot_wis <- function(df_long, facet, x='window_size', window_sizes=1:4, locations='all', 
                     start_date='1900-01-01', end_date='3000-01-01',
                     ncol=4, dir='v'){
  x <- ensym(x)
  facet <- ensym(facet)
  
  # consider national or state level (or all)
  switch(locations,
         "national" = {
           df_long <- subset(df_long, location == 'US')
         },
         "states" = {
           df_long <- subset(df_long, location != 'US')
         }
         )
  
  # filter scores
  df_long <- df_long %>%
    filter(target_end_date >= start_date,
           target_end_date <= end_date,
           window_size %in% window_sizes#,
           #target %in% targets
           )
  
  xlabel <- str_to_title(str_replace_all(x, "_", " "))
  
  switch(locations,
         'all'={
           plot_title <- "WIS decomposition"
           },
         'states' = {
           plot_title <- "WIS decomposition (state level)"
           },
         'national' = {
           plot_title <- "WIS decomposition (national level)"
         })
  
  start_date <- min(df_long$target_end_date)
  end_date <- max(df_long$target_end_date)
  
  plot_title <- paste0(plot_title, " - from ", start_date, " until ", end_date)
  

  ggplot(df_long, 
         aes(x=!!x, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
    {if(!missing(facet)) facet_wrap(facet, ncol=ncol, dir=dir)} + # if facet is given
    geom_bar(position="stack", stat="identity") +
    theme(axis.text.x=element_text(angle=0, hjust=0)) +
    scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                      labels = c("Overprediction", "Dispersion", "Underprediction")) +
    labs(title= plot_title,
         x = xlabel,
         y = "WIS") +
    theme_grey(base_size=8)+
    theme(plot.title= element_text(size=9),
          axis.text = element_text(size = 4))
  }

plot_wis(results_long, x=window_size)
plot_wis(results_long, locations='states', x=window_size, facet="model")
plot_wis(results_long, locations='states', x=window_size, window_sizes=c(1, 4), facet="model")
plot_wis(results_long, locations='national', x=window_size, facet=model)
plot_wis(results_long, locations='national', x=window_size, facet=model, start_date='2020-08-08')
plot_wis(results_long, x=model, facet=location)
plot_wis(results_long, location='states', x=model, facet=location)
plot_wis(results_long, location='states', x=model, facet=location, start_date='2020-08-08', 
         ncol=8, dir='h')
plot_wis(results_long, location='all', x=model, facet=location, start_date='2020-08-08', 
         ncol=8, dir='h')
plot_wis(results_long, location='states', x=target_end_date, facet=model)






ggplot(data = subset(df, location == 'US'& target_end_date > '2020-08-01'), 
       aes(x = model, y = wis, fill=model)) +
  facet_wrap(~window_size) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "WIS by window size (national level)",
       x = "Model",
       y = "WIS")



ggplot(data = subset(individual_scores, location == 'US'), 
       aes(x = model, y = ae, fill=model)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "WIS by window size (national level)",
       x = "Model",
       y = "WIS")
