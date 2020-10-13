pit_histogram <- function(df){
  df <- df %>%
    select(starts_with('value') | truth)
  
  df <- data.frame(1*sapply(subset(df, select=-c(truth)), 
                            function(x) x >= df$truth))
  for (i in rev(2:(ncol(df)))){
    df[, i] = (df[, i] - df[, i-1])
  }
  
  df$value.1 <- 1- rowSums(df)
  
  df <- data.frame(colMeans(df))
  colnames(df) <- 'value'
  
  quantiles <- c(get_quantile_levels(), 1)
  
  df$quantile <- quantiles
  widths <- c(0.01, diff(quantiles))
  ggplot(data=df, aes(x=quantile, y=value/widths, width=widths)) +
    geom_bar(stat="identity", position = position_nudge(x = -c(0.01, diff(quantiles))/2),
             fill=viridis(3)[2]) +
    labs(x='Probability Integral Transform',
         y='Density') +
    geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color=viridis(3)[1])
  
  
}

pit_histogram(subset(results, location=='US' & method=='EWA'))


#combine outer bins
pit_histogram <- function(df){
  df <- df %>%
    select(starts_with('value') | truth)
  
  df <- data.frame(1*sapply(subset(df, select=-c(truth)), 
                            function(x) x >= df$truth))
  for (i in rev(2:(ncol(df)))){
    df[, i] = (df[, i] - df[, i-1])
  }
  
  df$value.1 <- 1- rowSums(df)
  
  df <- data.frame(colMeans(df))
  colnames(df) <- 'value'
  
  quantiles <- c(get_quantile_levels(), 1)
  
  df$quantile <- quantiles
  
  
  df[3, 1] <- sum(head(df$value, 3))
  df[nrow(df), 1] <- sum(tail(df$value, 3))
  df <- df[-c(1, 2, nrow(df)-1, nrow(df)-2), ]
  
  width <- 0.05
  ggplot(data=df, aes(x=quantile, y=value/width, width=width)) +
    geom_bar(stat="identity", position = position_nudge(x = -width/2),
             fill=viridis(3)[2]) +
    labs(x='Probability Integral Transform',
         y='Density') +
    geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color=viridis(3)[1])
  
  
}

pit_histogram(subset(results, location=='US' & method=='GQRA3' & target_end_date > '2020-08-01'))
pit_histogram(subset(results_new, location=='US' & method=='GQRA4' & window_size==4))
pit_histogram(subset(results, location=='US' & method=='EWA'))

### ALL METHODS, 10 BINS

plot_pit_histograms <- function(df){
  ids <- df %>%
    select(method | location | target_end_date)
  
  df <- df %>%
    select(starts_with('value') | truth)
  df <- data.frame(1*sapply(subset(df, select=-c(truth)), 
                            function(x) x >= df$truth))
  for (i in rev(2:(ncol(df)))){
    df[, i] = (df[, i] - df[, i-1])
  }
  
  df$value.1 <- 1- rowSums(df)
  
  df <- cbind(ids, df)
  
  df <- df %>%
    select(-c(location, target_end_date)) %>%
    group_by(method) %>%
    summarise_all(mean)
  
  df <- df %>%
    mutate(value.0.05 = value.0.01 + value.0.025 + value.0.05) %>%
    mutate(value.1 = value.1 + value.0.99 + value.0.975) %>%
    select(-c(value.0.01, value.0.025, value.0.99, value.0.975))
  
  df[, seq(3, ncol(df), 2)] = (df[, seq(3, ncol(df), 2)] + df[, seq(2, ncol(df), 2)])
  df <- df[, seq(1, ncol(df), 2)]
  
  df_long <- pivot_longer(df, !method)
  quantiles <- seq(0.1, 1, 0.1)
  
  df_long$quantile <- rep(quantiles, length(unique(df_long$method)))
  
  width <- 0.1
  ggplot(data=df_long, aes(x=quantile, y=value/width, width=width)) +
    facet_wrap(~method, dir='v', ncol=4) +
    geom_bar(stat="identity", position = position_nudge(x = -width/2),
             fill=viridis(3)[2]) +
    labs(x='Probability Integral Transform',
         y='Density') +
    geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color=viridis(3)[1]) +
    theme_grey(base_size=8)+
    theme(plot.title= element_text(size=9),
               axis.text = element_text(size = 5)) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels=c("0", "0.25", "0.5", "0.75", "1"))
}

plot_pit_histograms(subset(results, location!='US' & target_end_date>'2020-08-01')) #+ 
#theme(text=element_text(size=12))

plot_pit_histograms(subset(individual_results)) #+ 
ggsave('plots/individual_models/individual_pits_all.png', width=11, height=7, dpi=500, unit='cm', device='png')

plot_pit_histograms(subset(individual_results, target_end_date>'2020-08-01')) #+ 
ggsave('plots/individual_models/individual_pits_after_0801_all.png', width=11, height=7, dpi=500, unit='cm', device='png')


ggsave('plots/ensemble_methods/ensemble_pits_after_0801_states.png', width=11, height=7, dpi=500, unit='cm', device='png')


plot_pit_histograms(subset(results, location!='US'))
plot_pit_histograms(subset(results, window_size==2 & target_end_date > '2020-08-01')) +
  labs(title='After data revision on 2020-08-01 (window size 4)')
plot_pit_histograms(subset(results, window_size==4 & location!='US' & target_end_date > '2020-08-01'))
plot_pit_histograms(subset(results, target_end_date > '2020-08-01'))


plot_pit_histograms_ws <- function(df){
  ids <- df %>%
    select(method | location | target_end_date | window_size)
  
  df <- df %>%
    select(starts_with('value') | truth)
  df <- data.frame(1*sapply(subset(df, select=-c(truth)), 
                            function(x) x >= df$truth))
  for (i in rev(2:(ncol(df)))){
    df[, i] = (df[, i] - df[, i-1])
  }
  
  df$value.1 <- 1- rowSums(df)
  
  df <- cbind(ids, df)
  
  df <- df %>%
    select(-c(location, target_end_date)) %>%
    group_by(method, window_size) %>%
    summarise_all(mean)
  
  df <- df %>%
    mutate(value.0.05 = value.0.01 + value.0.025 + value.0.05) %>%
    mutate(value.1 = value.1 + value.0.99 + value.0.975) %>%
    select(-c(value.0.01, value.0.025, value.0.99, value.0.975))
  

  df[, seq(4, ncol(df), 2)] = (df[, seq(4, ncol(df), 2)] + df[, seq(3, ncol(df), 2)])
  df <- df[, c(1, 2, seq(4, ncol(df), 2))]
  
  df_long <- pivot_longer(df, !c(method, window_size))
  quantiles <- seq(0.1, 1, 0.1)
  
  df_long$quantile <- rep(quantiles, length(unique(df_long$method))*length(unique(df_long$window_size)))
  
  width <- 0.1
  ggplot(data=df_long, aes(x=quantile, y=value/width, width=width)) +
    facet_grid(method~window_size) +
    geom_bar(stat="identity", position = position_nudge(x = -width/2),
             fill=viridis(3)[2]) +
    labs(x='Probability Integral Transform',
         y='Density',
         title='PIT Histograms by Window Size and Ensemble Method') +
    geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color=viridis(3)[1])
}

plot_pit_histograms_ws(subset(results, location!='US' & target_end_date > '2020-08-01')) +
  labs(title='PIT Histograms by Window Size and Ensemble Method - after 2020-08-01')
plot_pit_histograms_ws(results)

### QUANTILE COVERAGE

coverage_plot <- function(df){
  df_temp <- df %>%
    select(starts_with('value') | truth)
  
  df_temp <- data.frame(1*sapply(subset(df_temp, select=-c(truth)), 
                                 function(x) x >= df$truth))
  
  #df_temp$value.1 <- 1*(df$value.0.99 < df$truth)
  
  df_temp <- data.frame(colMeans(df_temp))
  colnames(df_temp) <- 'value'
  
  quantiles <- c(get_quantile_levels())
  
  df_temp$quantile <- quantiles
  #widths <- c(0.01, diff(quantiles))
  ggplot(data=df_temp, aes(x=quantile, y=value)) +
    geom_line(stat="identity",
              color=viridis(3)[2]) +
    labs(x='Quantile',
         y='Observed Relative Frequency') +
    geom_segment(aes(x=0,xend=1,y=0,yend=1), linetype="dashed", color=viridis(3)[1])
  
  
}

pit_histogram(subset(results, location=='US' & method=='V4' & target_end_date > '2020-08-01'))
coverage_plot(subset(results, location=='US' & method=='V4' & target_end_date > '2020-08-01'))
coverage_plot(subset(results, location=='US' & method=='QRA3'))
