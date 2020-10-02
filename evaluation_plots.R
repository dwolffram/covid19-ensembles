setwd("/home/dwolffram/covid19-ensembles")
library(tidyverse)
library(dplyr)
library(viridis)

results = read.csv('results/results_2020-09-23.csv',
                   colClasses = c(window_size = "factor", target_end_date = "Date"))

results = read.csv('results/results_2020-09-26_train_without_us.csv',
                   colClasses = c(window_size = "factor", target_end_date = "Date"))

results %>%
  count(target_end_date)

a <- subset(results, target_end_date=="2020-08-08")

unique(a$window_size)
unique(a$method)

results_2020_09_23 %>%
  count(target_end_date)


subset(results, target_end_date == "2020-08-08")

mean_wis_df <- results %>%
  group_by(target_end_date, window_size, method) %>%
  summarize(mean_wis=mean(wis))

ggplot(data = mean_wis_df, aes(x = target_end_date, mean_wis, colour = method)) +
  geom_line() +
  facet_wrap(~window_size)

mean_wis_wide <- mean_wis_df %>%
  pivot_wider(names_from = method, values_from = mean_wis)


ggplot(data=mean_wis_wide, aes(x=target_end_date, y=V2)) +
  geom_point()  +
  facet_wrap(~window_size) + 
  geom_segment( aes(x=target_end_date, xend=target_end_date, y=0, yend=V2))

mean_wis_wide$median <- apply(mean_wis_wide[3:length(mean_wis_wide)], 1, median, na.rm = T)

plot_comparison <- function(model_name="MED", baseline="EWA"){
  mean_wis_wide %>% mutate(Color = ifelse(get(model_name)-get(baseline) < 0, "green", "red")) %>%
    ggplot(aes(x=target_end_date, y=get(model_name)-get(baseline), color=Color)) +
    geom_point()  +
    facet_wrap(~window_size) + 
    geom_segment( aes(x=target_end_date, xend=target_end_date, y=0, yend=get(model_name)-get(baseline)))+
    scale_color_identity() +
    ylab(paste(model_name, "-", baseline)) +
    xlab("Test Date")
}


plot_comparison("MED")
plot_comparison("V2")
plot_comparison("V3")
plot_comparison("V4")
plot_comparison("V2", "V3")


plot_comparison("EWA")
ggsave('plots/comp_EWA.png', width=24, height=12, dpi=500, unit='cm', device='png')
plot_comparison("V3")
ggsave('plots/comp_V3.png', width=24, height=12, dpi=500, unit='cm', device='png')
plot_comparison("GQRA2")
ggsave('plots/comp_GQRA2.png', width=24, height=12, dpi=500, unit='cm', device='png')
plot_comparison("V2")
ggsave('plots/comp_V2.png', width=24, height=12, dpi=500, unit='cm', device='png')


ggplot(data = mean_wis_df, aes(x = method, y = mean_wis)) +
  #facet_wrap(~window_size) +
  geom_boxplot(outlier.shape=NA) +
  #ylim(0, 100) +
  labs(x = "Model",
       y = "Mean WIS")
ggsave('plots/boxplot.png', width=24, height=14, dpi=500, unit='cm', device='png')

#results$window_size <- as.factor(results$window_size)

ggplot(data = subset(results, location == 'US'), aes(x = window_size, y = wis)) +
  facet_wrap(~method) +
  geom_boxplot(outlier.shape=NA) +
  #ylim(0, 500) +
  labs(x = "Model",
       y = "WIS")

ggplot(data = subset(results, location=='US'), aes(x = method, y = wis)) +
  #facet_wrap(~location, scales='free_y') +
  geom_boxplot(outlier.shape=NA) +
  #ylim(0, 500) +
  labs(x = "Model",
       y = "WIS")

ggplot(data = subset(results, location == 'US' & window_size == 4), 
       aes(x = method, y = wis, fill=method)) +
  geom_violin(width=1) +
  geom_boxplot(width=0.1, color="grey", alpha=0.5, outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3, color="grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none") +
  labs(title= "US",
       x = "Model",
       y = "WIS")

ggplot(data = subset(results, location != 'US' & window_size == 4), 
       aes(x = method, y = wis, fill=method)) +
  geom_violin(width=1) +
  geom_boxplot(width=0.1, color="grey", alpha=0.5) +
  stat_summary(fun.y=mean, geom="point", shape=3, color="grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none") +
  #ylim(0, 100) +
  labs(title= "States",
       x = "Model",
       y = "WIS")


ggplot(data = subset(results, window_size == 4), 
       aes(x = method, y = wis, fill=method)) +
  facet_wrap(~location, scales="free") +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "US and States - Window Size 4",
       x = "Model",
       y = "WIS")

ggplot(data = subset(results, location == 'US' & window_size == 4), 
       aes(x = method, y = wis, fill=method)) +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "US - Window Size 4",
       x = "Model",
       y = "WIS")

ggplot(data = subset(results, location != 'US' & window_size == 4), 
       aes(x = method, y = wis, fill=method)) +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  ylim(0, 50) +
  theme(legend.position = "none") +
  labs(title= "States - Window Size 4",
       x = "Model",
       y = "WIS")

ggplot(data = subset(results, location == 'US'), 
       aes(x = method, y = wis, fill=method)) +
  facet_wrap(~window_size) +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "WIS by window size (national level)",
       x = "Model",
       y = "WIS")

results$window_size <- as.factor(results$window_size)

ggplot(data = subset(results, location == 'US'), 
       aes(x = window_size, y = wis, fill=window_size)) +
  facet_wrap(~method) +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "WIS by ensemble method (national level)",
       x = "Window Size",
       y = "WIS")

ggplot(data = subset(results, location != 'US'), 
       aes(x = window_size, y = wis, fill=window_size)) +
  facet_wrap(~method) +
  geom_violin(width=1) +
  geom_boxplot(width=0.1, color="grey", alpha=0.5) +  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  #ylim(0, 50) +
  theme(legend.position = "none") +
  labs(title= "States",
       x = "Window Size",
       y = "WIS")

ggplot(data = subset(results, location != 'US'), 
       aes(x = window_size, y = wis, fill=window_size)) +
  facet_wrap(~method) +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  ylim(0, 200) +
  theme(legend.position = "none") +
  labs(title= "States",
       x = "Window Size",
       y = "WIS")

ggplot(data = subset(results, location != 'US' & window_size == 4), 
       aes(x = method, y = wis)) +
  geom_boxplot(outlier.shape=NA)+
  stat_summary(fun.y=mean, geom="point", shape=4) +
  ylim(0, 100) +
  labs(title = "States",
       x = "Model",
       y = "WIS")

ggplot(data = results, aes(x = window_size, y = wis)) +
  facet_wrap(~method) +
  geom_boxplot(outlier.shape=NA) +
  #ylim(0, 100) +
  labs(x = "Model",
       y = "WIS")

ggsave('plots/boxplot.png', width=24, height=14, dpi=500, unit='cm', device='png')


ggplot(data = results_long, aes(x = window_size, y = wis)) +
  facet_wrap(~model) +
  geom_boxplot(outlier.shape=NA) +
  ylim(0, 100) +
  labs(x = "Window Size",
       y = "WIS")

ggsave('plots/boxplot_windowSizes.png', device='png')

#### Bump Chart

my_theme <- function() {
  
  # Colors
  color.background = "white"
  color.text = "#22211d"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "none") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


df_rank <- results_long %>% 
  group_by(window_size, test_date) %>%
  arrange(window_size, test_date, wis) %>% 
  mutate(ranking = row_number())


mean_wis_us <- results %>%
  subset(location=="US") %>%
  group_by(target_end_date, window_size, method) %>%
  summarize(mean_wis=mean(wis))


ggplot(data = mean_wis_us, aes(x = target_end_date, mean_wis, colour = method)) +
  geom_line() +
  facet_wrap(~window_size)

ggplot(data = subset(mean_wis_us, window_size==4), aes(x = target_end_date, mean_wis, colour = method)) +
  geom_line() 

df_rank_us <- mean_wis_us %>% 
  group_by(window_size, target_end_date) %>%
  arrange(window_size, target_end_date, mean_wis) %>%
  mutate(ranking = row_number())


df_rank <- mean_wis_df %>% 
  group_by(window_size, target_end_date) %>%
  arrange(window_size, target_end_date, mean_wis) %>%
  mutate(ranking = row_number())

#df_rank <- rename(df_rank, c(test_date=target_end_date, model=method))



bump_chart <- function(df_rank, windowSize="1", highlight_models=unique(df_rank$method)){
  df_temp <- subset(df_rank, window_size==windowSize)
  dates <- sort(unique(df_temp$target_end_date))
  
  df_temp <- df_temp %>%
    mutate(flag = ifelse(method %in% highlight_models, TRUE, FALSE),
           model_col = if_else(flag == TRUE, as.character(method), "zzz"))
  
  pal <- gg_color_hue(length(unique(df_temp$model_col)) -1)
  pal[length(pal)+1] <- "grey"
  
  ggplot(data = df_temp, aes(x = target_end_date, y = ranking, group = method)) +
    geom_line(aes(color = model_col, alpha = 1), size = 2) +
    geom_point(color = "#FFFFFF", size = 4) +
    geom_point(aes(color = model_col, alpha = 1), size = 4) +
    geom_point(color = "#FFFFFF", size = 1) +
    scale_y_reverse(breaks = 1:nrow(df_temp)) +
    scale_x_date(breaks = unique(df_temp$target_end_date), 
                       minor_breaks = unique(df_temp$target_end_date), expand = c(.05, .05)) +
    geom_text(data = df_temp %>% filter(target_end_date == dates[1]),
              aes(label = method, x = dates[1]-1) , hjust = 1, fontface = "bold", 
              color = "#888888", size = 4) +
    geom_text(data = df_temp %>% filter(target_end_date == dates[length(dates)]),
              aes(label = method, x = dates[length(dates)]+1) , hjust = 0, fontface = "bold", 
              color = "#888888", size = 4) +
    coord_cartesian(xlim = c(dates[1]-4,dates[length(dates)]+4)) +
    theme(legend.position = "none") +
    labs(x = "Test Date",
         y = "Rank",
         title = "Comparison of Ensemble Methods",
         subtitle = paste("For Window Size", windowSize, "(Ranked by WIS)")) +
    my_theme() +
    scale_color_manual(values = pal)
}

bump_chart(df_rank_us, 4, c("V2", "V3","V4", "GQRA3", "QRA4"))
bump_chart(df_rank, 4)

bump_chart(1)
bump_chart("4")

bump_chart("4", c("EWA","MED", "V3","QRA2", "GQRA3"))

bump_chart(1)
ggsave('plots/bump_chart_ws1.png', width=24, height=14, dpi=500, unit='cm', device='png')
bump_chart(2)
ggsave('plots/bump_chart_ws2.png', width=24, height=14, dpi=500, unit='cm', device='png')
bump_chart(3)
ggsave('plots/bump_chart_ws3.png', width=24, height=14, dpi=500, unit='cm', device='png')
bump_chart(4)
ggsave('plots/bump_chart_ws4.png', width=24, height=14, dpi=500, unit='cm', device='png')

bump_chart(4, c("EWA","V3","QRA3", "GQRA3"))

bump_chart(4, c("EWA"))
ggsave('plots/bump_chart_EWA.png', width=24, height=14, dpi=500, unit='cm', device='png')



bump_chart(4, c("EWA","V3","QRA3", "GQRA3"))


### Truth Plots
truth_us <- results %>%
  subset(location=="US") %>%
  select(c(target_end_date, truth)) %>%
  distinct()

ggplot(truth_us, aes(x=target_end_date, y=truth)) +
  geom_line() +
  labs(x="Date", y="Cumulative Deaths", title="Cumulative Deaths in the US")


truth_df <- results %>%
  #subset(location!="US") %>%
  select(c(location, target_end_date, truth)) %>%
  distinct()

ggplot(truth_df, aes(x=target_end_date, y=truth)) +
  facet_wrap(~location, scales='free') +
  #facet_wrap(~location) +
  geom_line() +
  labs(x="Date", y="Cumulative Deaths", title="Cumulative Deaths by State")


### WIS Decomposition

# create an empty plot to which forecasts can be added:
empty_plot <- function(xlim, ylim, xlab, ylab){
  plot(NULL, xlim = xlim, ylim = ylim,
       xlab = xlab, ylab = "", axes = FALSE)
  axis(2, las = 1)
  title(ylab = ylab, line = 4)
  all_dates <- seq(from = as.Date("2020-02-01"), to = Sys.Date() + 28, by  =1)
  saturdays <- all_dates[weekdays(all_dates) == "Saturday"]
  axis(1, at = saturdays, labels = as.Date(saturdays, origin = "1970-01-01"))
  box()
}

plot_scores <- function(wis_tab, method, location, 
                        xlim = c(as.Date("2020-04-01"), 
                        Sys.Date() + 28), ylim = c(0, 5000)){
  
  # subset to chosen target:
  wis_tab <- wis_tab[(wis_tab$method == method) & (wis_tab$location == location), ]

  empty_plot(xlab = "target end date", xlim = xlim, ylim = ylim, 
             ylab = "WIS")
  
  lines(wis_tab$target_end_date, wis_tab$wis, type = "h", col = "red", lwd = 2)
  lines(wis_tab$target_end_date, wis_tab$wgt_pen_l + wis_tab$wgt_iw, type = "h", col = "royalblue", lwd = 2)
  lines(wis_tab$target_end_date, wis_tab$wgt_pen_l, type = "h", col = "orange", lwd = 2)
  # points(wis_tab$target_end_date, wis_tab$wis, pch = 1, lwd = 2)
  
  legend("topright", legend = c("penalty for underprediction",
                                "forecast dispersion",
                                "penalty for overprediction"),
         col = c("red", "royalblue1", "orange"), lwd = c(2, 2, 2, NA),
         bty = "n", cex = 0.9, pch = c(NA, NA, NA, 5))
  
  title(main=paste(method, location, sep=' - '))
}

plot_scores(results, 'QRA4', '36', ylim = c(0, 1300))

### WIS DECOMPOSITION NEW
results_long <- pivot_longer(results, cols=c("wgt_pen_u", "wgt_iw", "wgt_pen_l"),
                             names_to="penalty")

ggplot(subset(results_long, location=='US'), 
       aes(x=window_size, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~method) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=0,hjust=0)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition (national level)",
       x = "Window Size",
       y = "WIS")

ggplot(subset(results_long, location=='US'& target_end_date > '2020-08-01'), 
       aes(x=window_size, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~method) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=0,hjust=0)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition (national level) - after data revision from 2020-08-01",
       x = "Window Size",
       y = "WIS") #+
  #ylim(0, 160000)

ggplot(subset(results_long, window_size==4), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~location, scales="free_y") +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS Decomposition by State and Ensemble Method",
       x = "Model",
       y = "Relative Contribution to WIS")

ggplot(subset(results_long, location!="US" & value >= 0 & window_size == 4), 
       aes(x=target_end_date, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~method, scales="free_y") +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition on state level (window size 4)",
       x = "Model",
       y = "WIS")

ggplot(subset(results_long, location!="US" & window_size == 4), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  #facet_wrap(~target_end_date, scales='free_y') +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition on state level (window size 4)",
       x = "Model",
       y = "WIS")# +
  #ylim(0, 10000)


ggplot(subset(results_long, location=="US" & target_end_date > '2020-08-01' & window_size == 4), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~target_end_date, scales='free_y') +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition on state level (window size 4)",
       x = "Model",
       y = "WIS")

# PIT HISTOGRAMS

pit_histogram <- function(df){
  df <- df %>%
    select(method | location | target_end_date | starts_with('value') | truth)
  df <- data.frame(1*sapply(subset(df, select=-c(method, location, target_end_date, truth)), 
                            function(x) x >= df$truth))
  for (i in rev(2:(ncol(df)))){
    df[, i] = (df[, i] - df[, i-1])
  }
  
  df$value.1 <- 1- rowSums(df)
  
  df %>%
    group_by(method) %>%
    summarise_all(sum)

}

df <- results


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


#a <- df[rowSums(df<0)>=1 , ]

df <- cbind(ids, df)

df <- df %>%
  select(-c(location, target_end_date)) %>%
  group_by(method) %>%
  summarise_all(mean)

df <- df %>%
  mutate(value.0.05 = value.0.01 + value.0.025 + value.0.05) %>%
  mutate(value.1 = value.1 + value.0.99 + value.0.975) %>%
  select(-c(value.0.01, value.0.025, value.0.99, value.0.975))

df
df_long <- pivot_longer(df, !method)
quantiles <- seq(0.05, 1, 0.05)
df_long$quantile <- rep(quantiles, length(unique(df_long$method)))

df_long

width <- 0.05
ggplot(data=df_long, aes(x=quantile, y=value/width, width=width)) +
  facet_wrap(~method) +
  geom_bar(stat="identity", position = position_nudge(x = -width/2),
           fill=viridis(3)[2]) +
  labs(x='Probability Integral Transform',
       y='Density') +
  geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color=viridis(3)[1])




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
pit_histogram(subset(results, location=='US' & method=='GQRA4' & window_size==4))
View(results[545, ])

quantiles <- c(get_quantile_levels(), 1)
widths <- c(0.01, diff(quantiles))


a <- subset(results, method=='GQRA3')
pit_histogram(a)

a <- subset(results, location=='US' & method=='QRA3')
a <- a %>%
  select(starts_with('value') | truth)

b <- data.frame(1*sapply(subset(a, select=-c(truth)), function(x) x >= a$truth))

#c <- apply(a, 1, function(x) cut(x[length(x)], breaks=x[1:length(x)-1] + runif(length(x)-1, -0.01, 0.01)))
for (i in rev(2:(ncol(b)))){
  b[, i] = (b[, i] - b[, i-1])
}

b$value.1 <- 1- rowSums(b)

c <- data.frame(colSums(b, na.rm = TRUE))
#c <- data.frame(c)
colnames(c) <- 'value'

quantiles <- c(get_quantile_levels(), 1)
               
c$quantile <- quantiles

c[3, 1] <- sum(head(c$value, 3))
c[nrow(c), 1] <- sum(tail(c$value), 3)
c[-c(1, 2, nrow(c)-1, nrow(c)-2), ]


width <- 0.05
ggplot(data=c, aes(x=quantile, y=value, width=width)) +
  geom_bar(stat="identity", position = position_nudge(x = -c(0.01, diff(quantiles))/2)) +
  labs(x='Probability Integral Transform',
       y='Relative Frequency')

ggplot(data=c, aes(x=quantile-0.01, y=value)) +
  geom_step(stat="identity")

a$bin <- apply(a, 1, function(x) cut(x[length(x)], breaks=x[1:length(x)-1] + runif(length(x)-1, -0.001, 0.001)))
ggplot(a, aes(x=bin)) + geom_bar()




coverage_plot <- function(wis_tab){

  coverage_levels <- c(0:9/10, 0.95, 0.98)
  emp_coverage <- numeric(length(coverage_levels))
  for(i in seq(coverage_levels)){
    emp_coverage[i] <-
      mean(
        wis_tab$truth < wis_tab[, paste0("value.", 1 - (1 - coverage_levels[i])/2)] &
          wis_tab$truth > wis_tab[, paste0("value.", (1 - coverage_levels[i])/2)],
        na.rm = TRUE
      )
  }
  plot(coverage_levels, emp_coverage, type = "l", xlab = "nominal coverage of PI",
       ylab = "empirical coverage", xlim = 0:1, ylim = 0:1)
  abline(0:1, lty = 2)
}

coverage_plot(subset(results, location=='US' & method=='QRA3'))


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


coverage_plot(subset(results, location=='US' & method=='V4' & target_end_date > '2020-08-01'))
coverage_plot(subset(results, location=='US' & method=='QRA3'))
