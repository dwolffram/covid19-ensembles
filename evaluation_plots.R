setwd("/home/dwolffram/covid19-ensembles")
library(tidyverse)
library(dplyr)
library(viridis)

results = read.csv('results/results_2020-09-23.csv',
                   colClasses = c(window_size = "factor", target_end_date = "Date"))

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
  geom_boxplot(width=0.1, color="grey", alpha=0.5, outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3, color="grey") +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none") +
  ylim(0, 100) +
  labs(title= "States",
       x = "Model",
       y = "WIS")

ggplot(data = subset(results, location == 'US' & window_size == 4), 
       aes(x = method, y = wis, fill=method)) +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "US",
       x = "Model",
       y = "WIS")

ggplot(data = subset(results, location == 'US'), 
       aes(x = method, y = wis, fill=method)) +
  facet_wrap(~window_size) +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "US",
       x = "Model",
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


df_rank <- wis_df %>% 
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
