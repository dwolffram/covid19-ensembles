setwd("/home/dwolffram/covid19-ensembles")
library(tidyverse)
library(dplyr)
results = read.csv('results/results3.csv',
                   colClasses = c(window_size = "factor", test_date = "Date"))

results_long <- results %>% 
  pivot_longer(names(results)[3:length(results)], names_to = "model", values_to = "wis")

ggplot(data = results_long, aes(x = test_date, y = wis, colour = model)) +
  geom_line() +
  facet_wrap(~window_size)

ggplot(data=results, aes(x=test_date, y=V2)) +
  geom_point()  +
  facet_wrap(~window_size) + 
  geom_segment( aes(x=test_date, xend=test_date, y=0, yend=V2))

results$median <- apply(results[3:length(results)], 1, median, na.rm = T)

plot_comparison <- function(model_name="EWA"){
  results %>% mutate(Color = ifelse(get(model_name)-median < 0, "green", "red")) %>%
    ggplot(aes(x=test_date, y=get(model_name)-median, color=Color)) +
    geom_point()  +
    facet_wrap(~window_size) + 
    geom_segment( aes(x=test_date, xend=test_date, y=0, yend=get(model_name)-median))+
    scale_color_identity() +
    ylab(paste(model_name, "-", "median"))
}

plot_comparison("EWA")
ggsave('plots/comp_EWA.png', width=24, height=12, dpi=500, unit='cm', device='png')
plot_comparison("V3")
ggsave('plots/comp_V3.png', width=24, height=12, dpi=500, unit='cm', device='png')
plot_comparison("GQRA2")
ggsave('plots/comp_GQRA2.png', width=24, height=12, dpi=500, unit='cm', device='png')
plot_comparison("V2")
ggsave('plots/comp_V2.png', width=24, height=12, dpi=500, unit='cm', device='png')


ggplot(data = results_long, aes(x = model, y = wis)) +
  #facet_wrap(~window_size) +
  geom_boxplot(outlier.shape=NA) +
  ylim(0, 100)
ggsave('plots/boxplot.png', width=24, height=12, dpi=500, unit='cm', device='png')


ggplot(data = results_long, aes(x = window_size, y = wis)) +
  facet_wrap(~model) +
  geom_boxplot(outlier.shape=NA) +
  ylim(0, 100)

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


bump_chart <- function(windowSize="1", highlight_models=unique(df_rank$model)){
  df_temp <- subset(df_rank, window_size==windowSize)
  dates <- sort(unique(df_temp$test_date))
  
  df_temp <- df_temp %>%
    mutate(flag = ifelse(model %in% highlight_models, TRUE, FALSE),
           model_col = if_else(flag == TRUE, model, "zzz"))
  
  pal <- gg_color_hue(length(unique(df_temp$model_col)) -1)
  pal[length(pal)+1] <- "grey"
  
  ggplot(data = df_temp, aes(x = test_date, y = ranking, group = model)) +
    geom_line(aes(color = model_col, alpha = 1), size = 2) +
    geom_point(color = "#FFFFFF", size = 4) +
    geom_point(aes(color = model_col, alpha = 1), size = 4) +
    geom_point(color = "#FFFFFF", size = 1) +
    scale_y_reverse(breaks = 1:nrow(df_temp)) +
    scale_x_date(breaks = unique(df_temp$test_date), 
                       minor_breaks = unique(df_temp$test_date), expand = c(.05, .05)) +
    geom_text(data = df_temp %>% filter(test_date == dates[1]),
              aes(label = model, x = dates[1]-1) , hjust = 1, fontface = "bold", 
              color = "#888888", size = 4) +
    geom_text(data = df_temp %>% filter(test_date == dates[length(dates)]),
              aes(label = model, x = dates[length(dates)]+1) , hjust = 0, fontface = "bold", 
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

bump_chart()
bump_chart("4", c("EWA","V3","QRA3", "GQRA3"))

bump_chart(1)
ggsave('plots/bump_chart_ws1.png', width=24, height=12, dpi=500, unit='cm', device='png')
bump_chart(2)
ggsave('plots/bump_chart_ws2.png', width=24, height=12, dpi=500, unit='cm', device='png')
bump_chart(3)
ggsave('plots/bump_chart_ws3.png', width=24, height=12, dpi=500, unit='cm', device='png')
bump_chart(4)
ggsave('plots/bump_chart_ws4.png', width=24, height=12, dpi=500, unit='cm', device='png')

bump_chart(4, c("EWA","V3","QRA3", "GQRA3"))

bump_chart(4, c("EWA"))
ggsave('plots/bump_chart_EWA.png', width=24, height=12, dpi=500, unit='cm', device='png')




