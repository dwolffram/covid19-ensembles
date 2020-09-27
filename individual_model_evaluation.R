test_dates <- as.list(possible_dates[(4+1):length(possible_dates)])
df_test <- subset(df, target_end_date %in% test_dates)

results <- wis_table(df_test)

mean_wis_df <- results %>%
  group_by(target_end_date, model) %>%
  summarize(mean_wis=mean(wis))

ggplot(data = mean_wis_df, aes(x = target_end_date, y = mean_wis, colour = model)) +
  geom_line() 

ggplot(data = results, aes(x = target_end_date, y = wis, colour = model)) +
  facet_wrap(~location, scales="free") +
  geom_line() +
  labs(title= "WIS by State over Time",
       x = NULL,
       y = "WIS",
       colour = "Model")



ggplot(data = subset(results, location == 'US'), 
       aes(x = model, y = wis, fill=model)) +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "US",
       x = "Model",
       y = "WIS")

ggplot(data = subset(results, location != 'US'), 
       aes(x = model, y = wis, fill=model)) +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "US",
       x = "Model",
       y = "WIS")

ggplot(data = results, 
       aes(x = model, y = wis, fill=model)) +
  facet_wrap(~location, scales="free") +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  #theme(legend.position = "none") +
  labs(title= "US and States",
       x = "Model",
       y = "WIS") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  #theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


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

plot_scores <- function(wis_tab, model, location, 
                        xlim = c(as.Date("2020-06-01"), 
                                 Sys.Date() + 28), ylim = c(0, 5000)){
  
  # subset to chosen target:
  wis_tab <- wis_tab[(wis_tab$model == model) & (wis_tab$location == location), ]
  
  empty_plot(xlab = "target end date", xlim = xlim, ylim = ylim, 
             ylab = "WIS")
  
  lines(wis_tab$target_end_date, wis_tab$wis/wis_tab$wis, type = "h", col = "red", lwd = 5)
  lines(wis_tab$target_end_date, (wis_tab$wgt_pen_l + wis_tab$wgt_iw)/wis_tab$wis, type = "h", col = "royalblue", lwd = 5)
  lines(wis_tab$target_end_date, (wis_tab$wgt_pen_l)/wis_tab$wis, type = "h", col = "orange", lwd = 5)
  #points(wis_tab$target_end_date, wis_tab$wis, pch = 1, lwd = 2)
  
  legend("topright", legend = c("Underprediction",
                                "Dispersion",
                                "Overprediction"),
         title = "Penalty for:",
         col = c("red", "royalblue1", "orange"), lwd = c(2, 2, 2, NA),
         bty = "n", cex = 0.9, pch = c(NA, NA, NA, 5))
  
  title(main=paste(model, location, sep=' - '))
}

unique(results$model)
# [1] "UCLA-SuEIR"            "YYG-ParamSearch"       "MOBS-GLEAM_COVID"      "CovidAnalytics-DELPHI"
# [5] "LANL-GrowthRate"       "COVIDhub-baseline" 
plot_scores(results, 'YYG-ParamSearch', 'US', ylim = c(0, 1.1))


plot_scores <- function(wis_tab, model, location, normalize = FALSE, ylim = c(0, 5000)){
  
  # subset to chosen model and location:
  wis_tab <- wis_tab[(wis_tab$model == model) & (wis_tab$location == location), ]
  
  dates <- unique(wis_tab$target_end_date)
  
  xlim <- c(dates[1] - 7, dates[length(dates)] + 35)
  ylim <- c(0, 1.2 * max(wis_tab$wis))

  ytop1 <- wis_tab$wgt_pen_u
  ytop2 <- ytop1 + wis_tab$wgt_iw
  ytop3 <- ytop2 + wis_tab$wgt_pen_l
  
  if(normalize){
    ylim = c(0, 1.1)
    ytop1 <- ytop1/wis_tab$wis
    ytop2 <- ytop2/wis_tab$wis
    ytop3 <- ytop3/wis_tab$wis
    
  }
  
  empty_plot(xlab = NA, 
             xlim = xlim, 
             ylim = ylim, 
             ylab = "WIS")
  
  width=4
  
  # underprediction
  rect(xleft = wis_tab$target_end_date - width/2, ybottom = 0, 
       xright = wis_tab$target_end_date + width/2, 
       ytop = ytop1, col = viridis(3)[3])
  
  # dispersion
  rect(xleft = wis_tab$target_end_date - width/2, ybottom = ytop1, 
       xright = wis_tab$target_end_date + width/2, 
       ytop = ytop2, col = viridis(3)[2])
  
  # overprediction
  rect(xleft = wis_tab$target_end_date - width/2, ybottom = ytop2, 
       xright = wis_tab$target_end_date + width/2, 
       ytop = ytop3, col = viridis(3)[1])
  
  
  legend("topright", legend = c("Underprediction",
                                "Dispersion",
                                "Overprediction"),
         title = "Penalty for:",
         col = viridis(3)[order(c(3, 2, 1))], lwd = c(4, 4, 4, NA),
         bty = "n", cex = 0.9, pch = c(NA, NA, NA, 5))
  

  title(main=paste(model, location, sep=' - '))
}
plot_scores(results, "CovidAnalytics-DELPHI", "34", normalize=TRUE)


results_long <- pivot_longer(results, cols=c("wgt_pen_u", "wgt_iw", "wgt_pen_l"),
                             names_to="penalty")


ggplot(results_long, aes(x=model, y=value, 
                         fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) + 
  facet_wrap(~location) +
  geom_bar(position="fill", stat="identity")

ggplot(subset(results_long, location=="US"), 
       aes(x=model, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) + 
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "US",
       x = "Model",
       y = "WIS")

ggplot(results_long, 
       aes(x=model, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~location, scales="free") +
  geom_bar(position="fill", stat="identity") +
  #theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "US",
       x = "Model",
       y = "WIS")

ggplot(results_long, 
       aes(x=location, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~model, scales="free") +
  geom_bar(position="fill", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "US",
       x = "Model",
       y = "WIS")

ggplot(subset(results_long, location!="US"), 
       aes(x=model, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "States",
       x = "Model",
       y = NULL)

unique(results_long$penalty)

ggplot(results_long, aes(fill=penalty, y=value, x=model)) + 
  facet_wrap(~location, scales="free") +
  stat_summary(fun.y=sum,geom="bar",fill=penalty,colour="black")
  
ggplot(subset(results_long, location=="US"), 
       aes(x=model, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~target_end_date) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition by date on national level",
       x = "Model",
       y = "WIS")
