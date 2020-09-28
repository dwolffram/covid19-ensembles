# cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(length(levels_coverage) + 1)[-1]
cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1]

data <- subset(results, location=="US" & method=="MED" & window_size==4)
data <- subset(results, location=="US" & window_size==4)
data <- subset(results, method=="EWA" & window_size==4)

unique(df$model)
data <- subset(df, model=="CovidAnalytics-DELPHI")

data <- pivot_wider(data, names_from=quantile, names_prefix="value.", values_from=value)

ggplot(data, aes(x=target_end_date, y=truth)) +
  #facet_wrap(~method) +
  facet_wrap(~location, scales="free_y") +
  geom_smooth(aes(y = data$value.0.5, ymin = data$value.0.05, ymax = data$value.0.95), 
              linetype=2, size=0.5, fill=cols[2], alpha=1, stat = "identity") +
  geom_smooth(aes(y = data$value.0.5, ymin = data$value.0.25, ymax = data$value.0.75),
              linetype=2, size=0.5, fill=cols[1], alpha=1, stat = "identity") +
  geom_line() +
  geom_point() +
  geom_point(aes(y = data$value.0.5), pch = 21, col = "black", bg = "white") +
  theme_bw()

plot_forecast <- function(data){
  ggplot(data, aes(x=target_end_date, y=truth)) +
    #facet_wrap(~method) +
    facet_wrap(~location, scales="free_y") +
    geom_smooth(aes(y = data$value.0.5, ymin = data$value.0.05, ymax = data$value.0.95), 
                linetype=2, size=0.5, fill=cols[2], alpha=1, stat = "identity") +
    geom_smooth(aes(y = data$value.0.5, ymin = data$value.0.25, ymax = data$value.0.75),
                linetype=2, size=0.5, fill=cols[1], alpha=1, stat = "identity") +
    geom_line() +
    geom_point() +
    geom_point(aes(y = data$value.0.5), pch = 21, col = "black", bg = "white") +
    theme_bw()
}
plot_forecast(data)
