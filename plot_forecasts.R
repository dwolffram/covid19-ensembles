# cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(length(levels_coverage) + 1)[-1]
cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1]

data <- subset(results, location=="US" & method=="MED" & window_size==4)
data <- subset(results, location=="34" & window_size==4)
data <- subset(results, method=="V3" & window_size==4)
data <- subset(results, location=="US" & window_size==4)


unique(df$model)
data <- subset(df, model=="YYG-ParamSearch")
data <- subset(df, location=="US")

data <- pivot_wider(data, names_from=quantile, names_prefix="value.", values_from=value)

ggplot(data, aes(x=target_end_date, y=truth)) +
  facet_wrap(~method) +
  #facet_wrap(~location, scales="free_y") +
  geom_smooth(aes(y = data$value.0.5, ymin = data$value.0.05, ymax = data$value.0.95), 
              linetype=2, size=0.5, fill=cols[2], alpha=1, stat = "identity") +
  geom_smooth(aes(y = data$value.0.5, ymin = data$value.0.25, ymax = data$value.0.75),
              linetype=2, size=0.5, fill=cols[1], alpha=1, stat = "identity") +
  geom_line() +
  geom_point() +
  geom_point(aes(y = data$value.0.5), pch = 21, col = "black", bg = "white") +
  labs(title= "QRA3",
       x = "Date",
       y = "Cumulative Deaths")
  theme_bw()

plot_forecast <- function(data){
  ggplot(data, aes(x=target_end_date, y=truth)) +
    facet_wrap(~method) +
    #facet_wrap(~location, scales="free_y") +
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

plot_forecast <- function(data, incidence=FALSE, title=NULL){
  if (incidence){
    data <- data %>% 
      #group_by(method, scales="free_y") %>%
      group_by(location_name) %>%
      mutate(value.0.5 = value.0.5 - lag(truth)) %>%
      mutate(value.0.05 = value.0.05 - lag(truth)) %>%
      mutate(value.0.95 = value.0.95 - lag(truth)) %>%
      mutate(value.0.25 = value.0.25 - lag(truth)) %>%
      mutate(value.0.75 = value.0.75 - lag(truth)) %>%
      mutate(truth=c(NA, diff(truth)))
  }
  ggplot(data, aes(x=target_end_date, y=truth)) +
    #facet_wrap(~method) +
    facet_wrap(~location_name, scales="free_y") +
    geom_smooth(aes(y = data$value.0.5, ymin = data$value.0.05, ymax = data$value.0.95), 
                linetype=2, size=0.5, fill=cols[2], alpha=1, stat = "identity") +
    geom_smooth(aes(y = data$value.0.5, ymin = data$value.0.25, ymax = data$value.0.75),
                linetype=2, size=0.5, fill=cols[1], alpha=1, stat = "identity") +
    geom_line() +
    geom_point() +
    geom_point(aes(y = data$value.0.5), pch = 21, col = "black", bg = "white") +
    theme_bw() +
    #theme(axis.text.x=element_text(angle=45,hjust=1)) +
    labs(title=title,
         x = "Date",
         y = "Deaths")
}

data <- subset(individual_results, location=="US")
data <- subset(results, location=="US" & window_size==4)

data <- subset(individual_results, method=="COVIDhub-baseline")
data <- subset(individual_results, method=="YYG-ParamSearch")

plot_forecast(data, incidence=TRUE, 
              title="1 week ahead individual forecasts with 50%- and 90%-prediction intervals (national level)")
  #theme(text = element_text(size = 25)) 

plot_forecast(data, incidence=TRUE, 
              title="YYG-ParamSearch forecasts with 50%- and 90%-prediction intervals (national level)")

data <- subset(results, location=="US" & window_size==4)

data$value.0.5 = data$value.0.5 - lag(data$truth)
data$value.0.05 = data$value.0.05 - lag(data$truth)
data$value.0.95 = data$value.0.95 - lag(data$truth)
data$value.0.25 = data$value.0.25 - lag(data$truth)
data$value.0.75 = data$value.0.75 - lag(data$truth)

data <- data %>% 
  group_by(method) %>%
  mutate(value.0.5 = value.0.5 - lag(truth)) %>%
  mutate(value.0.05 = value.0.05 - lag(truth)) %>%
  mutate(value.0.95 = value.0.95 - lag(truth)) %>%
  mutate(value.0.25 = value.0.25 - lag(truth)) %>%
  mutate(value.0.75 = value.0.75 - lag(truth)) %>%
  mutate(truth=c(NA, diff(truth)))
#data$truth <- c(NA, diff(data$truth))

ggplot(data, aes(x=target_end_date, y=truth)) +
  facet_wrap(~method) +
  #facet_wrap(~location, scales="free_y") +
  geom_smooth(aes(y = data$value.0.5, ymin = data$value.0.05, ymax = data$value.0.95), 
              linetype=2, size=0.5, fill=cols[2], alpha=1, stat = "identity") +
  geom_smooth(aes(y = data$value.0.5, ymin = data$value.0.25, ymax = data$value.0.75),
              linetype=2, size=0.5, fill=cols[1], alpha=1, stat = "identity") +
  geom_line() +
  geom_point() +
  geom_point(aes(y = data$value.0.5), pch = 21, col = "black", bg = "white") +
  theme_bw()
