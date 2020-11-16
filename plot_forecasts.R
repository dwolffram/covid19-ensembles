source("data_loading.R")
source("scoring.R")
source("ensemble_methods.R")
source("ensemble_functions.R")


# cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(length(levels_coverage) + 1)[-1]
cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1]


df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_2020-11-13.csv", add_baseline = TRUE)

df_wide <- pivot_wider(df, names_from=quantile, names_prefix="value.", values_from=value)
df_wide <- add_truth(df_wide)
df_wide <- add_location_names(df_wide)

# results <- add_truth(df_ensembles)

data <- subset(df_wide, location=="US" & model =="MED" & window_size==4)
data <- subset(df_wide, location=="34" & window_size==4)
data <- subset(df_wide, model=="V3" & window_size==4)
data <- subset(df_wide, location=="US" & window_size==4)


plot_forecast <- function(data, facet, incidence=FALSE, title=NULL, ncol=4, dir='v', scales='fixed'){
  facet <- ensym(facet)
  
  if (incidence){
    data <- data %>% 
      group_by(!!facet) %>%
      mutate(value.0.5 = value.0.5 - lag(truth)) %>%
      mutate(value.0.05 = value.0.05 - lag(truth)) %>%
      mutate(value.0.95 = value.0.95 - lag(truth)) %>%
      mutate(value.0.25 = value.0.25 - lag(truth)) %>%
      mutate(value.0.75 = value.0.75 - lag(truth)) %>%
      mutate(truth=c(NA, diff(truth))) %>%
      drop_na()
  }
  
  
  if(missing(title)){
    horizon = substr(unique(data$target), 1, 1)
    title = paste(horizon, "wk ahead forecasts with 50% and 90% prediction intervals")
    }
  
  ggplot(data, aes(x=target_end_date, y=truth)) +
    {if(!missing(facet)) facet_wrap(facet, ncol=ncol, dir=dir, scales=scales)} +
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
     theme_grey(base_size=8)+
     theme(plot.title= element_text(size=9),
           axis.text = element_text(size = 5)) 
}


plot_forecast(subset(df_wide, window_size==4 & location=='US'), facet=model, incidence=TRUE, ncol=4)
plot_forecast(subset(df_wide, window_size==4 & location=='US' & model=='QRA3'), incidence=FALSE, ncol=4)


plot_forecast(subset(df_wide, window_size==4 & model=='V3'), facet=location_name, scales='free_y', 
              incidence=TRUE, ncol=8, dir='h')




data <- subset(add_truth(df_ensembles), location=="US")
data <- subset(individual_results, location=="US")
data <- subset(results, location=="34" & window_size==4)

data <- subset(individual_results, method=="COVIDhub-baseline")
data <- subset(individual_results, method=="YYG-ParamSearch")

plot_forecast(data, incidence=FALSE, title='Forecasts for New Jersey')

plot_forecast(subset(df_wide, window_size==4 & location=='US'), incidence=TRUE, 
              title="1 week ahead individual forecasts with 50%- and 90%-prediction intervals (national level)")
  #theme(text = element_text(size = 25)) 

plot_forecast(data, incidence=TRUE, 
              title="YYG-ParamSearch forecasts with 50%- and 90%-prediction intervals")
ggsave('plots/individual_models/individual_incidence_us.png', width=11, height=7, dpi=500, unit='cm', device='png')


