setwd("/home/dwolffram/covid19-ensembles")
# 
# library(tidyverse)
# library(dplyr)
# library(readr)

source("functions.R")

models <- c("LANL-GrowthRate", "MIT_CovidAnalytics-DELPHI", "MOBS_NEU-GLEAM_COVID", 
            "YYG-ParamSearch", "UCLA-SuEIR")

exclude_locations <- c("11", "66", "69", "72", "78")


df <- load_df(models=models, exclude_locations=exclude_locations)
results <- wis_table(df)


ggplot(data = subset(results, wis<150), aes(x = model, y = wis)) +
  geom_boxplot(outlier.shape=NA)

# ggplot(data = results, aes(x = target_end_date, y = wis, colour = model)) +
#   geom_line()

results_mean <- results %>%
  select(model, target_end_date, wis) %>%
  group_by(model, target_end_date) %>% 
  summarize(mean_wis = mean(wis))

results_mean <- results %>%
  select(method, target_end_date, wis, window_size) %>%
  group_by(method, target_end_date, window_size) %>% 
  summarize(mean_wis = mean(wis))

ggplot(data = results_mean, aes(x = target_end_date, y = mean_wis, colour = method)) +
  geom_line() +
  facet_wrap(~window_size) +
  ylim(0, 110)
ggsave('plots/mean_wis_ws.png', width=24, height=14, dpi=500, unit='cm', device='png')


results_mean_targets <- results %>%
  select(method, target_end_date, target, wis) %>%
  group_by(method, target_end_date, target) %>% 
  summarize(mean_wis = mean(wis))

ggplot(data = results_mean_targets, aes(x = target_end_date, y = mean_wis, colour = method)) +
  geom_line() +
  facet_wrap(~target) +
  ylim(0, 600)

model_count <- df %>%
  group_by(target_end_date, target) %>%
  summarize(model_count = length(unique(model)))


# library(ggfan)
# intervals = 1:19/20
# b <- subset(df, model=="YYG-ParamSearch")
# ggplot(b,  aes(x=target_end_date,y=value,quantile=quantile)) + geom_fan(intervals=c(0.2, 0.5, 0.8))

# ggplot(data = b, aes(x = target_end_date, y = value, colour = quantile)) +
#   geom_line()






