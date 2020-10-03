### LOAD FILEs

locations <- read.csv('data/locations.csv')
locations <- locations %>%
  select(-c(abbreviation))

p <- round(unique(results$population)/1000)*1000

results$population <- cut(p, breaks=quantile(p, probs=seq(0,1, length  = 11), na.rm=TRUE),include.lowest=TRUE)

results$population <- ntile(results$population, 10)

results <- read.csv('results/results_2020-10-02_fixed.csv',
                   colClasses = c(window_size = "factor", target_end_date = "Date"))

results <- left_join(results, locations, by="location")

individual_results <- read.csv('results/individual_results.csv',
                   colClasses = c(target_end_date = "Date"))

individual_results <- individual_results %>%
  select(-c(type, forecast_date)) %>%
  mutate(window_size=NA) %>%
  rename(method=model) %>%
  as.data.frame()

individual_results <- left_join(individual_results, locations, by='location')


results$method <- factor(results$method, levels=c('EWA', 'MED', 'V2', 'V3', 'V4', 
                                                  'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4'))

individual_results$method <- factor(individual_results$method, 
                                    levels=c('COVIDhub-baseline', 'CovidAnalytics-DELPHI', 
                                             'LANL-GrowthRate', 'MOBS-GLEAM_COVID', 
                                              'UCLA-SuEIR','YYG-ParamSearch'))

unique(results$method)
unique(individual_results$method)

baseline <- subset(individual_results, method=='COVIDhub-baseline')
baseline$method <- 'Baseline'
baseline <- bind_rows(replicate(4, baseline, simplify = FALSE))
baseline$window_size <- rep(1:4, each = nrow(baseline)/4)
results_with_baseline <- rbind(results, baseline)
results_with_baseline$method <- factor(results_with_baseline$method, 
                                       levels=c('Baseline', 'EWA', 'MED', 'V2', 'V3', 'V4', 
                                                  'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4'))
unique(results_with_baseline$method)

results <- results_with_baseline

l <- unique(results$location_name)
l <- c(as.character(l[l!='US']), 'US')
results$location_name <- factor(results$location_name, levels=l)

results_long <- pivot_longer(results, cols=c("wgt_pen_u", "wgt_iw", "wgt_pen_l"),
                             names_to="penalty")

### BOXPLOTS

ggplot(data = subset(results, location == 'US'), 
       aes(x = method, y = wis, fill=method)) +
  facet_wrap(~window_size) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "WIS by window size (national level)",
       x = "Model",
       y = "WIS")


ggplot(data = subset(results, location == 'US'), 
       aes(x = window_size, y = wis, fill=window_size)) +
  facet_wrap(~method, ncol=4, dir='v') +
  geom_boxplot() +
  #geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "WIS by ensemble method (national level)",
       x = "Window Size",
       y = "WIS")

# after revision
ggplot(data = subset(results, location == 'US' & target_end_date > '2020-08-01'), 
       aes(x = method, y = wis, fill=method)) +
  facet_wrap(~window_size) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "WIS by window size (national level) - after data revision from 2020-08-01",
       x = "Model",
       y = "WIS")

# after revision
ggplot(data = subset(results, location == 'US'& target_end_date > '2020-08-01'), 
       aes(x = window_size, y = wis, fill=window_size)) +
  facet_wrap(~method) +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "WIS by ensemble method (national level) - after data revision from 2020-08-01",
       x = "Window Size",
       y = "WIS")

# window_size 4
ggplot(data = subset(results, location == 'US' & window_size == 4), 
       aes(x = method, y = wis, fill=method)) +
  #geom_boxplot(outlier.shape=NA) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "WIS on national level (window size 4)",
       x = "Model",
       y = "WIS")

ggplot(data = subset(results, location == 'US' & window_size == 4 & target_end_date>'2020-08-01'), 
       aes(x = method, y = wis, fill=method)) +
  geom_boxplot(outlier.shape=NA) +
  stat_summary(fun.y=mean, geom="point", shape=3) +
  scale_fill_viridis(discrete = TRUE, alpha=0.5) +
  theme(legend.position = "none") +
  labs(title= "Window size 4 (national level) - after data revision from 2020-08-01",
       x = "Model",
       y = "WIS")



### WIS DECOMPOSITION


# national level
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

ggplot(subset(results_long, location=='US' & window_size==4), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=0,hjust=0)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition on national level (window size 4)",
       x = "Window Size",
       y = "WIS")

ggplot(subset(results_long, location!='US' & window_size==4), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=0,hjust=0)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition on state level (window size 4)",
       x = "Window Size",
       y = "WIS")


# national level after 2020-08-01
ggplot(subset(results_long, location=='US'& target_end_date > '2020-08-01'), 
       aes(x=window_size, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~method) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=0,hjust=0)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition (national level) - after data revision from 2020-08-01",
       x = "Window Size",
       y = "WIS")


# window size 4, state level
ggplot(subset(results_long, location!="US"), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  #facet_wrap(~target_end_date, scales='free_y') +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition on state level (window size 4)",
       x = "Model",
       y = "WIS")

# overall performance, national level
ggplot(subset(results_long, location=="US"), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  #facet_wrap(~target_end_date, scales='free_y') +
  facet_wrap(~window_size) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition on national level",
       x = "Model",
       y = "WIS")

ggplot(subset(results_long, location=="US" & target_end_date>'2020-08-01'), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~window_size) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition on national level - after data revision from 2020-08-01",
       x = "Model",
       y = "WIS")

## POPULATION
ggplot(subset(results_long, window_size==4 & location!='US'), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~population) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS Decomposition by State and Ensemble Method",
       x = "Model",
       y = "WIS")


## wis decomposition by state (fix and scales="free_y")
ggplot(subset(results_long, window_size==4 & location!='US'), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~location_name) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS Decomposition by State and Ensemble Method",
       x = "Model",
       y = "WIS")

ggplot(subset(results_long, window_size==4 & location!='US' & target_end_date>'2020-08-08'), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~location_name) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS Decomposition by State and Ensemble Method",
       x = "Model",
       y = "WIS")



## wis decomposition over time
ggplot(subset(results_long, location!="US" & window_size == 4), 
       aes(x=target_end_date, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~method, scales="free_y") +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition on state level (window size 4)",
       x = "Model",
       y = "WIS")

ggplot(subset(results_long, location=="US" & window_size==4), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~target_end_date, scales='free_y') +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS decomposition on national level (window size 4)",
       x = "Model",
       y = "WIS")# +
#ylim(0, 10000)