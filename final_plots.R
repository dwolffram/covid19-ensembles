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
  facet_wrap(~method) +
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


## wis decomposition by state (fix and scales="free_y")
ggplot(subset(results_long, window_size==4 & location!='US'), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~location) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS Decomposition by State and Ensemble Method",
       x = "Model",
       y = "Relative Contribution to WIS")

ggplot(subset(results_long, window_size==4 & location!='US' & target_end_date>'2020-08-01'), 
       aes(x=method, y=value, fill=factor(penalty, levels=c("wgt_pen_l", "wgt_iw", "wgt_pen_u")))) +
  facet_wrap(~location) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_fill_viridis(discrete=TRUE, name = "Penalty for", 
                     labels = c("Overprediction", "Dispersion", "Underprediction")) +
  labs(title= "WIS Decomposition by State and Ensemble Method",
       x = "Model",
       y = "Relative Contribution to WIS")



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