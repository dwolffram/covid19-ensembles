setwd("/home/wolffram/covid19-ensembles")

source("data_loading.R")
library(ggplot2)

source("uPIT_functions.R")


upit_histogram <- function(df, ..., facet, breaks, xlab='Probability Integral Transform', ylab='Density', 
                           scales="fixed", base_size=12){
  index_cols <- enquos(...)
  
  try(facet <- ensym(facet), silent=TRUE) # if NULL: no facetting
  
  if(missing(facet)){
    facet <- NULL
  }
  
  if(!missing(breaks)){
    df <- subset(df, quantile %in% round(breaks, 3))
  }
  
  models <- unique(df$model)
  
  results <- data.frame()
  for (m in models){
    temp <- subset(df, model==m) %>%
      group_by(!!!index_cols) %>%
      summarize(bin = get_bin(truth, value, quantile))
    
    temp <- t(sapply(temp$bin, FUN=get_bounds))
    temp <- data.frame(temp)
    temp$val <- 1/(nrow(temp)*(temp$X2 - temp$X1))
    
    results_m <- data.frame(alpha=c(unique(df$quantile), 1))
    results_m$upit <- sapply(results_m$alpha, FUN=get_upit, temp=temp)
    results_m <- bind_rows(results_m, data.frame(alpha=0, upit=0))
    results_m <- arrange(results_m, alpha)
    results_m$model <- m
    results <- bind_rows(results, results_m)
  }
  
  # results$model <- factor(results$model)
  # results$model <- recode_factor(results$model, "COVIDhub-baseline"="Baseline")
  # results$model <- fct_relevel(results$model, "Baseline", after = Inf)
  # 
  # results$model <- factor(results$model, levels=intersect(c('EWA', 'MED', 'INV', 'V2', 'V3', 'V4',
  #                                                           'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4',
  #                                                           'Baseline'),
  #                                                         unique(results$model)))
  # try(results$model <- factor(results$model, labels=c("EWA", "MED", "INV", "V[2]", "V[3]", "V[4]", "GQRA[2]", "GQRA[3]", "GQRA[4]",
  #                                                 "QRA[2]", "QRA[3]", "QRA[4]", "Baseline")),
  #     silent=TRUE)
  
  ggplot(results, aes(alpha, upit)) + 
    geom_rect(aes(xmin = alpha, xmax = lead(alpha), ymin = 0, ymax = lead(upit)), 
              color="black", fill = "black", alpha = 0.3)+
    facet_wrap(facet, ncol=3, scales=scales, labeller=label_parsed) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = function(x) ifelse(x == 0, "0", x)) +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    #scale_y_continuous(breaks = f(0.5), labels = function(y) ifelse(y == 0, "0", y)) +
    #ylim(0, 1.75) +
    labs(x="uPIT", y="Density") +
    geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color="black") +
    theme_gray(base_size=base_size)
}

df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_noUS.csv", add_baseline = TRUE, 
                     remove_revisions=TRUE, add_truth=TRUE)

df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk.csv", add_baseline = TRUE, 
                     remove_revisions=TRUE, add_truth=TRUE)

df <- load_ensembles("data/ensemble_forecasts/df_ensembles_4wk_noUS.csv", add_baseline = TRUE, 
                     remove_revisions=TRUE, add_truth=TRUE)

b <- subset(df, location!='US' & model=='EWA' & window_size==4)
upit_histogram(b, target_end_date, location)

c <- subset(df,  location!='US' & window_size==4)

upit_histogram(c, model, target_end_date, location, facet=model, scales="free_y")
upit_histogram(c, target_end_date, location, facet=model, breaks=seq(0, 1, 0.1))


upit_histogram(subset(df, location != "US" & window_size==4), model, target_end_date, location, 
               facet=model, scales="free_y", base_size=12)

upit_histogram(subset(df, location != "US" & window_size==4), model, target_end_date, location, 
               facet=model, scales="fixed",
               breaks=seq(0, 1, 0.1), base_size=12)

upit_histogram(subset(df, location != "US" & window_size==4), model, target_end_date, location, 
               facet=model, scales="free_y",
               breaks=seq(0, 1, 0.05), base_size=12)

ggsave('plots/1wk_ahead/upit_ensembles_1wk_23.png', width=15.5, height=18, dpi=500, unit='cm', device='png')
ggsave('plots/4wk_ahead/upit_ensembles_4wk_20.png', width=15.5, height=18, dpi=500, unit='cm', device='png')

upit_histogram(subset(c, location_name=="California"), target_end_date, location, facet=model, breaks=seq(0, 1, 0.1))

t <- load_truth()
t <- t %>%
  filter(location %in% unique(df$location))

t1 <- t %>%
  filter(date == "2020-10-10" & location != "US")


t1 <- t1 %>% 
  mutate(mortality = ntile(value, 3)) %>%
  select(c(location, mortality))

c <- left_join(c, t1, by = "location")

upit_histogram(subset(c, mortality==1), target_end_date, location, facet=model, breaks=seq(0, 1, 0.1))
upit_histogram(subset(c, mortality==2), target_end_date, location, facet=model, breaks=seq(0, 1, 0.1))
upit_histogram(subset(c, mortality==3), target_end_date, location, facet=model, breaks=seq(0, 1, 0.1))

upit_histogram(subset(c, mortality==1), target_end_date, location, facet=model, scales="free_y")
upit_histogram(subset(c, mortality==2), target_end_date, location, facet=model, scales="free_y")
upit_histogram(subset(c, mortality==3), target_end_date, location, facet=model, scales="free_y")


### INDIVIDUAL MODELS

df <- load_forecasts(models = c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", 
                                           "JHU_IDD-CovidSP", "LANL-GrowthRate", "MOBS-GLEAM_COVID", 
                                           "PSI-DRAFT", "UCLA-SuEIR", "UMass-MechBayes", "YYG-ParamSearch"),
                                targets = "1 wk ahead cum death",
                                exclude_locations = c("11", "60", "66", "69", "72", "74", "78"), 
                                start_date = "2020-06-20", 
                                end_date = "2020-10-10",
                                add_truth = TRUE) %>% 
  select(-c(forecast_date, type)) %>%
  select(target_end_date, everything())

# 4 wk ahead

df <- load_forecasts(models = c("CovidAnalytics-DELPHI", "COVIDhub-baseline", "CU-select", 
                                           "JHU_IDD-CovidSP", "LANL-GrowthRate", "MOBS-GLEAM_COVID", 
                                           "PSI-DRAFT", "UCLA-SuEIR", "UMass-MechBayes", "YYG-ParamSearch"),
                                targets = "4 wk ahead cum death",
                                exclude_locations = c("11", "60", "66", "69", "72", "74", "78"), 
                                start_date = "2020-08-01", 
                                end_date = "2020-10-31",
                                add_truth=TRUE) %>% 
  select(-c(forecast_date, type)) %>%
  select(target_end_date, everything())

df$model <- factor(df$model)
df$model <- recode_factor(df$model, "COVIDhub-baseline"="Baseline")
df$model <- fct_relevel(df$model, "Baseline", after = Inf)
levels(df$model)

#df <- add_truth(df)

upit_histogram(subset(df, location != "US"), model, target_end_date, location, facet=model, scales="free_y", base_size=12)
upit_histogram(subset(df, location != "US"), model, target_end_date, location, facet=model, scales="free_y",
               breaks=seq(0, 1, 0.1), base_size=12)

upit_histogram(subset(df, location != "US"), model, target_end_date, location, facet=model, scales="free_y",
               breaks=seq(0, 1, 0.05), base_size=12)

ggsave('plots/1wk_ahead/upit_individual_1wk_20.png', width=15.5, height=18, dpi=500, unit='cm', device='png')
ggsave('plots/4wk_ahead/upit_individual_4wk_20.png', width=15.5, height=18, dpi=500, unit='cm', device='png')


upit_histogram(subset(df, location != "US" & model == "UCLA-SuEIR"), model, target_end_date, location, 
               facet=model, scales="free_y", base_size=10)


c <- subset(df, location != "US" & model == "UCLA-SuEIR")
c <- left_join(c, t1, by = "location")

# upit_histogram(subset(c, location != "US" & model == "UCLA-SuEIR"), model, target_end_date, mortality, location, 
#                facet=mortality, scales="free_y", base_size=10)


mortality <- unique(c$mortality)

results <- data.frame()
for (m in mortality){
  print(m)
  temp <- subset(c, mortality==m) %>%
    group_by(model, target_end_date, location) %>%
    summarize(bin = get_bin(truth, value, quantile))
  
  temp <- t(sapply(temp$bin, FUN=get_bounds))
  temp <- data.frame(temp)
  temp$val <- 1/(nrow(temp)*(temp$X2 - temp$X1))
  
  results_m <- data.frame(alpha=c(unique(c$quantile), 1))
  results_m$upit <- sapply(results_m$alpha, FUN=get_upit, temp=temp)
  results_m <- bind_rows(results_m, data.frame(alpha=0, upit=0))
  results_m <- arrange(results_m, alpha)
  results_m$mortality <- m
  results <- bind_rows(results, results_m)
}

#     silent=TRUE)

ggplot(results, aes(alpha, upit)) + 
  geom_rect(aes(xmin = alpha, xmax = lead(alpha), ymin = 0, ymax = lead(upit)), 
            color="black", fill = "black", alpha = 0.3)+
  facet_wrap(mortality, ncol=3, scales="free", labeller=label_parsed) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = function(x) ifelse(x == 0, "0", x)) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  #scale_y_continuous(breaks = f(0.5), labels = function(y) ifelse(y == 0, "0", y)) +
  #ylim(0, 1.75) +
  labs(x="uPIT", y="Density") +
  geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color="black") +
  theme_gray(base_size=10)


upit_by_mortality <- function(df, model){
  c <- subset(df, location != "US" & model == model)
  c <- left_join(c, t1, by = "location")
  
  # upit_histogram(subset(c, location != "US" & model == "UCLA-SuEIR"), model, target_end_date, mortality, location, 
  #                facet=mortality, scales="free_y", base_size=10)
  
  
  mortality <- unique(c$mortality)
  
  results <- data.frame()
  for (m in mortality){
    print(m)
    temp <- subset(c, mortality==m) %>%
      group_by(model, target_end_date, location) %>%
      summarize(bin = get_bin(truth, value, quantile))
    
    temp <- t(sapply(temp$bin, FUN=get_bounds))
    temp <- data.frame(temp)
    temp$val <- 1/(nrow(temp)*(temp$X2 - temp$X1))
    
    results_m <- data.frame(alpha=c(unique(c$quantile), 1))
    results_m$upit <- sapply(results_m$alpha, FUN=get_upit, temp=temp)
    results_m <- bind_rows(results_m, data.frame(alpha=0, upit=0))
    results_m <- arrange(results_m, alpha)
    results_m$mortality <- m
    results <- bind_rows(results, results_m)
  }
  
  #     silent=TRUE)
  
  ggplot(results, aes(alpha, upit)) + 
    geom_rect(aes(xmin = alpha, xmax = lead(alpha), ymin = 0, ymax = lead(upit)), 
              color="black", fill = "black", alpha = 0.3)+
    facet_wrap(mortality, ncol=3, scales="free", labeller=label_parsed) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = function(x) ifelse(x == 0, "0", x)) +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    #scale_y_continuous(breaks = f(0.5), labels = function(y) ifelse(y == 0, "0", y)) +
    #ylim(0, 1.75) +
    labs(x="uPIT", y="Density") +
    geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color="black") +
    theme_gray(base_size=10)
}

upit_by_mortality(df, "UCLA-SuEIR")
upit_by_mortality(df, "YYG-ParamSearch")


c <- subset(df, location != "US" & model == "UCLA-SuEIR")
c <- subset(df, location != "US" & model == "YYG-ParamSearch")

c <- left_join(c, t1, by = "location")

# upit_histogram(subset(c, location != "US" & model == "UCLA-SuEIR"), model, target_end_date, mortality, location, 
#                facet=mortality, scales="free_y", base_size=10)


c <- subset(df, location != "US" & model == "YYG-ParamSearch")
upit_histogram(subset(c, location == "34" & model == "YYG-ParamSearch"), model, target_end_date, location,
               facet=model, scales="free_y", base_size=10)

mortality <- unique(c$location)

results <- data.frame()
for (m in mortality){
  print(m)
  temp <- subset(c, location==m) %>%
    group_by(model, target_end_date, location) %>%
    summarize(bin = get_bin(truth, value, quantile))
  
  temp <- t(sapply(temp$bin, FUN=get_bounds))
  temp <- data.frame(temp)
  temp$val <- 1/(nrow(temp)*(temp$X2 - temp$X1))
  
  results_m <- data.frame(alpha=c(unique(c$quantile), 1))
  results_m$upit <- sapply(results_m$alpha, FUN=get_upit, temp=temp)
  results_m <- bind_rows(results_m, data.frame(alpha=0, upit=0))
  results_m <- arrange(results_m, alpha)
  results_m$mortality <- m
  results <- bind_rows(results, results_m)
}

#     silent=TRUE)

ggplot(subset(results, mortality == "01"), aes(alpha, upit)) + 
  geom_rect(aes(xmin = alpha, xmax = lead(alpha), ymin = 0, ymax = lead(upit)), 
            color="black", fill = "black", alpha = 0.3)+
  facet_wrap(mortality, scales="free", labeller=label_parsed, ncol=3) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = function(x) ifelse(x == 0, "0", x)) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  #scale_y_continuous(breaks = f(0.5), labels = function(y) ifelse(y == 0, "0", y)) +
  #ylim(0, 1.75) +
  labs(x="uPIT", y="Density") +
  geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color="black") +
  theme_gray(base_size=10)

# temp <- subset(df, location != "US" & model == "UCLA-SuEIR") %>%
#   group_by(target_end_date, location) %>%
#   summarize(bin = get_bin(truth, value, quantile))
# 
# temp <- t(sapply(temp$bin, FUN=get_bounds))
# temp <- data.frame(temp)
# temp$val <- 1/(nrow(temp)*(temp$X2 - temp$X1))
# 
# u <- subset(df, location != "US" & model == "UCLA-SuEIR")
# 
# l <- u %>%
#   group_by(target_end_date, location) %>%
#   mutate(n = n_distinct(value))
# 
# View(subset(l, n < 23))
