setwd("/home/wolffram/covid19-ensembles")

library(ggplot2)
library(tidyverse)
library(stringr)

source("uPIT_functions.R")




sampleDist = function(df, n) { 
  sample(x = df$y, n, replace = T, prob = df$p) 
}

make_dist <- function(y, p){
  df <- data.frame(y=y, p=p)
  df$cdf <- cumsum(df$p)
  return(df)
}

plot_dist <- function(df){
  ggplot(data=df, aes(x=y, y=p)) +
  geom_bar(stat="identity", color="black", fill="black", alpha=0.3) +
  scale_x_discrete(limits=factor(1:10))+
  geom_step(aes(x=y, y=cdf), direction='hv') +
  labs(title="Example Distribution") +
  ylab("Probability") +
  xlab("Y")
}

estimate_dist <- function(sample){
  s_df <- as.data.frame(table(factor(sample, levels=unique(sample))))
  colnames(s_df) <- c("y", "p")
  s_df$p <- s_df$p/sum(s_df$p)
  make_dist(s_df$y, s_df$p)
} 


get_quantile <- function(df, alpha){
  min(subset(df, df$cdf >= alpha)$y)
}

make_quantile_forecast <- function(df_dist, alphas){
  F <- sapply(alphas, FUN=get_quantile, df=df_dist)
}

forecasts_for_sample <- function(df_dist, alphas, sample){
  F <- make_quantile_forecast(df_dist, alphas)
  F <- data.frame(quantile=alphas, value=F)
  df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
  df$truth <- rep(sample, each=length(alphas))
  df$index <- rep(1:length(sample), each=length(alphas))
  return(df)
}


y <- 1:10

p <- c(.1, .1, .1, .1, .3, .1, .05, .05, .05, .05)
p <- c(0.1, 0.1, 0.1, 0.1, 0.3, 0, 0, 0.1, 0.1, 0.1)
p <- c(.05, .05, .1, .2, .3, .15, .05, .05, .025, .025)

df_dist <- make_dist(y, p)

plot_dist(df_dist)

s <- sampleDist(df_dist, 10000)

# Histogram of realized values
est_dist <- estimate_dist(s)
plot_dist(est_dist)


alphas <- round(seq(0.1, 0.9, 0.1), 3)
alphas <- c(0.2, 0.4, 0.6, 0.8)

F <- make_quantile_forecast(df_dist, alphas)
F <- data.frame(quantile=alphas, value=F)


df <- forecasts_for_sample(df_dist, alphas, s)
upit_histogram(df, index)

df <- data.frame(quantile=alphas, value=c(1, 2, 4, 5))


# Negative bias / Underprediction
df <- forecasts_for_sample(make_dist(1:10, c(.3, .3, .1, .1, .1, .1, 0, 0, 0, 0)), alphas, s)
upit_histogram(df)

# Positive bias /Overprediction
df <- forecasts_for_sample(make_dist(1:10, c(0, 0, 0, 0, .1, .1, .1, .1, .3, .3)), alphas, s)
upit_histogram(df)

# Underdispersion
df <- forecasts_for_sample(make_dist(1:10, c(0, 0, 0, .25, .5, .25, 0, 0, 0, 0)), alphas, s)
upit_histogram(df)

# Overdispersion
df <- forecasts_for_sample(make_dist(1:10, rep(0.1, 10)), alphas, s)
upit_histogram(df)

df <- forecasts_for_sample(make_dist(-5:14, rep(0.05, 20)), alphas, s)
upit_histogram(df)



### COVID-FORECASTS
source("data_loading.R")
df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk.csv", add_baseline = TRUE, 
                     remove_revisions=TRUE, add_truth=TRUE)

seq(0, 1, 0.05)
unique(df$quantile)[unique(df$quantile) %in% seq(0, 1, 0.05)]
unique(df$quantile)[unique(df$quantile) %in% round(seq(0, 1, 0.05), 3)]




b <- subset(df, location!='US' & model=='EWA')
upit_histogram(b, target_end_date, location)

upit_histogram(subset(b, quantile %in% seq(0, 1, 0.05)))
upit_histogram(b, seq(0, 1, 0.05))

c <- subset(b, quantile %in% seq(0, 1, 0.05))


upit_histogram(subset(df, location!='US' & model=='QRA2' & window_size==4))
upit_histogram(subset(df, location!='US' & model=='MED'))
upit_histogram(subset(df, location!='US' & model=='V2'))
upit_histogram(subset(df, location!='US' & model == 'Baseline' & window_size==4))
upit_histogram(subset(df, location!='US' & model == 'GQRA2' & window_size==4))
upit_histogram(subset(df, location!='US' & model == 'INV' & window_size==4))
upit_histogram(subset(df, location!='US' & model == 'V4' & window_size==4))
upit_histogram(subset(df, location!='US' & model == 'QRA3' & window_size==4))

upit_histogram(subset(df, location!='US' & model == 'Baseline' & window_size==4))
upit_histogram(subset(df, location!='US' & model == 'Baseline' & window_size==4), seq(0, 1, 0.05))
upit_histogram(subset(df, location!='US' & model == 'Baseline' & window_size==4), seq(0, 1, 0.1))


