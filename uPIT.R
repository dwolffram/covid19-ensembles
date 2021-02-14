setwd("/home/wolffram/covid19-ensembles")
setwd("D:/Dokumente/Workspace2/covid19-ensembles")

library(ggplot2)
library(tidyverse)
library(stringr)
library(xtable)


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
  #geom_step(aes(x=y, y=cdf), direction='hv') +
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

# problematic
p <- c(.05, .05, .1, .2, .3, .15, .05, .05, .025, .025)

df_dist <- make_dist(y, p)

plot_dist(df_dist)

s <- sampleDist(df_dist, 10000)

# Histogram of realized values
est_dist <- estimate_dist(s)
plot_dist(est_dist)


alphas <- round(seq(0.1, 0.9, 0.1), 3)
# alphas <- c(0.2, 0.4, 0.6, 0.8)

F <- make_quantile_forecast(df_dist, alphas)
F <- data.frame(quantile=alphas, value=F)

df <- forecasts_for_sample(df_dist, alphas, s)
upit_histogram(df, index)



# Negative bias / Underprediction
df <- forecasts_for_sample(make_dist(1:10, c(.3, .3, .1, .1, .1, .1, 0, 0, 0, 0)), alphas, s)
upit_histogram(df, index)

# Positive bias /Overprediction
df <- forecasts_for_sample(make_dist(1:10, c(0, 0, 0, 0, .1, .1, .1, .1, .3, .3)), alphas, s)
upit_histogram(df, index)

# Underdispersion
df <- forecasts_for_sample(make_dist(1:10, c(0, 0, 0, .25, .5, .25, 0, 0, 0, 0)), alphas, s)
upit_histogram(df, index)

# Overdispersion
df <- forecasts_for_sample(make_dist(1:10, rep(0.1, 10)), alphas, s)
upit_histogram(df, index)

df <- forecasts_for_sample(make_dist(-5:14, rep(0.05, 20)), alphas, s)
upit_histogram(df, index)


### NEGATIVE BINOMIAL
upit_negbin <- function(n, size, mu, alphas){
  sample <- rnbinom(n, size=size, mu=mu)
  F <- qnbinom(p=alphas, size=size, mu=mu)
  hist(sample, freq=FALSE)
  
  F <- data.frame(quantile=alphas, value=F)
  
  df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
  df$truth <- rep(sample, each=length(alphas))
  df$index <- rep(1:length(sample), each=length(alphas))
  
  upit_histogram(df, index, xlab='uPIT')+
    theme(
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10))#+ 
    # labs(title=bquote(mu==.(mu)~", Size ="~.(size)))
    # labs(title=bquote("Negative Binomial with"~mu==.(mu)~"and Size ="~.(size)))
}

upit_negbin(1000, 5, 200, alphas)

ggsave('plots/examples/uPIT/upit_nbin_50-5.png', width=14, height=8, dpi=500, unit='cm', device='png')

ggsave('plots/examples/uPIT/upit_nbin_100-5.png', width=6, height=6, dpi=500, unit='cm', device='png')
ggsave('plots/examples/uPIT/upit_nbin_200-5.png', width=6, height=6, dpi=500, unit='cm', device='png')


size=3
mu=200
sample <- rnbinom(5000, size=size, mu=mu)
F <- qnbinom(p=alphas, size=size, mu=mu)
hist(sample)

F <- data.frame(quantile=alphas, value=F)

df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
df$truth <- rep(sample, each=length(alphas))
df$index <- rep(1:length(sample), each=length(alphas))


upit_histogram(df, index)


size=5
mu=150
F <- qnbinom(p=alphas, size=size, mu=mu)
ps <- pnbinom(F, size=size, mu=mu)

quantiles_df <- t(data.frame(alpha=alphas, F=round(ps, 3)))

print(xtable(quantiles_df, caption="This is a caption"), include.rownames = TRUE, include.colnames = FALSE)


quantile(sample, alphas)


sample <- rnorm(1000, mean=5, sd=20)
F <- qnorm(alphas, mean=5, sd=20)
# 
# F <- sort(round(runif(9, -20, 20), 3))
# 
# F <- qunif(alphas, -20, 20)


F <- data.frame(quantile=alphas, value=F)

df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
df$truth <- rep(sample, each=length(alphas))
df$index <- rep(1:length(sample), each=length(alphas))


upit_histogram(df, index)

upit_histogram(df, index, breaks=round(seq(0, 0.9, .05), 2))

hist(sample)

### 23 quantiles
alphas <- c(0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
alphas <- sort(c(unique(c(alphas/2, 1-alphas/2)),0.5))
sample <- rnorm(5000, 0, 1)

# 0.8, 1.2
F <- qnorm(p=alphas, 0, 1)
F <- data.frame(quantile=alphas, value=F)

df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
df$truth <- rep(sample, each=length(alphas))
df$index <- rep(1:length(sample), each=length(alphas))

upit_histogram(df, index, xlab="uPIT")
ggsave('plots/examples/uPIT/upit_norm_23alphas.png', width=9, height=6, dpi=500, unit='cm', device='png')

ggsave('plots/examples/uPIT/upit_norm_23alphas_underdispersed.png', width=9, height=6, dpi=500, unit='cm', device='png')

upit_histogram(df, index, breaks=seq(0, 1, 0.1), xlab="uPIT")
ggsave('plots/examples/uPIT/upit_norm_10alphas_underdispersed.png', width=9, height=6, dpi=500, unit='cm', device='png')


### SIMULATION

upit_pois <- function(n, lambda, alphas, breaks){
  sample <- rpois(n, lambda)
  F <- qpois(p=alphas, lambda=lambda)
  F <- data.frame(quantile=alphas, value=F)
  
  df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
  df$truth <- rep(sample, each=length(alphas))
  df$index <- rep(1:length(sample), each=length(alphas))
  
  upit_histogram(df, index, breaks=breaks)
}

alphas <- round(seq(0.1, 0.9, 0.1), 3)
alphas <- round(seq(0.01, 0.99, 0.01), 3)

upit_pois(10000, 100, alphas)

upit_pois(1000, 80, alphas)
upit_pois(1000, 100, alphas)
upit_pois(1000, 200, alphas)
upit_pois(5000, 200, alphas)

l = 200
sample <- rpois(1000, lambda=l)
quantile(sample, alphas)

success = 0:(2*l)

plot(success, dpois(success, lambda=l),
     type='h',
     main='Poisson Distribution (lambda = 5)',
     ylab='Probability',
     xlab ='# Successes',
     lwd=3)

hist(sample)

F <- qpois(p=alphas, lambda=l)
F <- data.frame(quantile=alphas, value=F)

df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
df$truth <- rep(sample, each=length(alphas))
df$index <- rep(1:length(sample), each=length(alphas))


upit_histogram(df, index)


qpois(p=alphas, lambda=1500)

### COVID-FORECASTS
source("data_loading.R")
df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_noUS.csv", add_baseline = TRUE, 
                     remove_revisions=TRUE, add_truth=TRUE)

seq(0, 1, 0.05)
unique(df$quantile)[unique(df$quantile) %in% seq(0, 1, 0.05)]
unique(df$quantile)[unique(df$quantile) %in% round(seq(0, 1, 0.05), 3)]


upit_histogram(df, model, target_end_date, location)
upit_histogram(subset(df, model=="EWA"), target_end_date, location)


b <- subset(df, location!='US' & model=='EWA' & window_size==4)
upit_histogram(b, target_end_date, location)

upit_histogram(subset(b, quantile %in% seq(0, 1, 0.05)))
upit_histogram(b, target_end_date, location, breaks=seq(0, 1, 0.1))

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



c <- subset(df,  location!='US' & window_size==4)

temp <- a %>%
  group_by(model, target_end_date, location) %>%
  summarize(bin = get_bin(truth, value, quantile))

model_index <- temp$model

temp <- t(sapply(temp$bin, FUN=get_bounds))
temp <- data.frame(temp)
temp$model <- model_index

temp <- temp %>%
  group_by(model) %>%
  mutate(val=(1/n()) * (X2 - X1), n=n())


alphas <- c(unique(df$quantile), 1)
models <- unique(a$model)
results <- data.frame(model=rep(models, each=length(alphas)), alpha=rep(alphas, length(models)))

results %>%
  group_by(model, alpha) %>%
  summarize(upit=model)

results <- results %>%
  group_by(model, alpha) %>%
  summarize(upit=get_upit(subset(temp, model == first(model) ), alpha))

results <- results %>%
  group_by(model, alpha) %>%
  summarize(upit=get_upit(subset(temp, model == .$model[1] ), alpha))


a <- subset(c, quantile %in% round(seq(0, 1, 0.1), 3))
a <- c
results <- data.frame()
for (m in models){
  print(m)
  
  temp <- subset(a, model==m) %>%
    group_by(target_end_date, location) %>%
    summarize(bin = get_bin(truth, value, quantile))
  
  temp <- t(sapply(temp$bin, FUN=get_bounds))
  temp <- data.frame(temp)
  temp$val <- 1/(nrow(temp)*(temp$X2 - temp$X1))
  
  results_m <- data.frame(alpha=c(unique(a$quantile), 1))
  results_m$upit <- sapply(results_m$alpha, FUN=get_upit, temp=temp)
  results_m <- bind_rows(results_m, data.frame(alpha=0, upit=0))
  results_m <- arrange(results_m, alpha)
  results_m$model <- m
  results <- bind_rows(results, results_m)
}

results$model <- factor(results$model, levels=intersect(c('EWA', 'MED', 'INV', 'V2', 'V3', 'V4', 
                                                'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4',
                                                'Baseline'),
                                              unique(results$model)))

ggplot(subset(results, model=="EWA"), aes(alpha, upit)) + 
  geom_rect(aes(xmin = alpha, xmax = lead(alpha), ymin = 0, ymax = lead(upit)), 
            color="black", fill = "black", alpha = 0.3)+
  facet_wrap('model', ncol=3) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = function(x) ifelse(x == 0, "0", x)) +
  #labels=c("0", "0.25", "0.5", "0.75", "1")) +
  scale_y_continuous(breaks = f(0.5), labels = function(y) ifelse(y == 0, "0", y)) +
  #ylim(0, 1.75) +
  labs(x="uPIT", y="Density") +
  geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color="black") +
  theme_gray(base_size=12)

ggplot(results, aes(alpha, upit)) + 
  geom_rect(aes(xmin = alpha, xmax = lead(alpha), ymin = 0, ymax = lead(upit)), 
            color="black", fill = "black", alpha = 0.3)+
  facet_wrap('model', ncol=3, scales="free_y") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = function(x) ifelse(x == 0, "0", x)) +
  #labels=c("0", "0.25", "0.5", "0.75", "1")) +
  scale_y_continuous(breaks = f(0.5), labels = function(y) ifelse(y == 0, "0", y)) +
  #ylim(0, 1.75) +
  labs(x="uPIT", y="Density") +
  geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color="black") +
  theme_gray(base_size=12)


upit_histogram <- function(df, ..., facet, breaks, xlab='Probability Integral Transform', ylab='Density'){
  index_cols <- enquos(...)
  
  try(facet <- ensym(facet), silent=TRUE) # if NULL: no facetting
  
  if(!missing(breaks)){
    df <- subset(df, quantile %in% round(breaks, 3))
  }
  
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
  
  results$model <- factor(results$model, levels=intersect(c('EWA', 'MED', 'INV', 'V2', 'V3', 'V4', 
                                                            'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4',
                                                            'Baseline'),
                                                          unique(results$model)))
  results$model <- factor(results$model, labels=c("EWA", "MED", "INV", "V[2]", "V[3]", "V[4]", "GQRA[2]", "GQRA[3]", "GQRA[4]", 
                                            "QRA[2]", "QRA[3]", "QRA[4]", "Baseline"))
  
  ggplot(results, aes(alpha, upit)) + 
    geom_rect(aes(xmin = alpha, xmax = lead(alpha), ymin = 0, ymax = lead(upit)), 
              color="black", fill = "black", alpha = 0.3)+
    facet_wrap(facet, ncol=3, scales="free_y", labeller=label_parsed) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = function(x) ifelse(x == 0, "0", x)) +
    #labels=c("0", "0.25", "0.5", "0.75", "1")) +
    scale_y_continuous(breaks = f(0.5), labels = function(y) ifelse(y == 0, "0", y)) +
    #ylim(0, 1.75) +
    labs(x="uPIT", y="Density") +
    geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color="black") +
    theme_gray(base_size=12)
}

upit_histogram(c, target_end_date, location, facet=model)
upit_histogram(c, target_end_date, location, facet=model, breaks=seq(0, 1, 0.1))
