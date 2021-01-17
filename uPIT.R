setwd("/home/wolffram/covid19-ensembles")

library(ggplot2)
library(tidyverse)
library(stringr)


alphas = seq(0.1, 0.9, 0.1)
alphas = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
quantiles = c(1, 2, 3, 4, 5, 5, 5, 8, 9)

sampleDist = function(n) { 
  sample(x = 1:10, n, replace = T, prob = c(0.1, 0.1, 0.1, 0.1, 0.3, 0, 0, 0.1, 0.1, 0.1)) 
}

get_bin <- function(truth, value, quantile){
  lower <- c(0, quantile[!duplicated(value, fromLast=TRUE)])
  upper <- c(quantile[!duplicated(value, fromLast=TRUE)], 1)
  labels <- paste0("(", lower, ", ", upper, "]")
  cut(unique(truth), breaks=c(-Inf, unique(value), Inf), labels=labels, right=TRUE)
}

get_bounds <- function(s){
  as.numeric(str_split(str_sub(s, 2, -2), ",")[[1]])
}

get_upit <-function(temp, alpha){
  return(sum(subset(temp, X1 < alpha & alpha <= X2)$val))
}

upit_histogram <- function(df){
  bins <- df %>%
    group_by(index) %>%
    summarize(bin = get_bin(truth, value, quantile))
  
  e <- t(sapply(bins$bin, FUN=get_bounds))
  e <- data.frame(e)
  e$val <- 1/(length(unique(df$index))*(e$X2 - e$X1))
  
  results <- data.frame(alpha=c(alphas, 1))
  results$upit <- sapply(results$alpha, FUN=get_upit, temp=e)
  results <- bind_rows(results, data.frame(alpha=0, upit=0))
  results <- arrange(results, alpha)
  
  ggplot(results, aes(alpha, upit)) + 
    geom_rect(aes(xmin = alpha, xmax = lead(alpha), ymin = 0, ymax = lead(upit)), 
              color="black", fill = "black", alpha = 0.3) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels=c("0", "0.25", "0.5", "0.75", "1"))+
    labs(x='Probability Integral Transform',
         y='Density') +
    geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color="black") +
    theme_gray(base_size=16)
  
}

# Example Distribution
y = 1:10
p = c(0.1, 0.1, 0.1, 0.1, 0.3, 0, 0, 0.1, 0.1, 0.1)

df_distribution <- data.frame(y=y, p=p)


ggplot(data=df_distribution, aes(x=y, y=p)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=factor(1:10))+
  labs(title="Example Distribution") +
  ylab("Probability") +
  xlab("Y")


s <- sampleDist(10000)

s_df <- as.data.frame(table(factor(s, levels=1:10)))

ggplot(s_df, aes(x=Var1, y=Freq/sum(Freq))) +
  geom_bar(stat="identity", color='black', fill='black', alpha=0.3)+
  labs(title="Relative Frequency") +
  ylab("Relative Frequency") +
  xlab("Y")


df <- data.frame(quantile=alphas, value=quantiles)
df <- bind_rows(replicate(length(s), df, simplify = FALSE))
df$truth <- rep(s, each=length(alphas))
df$index <- rep(1:length(s), each=length(alphas))

upit_histogram(df)


alphas <- c(0.2, 0.4, 0.6, 0.8)
df <- data.frame(quantile=alphas, value=c(1, 2, 4, 5))
df <- bind_rows(replicate(length(s), df, simplify = FALSE))
df$truth <- rep(s, each=length(alphas))
df$index <- rep(1:length(s), each=length(alphas))

upit_histogram(df)


### COVID-FORECASTS
source("data_loading.R")
df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk.csv", add_baseline = TRUE, remove_revisions=TRUE, add_truth=TRUE)
df <- add_truth(df)

seq(0, 1, 0.05)
unique(df$quantile)[unique(df$quantile) %in% seq(0, 1, 0.05)]
unique(df$quantile)[unique(df$quantile) %in% round(seq(0, 1, 0.05), 3)]


upit_histogram <- function(df, breaks){
  if(!missing(breaks)){
    df <- subset(df, quantile %in% round(breaks, 3))
  }
  
  temp <- df %>%
    group_by(target_end_date, location) %>%
    summarize(bin = get_bin(truth, value, quantile))
  
  temp <- t(sapply(temp$bin, FUN=get_bounds))
  temp <- data.frame(temp)
  temp$val <- 1/(nrow(temp)*(temp$X2 - temp$X1))
  
  results <- data.frame(alpha=c(unique(df$quantile), 1))
  results$upit <- sapply(results$alpha, FUN=get_upit, temp=temp)
  results <- bind_rows(results, data.frame(alpha=0, upit=0))
  results <- arrange(results, alpha)
  
  ggplot(results, aes(alpha, upit)) + 
    geom_rect(aes(xmin = alpha, xmax = lead(alpha), ymin = 0, ymax = lead(upit)), 
              color="black", fill = "black", alpha = 0.3) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels=c("0", "0.25", "0.5", "0.75", "1")) +
    labs(x='Probability Integral Transform', y='Density') +
    geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color="black") +
    theme_gray(base_size=16)
}

b <- subset(df, location!='US' & model=='EWA')
#b <- add_truth(b)
upit_histogram(b)
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


