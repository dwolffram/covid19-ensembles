# setwd("/home/wolffram/covid19-ensembles")
setwd("D:/Dokumente/Workspace2/covid19-ensembles")

library(ggplot2)
library(tidyverse)
library(stringr)

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

f <- function(k) {
  step <- k
  function(y) seq(floor(min(y)), ceiling(max(y)), by = step)       
}

# f <- function(k) {
#   step <- k
#   function(y) seq(0, 1.75, by = step)       
# }


upit_histogram <- function(df, ..., breaks, xlab='Probability Integral Transform', ylab='Density'){
  index_cols <- enquos(...)
  
  if(!missing(breaks)){
    df <- subset(df, quantile %in% round(breaks, 3))
  }
  
  temp <- df %>%
    group_by(!!!index_cols) %>%
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
                       labels = function(x) ifelse(x == 0, "0", x)) +
                       #labels=c("0", "0.25", "0.5", "0.75", "1")) +
    scale_y_continuous(breaks = f(0.5), labels = function(y) ifelse(y == 0, "0", y)) +
    #ylim(0, 1.75) +
    labs(x=xlab, y=ylab) +
    geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color="black") +
    theme_gray(base_size=12)
}
