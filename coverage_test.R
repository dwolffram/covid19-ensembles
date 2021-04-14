# Unconditional Coverage
test_coverage_uc <- function(df, lower, upper){
  df <- df %>%
    mutate(I = truth >= get(paste0('quantile_', lower)) & truth <= get(paste0('quantile_', upper)))
  
  
  n1 <- sum(df$I)
  n0 <- nrow(df) - n1
  
  p <- upper - lower
  p1 <- mean(df$I)

  L0 <- (1 - p)^n0 * p^n1
  L1 <- (1 - p1)^n0 * p1^n1
  
  LR_uc <- -2*log(L0/L1)
  pval <- 1-pchisq(LR_uc, 1)
  
  # print(paste0('Coverage probability: ', p))
  # print(paste0('Observed coverage: ', p1))
  # print(paste0('p-value: ', pval))
  
  return(pval)
}


# Independence
test_independence <- function(df, lower, upper){
  df <- df %>%
    mutate(I = truth >= get(paste0('quantile_', lower)) & truth <= get(paste0('quantile_', upper)))
  
  df <- df %>%
    mutate(I_lag = lag(I)) %>%
    mutate(I11 = ifelse(is.na(I_lag), NA, I_lag & I), 
           I00 = ifelse(is.na(I_lag), NA, !I_lag & !I), 
           I01 = ifelse(is.na(I_lag), NA, !I_lag & I),
           I10 = ifelse(is.na(I_lag), NA, I_lag & !I))
  
  n00 <- sum(df$I00, na.rm=TRUE)
  n01 <- sum(df$I01, na.rm=TRUE)
  n10 <- sum(df$I10, na.rm=TRUE)
  n11 <- sum(df$I11, na.rm=TRUE)
  
  p <- (n01 + n11)/(n00 + n01 + n10 + n11)
  
  p00 <- n00/(n00 + n01)
  p01 <- n01/(n00 + n01)
  p10 <- n10/(n10 + n11)
  p11 <- n11/(n10 + n11)
  
  L0 <- (1 - p)^(n00 + n10) * p^(n01 + n11)
  L1 <- p00^n00 * p01^n01 * p10^n10 * p11^n11
  
  LR_ind <- -2*log(L0/L1)
  pval <- 1-pchisq(LR_ind, 1)
  
  return(pval)
}


# Conditional Coverage
test_coverage <- function(df, lower, upper){
  df <- df %>%
    mutate(I = truth >= get(paste0('quantile_', lower)) & truth <= get(paste0('quantile_', upper)))
  
  # null of the unconditional coverage test
  n1 <- sum(df$I)
  n0 <- nrow(df) - n1
  
  p <- upper - lower

  L0 <- (1 - p)^n0 * p^n1
  
  # alternative of the independence test
  df <- df %>%
    mutate(I_lag = lag(I)) %>%
    mutate(I11 = ifelse(is.na(I_lag), NA, I_lag & I), 
           I00 = ifelse(is.na(I_lag), NA, !I_lag & !I), 
           I01 = ifelse(is.na(I_lag), NA, !I_lag & I),
           I10 = ifelse(is.na(I_lag), NA, I_lag & !I))
  
  n00 <- sum(df$I00, na.rm=TRUE)
  n01 <- sum(df$I01, na.rm=TRUE)
  n10 <- sum(df$I10, na.rm=TRUE)
  n11 <- sum(df$I11, na.rm=TRUE)
  
  p00 <- n00/(n00 + n01)
  p01 <- n01/(n00 + n01)
  p10 <- n10/(n10 + n11)
  p11 <- n11/(n10 + n11)
  
  L1 <- p00^n00 * p01^n01 * p10^n10 * p11^n11

  LR_cc <- -2*log(L0/L1)
  pval <- 1-pchisq(LR_cc, 1)
  
  return(pval)
}

# Example
size=5
mu=10
sample <- rnbinom(200, size=size, mu=mu)
F <- qnbinom(p=alphas, size=size, mu=mu)
hist(sample)

sample <- rnorm(200)
F <- qnorm(p=alphas)
hist(sample)

F <- data.frame(quantile=alphas, value=F)
F <- pivot_wider(F, names_from=quantile, names_prefix="quantile_", values_from=value)

df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
df$truth <- sample


test_coverage_uc(df, 0.2, 0.8)

test_independence(df, 0.2, 0.8)

test_coverage(df, 0.2, 0.8)
