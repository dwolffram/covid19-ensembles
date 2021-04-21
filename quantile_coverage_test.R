test_coverage <- function(df, verbose=FALSE){
  
  results <- df %>%
    mutate(I = truth <= value) %>%
    group_by(quantile) %>%
    mutate(I_lag = lag(I)) %>%
    mutate(I11 = ifelse(is.na(I_lag), NA, I_lag & I), 
           I00 = ifelse(is.na(I_lag), NA, !I_lag & !I), 
           I01 = ifelse(is.na(I_lag), NA, !I_lag & I),
           I10 = ifelse(is.na(I_lag), NA, I_lag & !I)) %>%
    drop_na() %>%
    summarize(n1 = sum(I), 
              n0 = sum(1 - I), 
              p1 = mean(I),
              
              n00 = sum(I00),
              n01 = sum(I01),
              n10 = sum(I10),
              n11 = sum(I11),
              
              p = (n01 + n11)/(n00 + n01 + n10 + n11),
              
              p00 = n00/(n00 + n01),
              p01 = n01/(n00 + n01),
              p10 = n10/(n10 + n11),
              p11 = n11/(n10 + n11),
              
              L_uc_0 = (1 - unique(quantile))^n0 * unique(quantile)^n1,
              L_uc_1 = (1 - p1)^n0 * p1^n1,
              
              LR_uc = -2*log(L_uc_0/L_uc_1),
              p_uc = 1-pchisq(LR_uc, 1),
              
              L_ind_0 = (1 - p)^(n00 + n10) * p^(n01 + n11),
              L_ind_1 = p00^n00 * p01^n01 * p10^n10 * p11^n11,
              LR_ind = -2*log(L_ind_0/L_ind_1),
              p_ind = 1-pchisq(LR_ind, 1),
              
              LR_cc = -2*log(L_uc_0/L_ind_1),
              p_cc = 1-pchisq(LR_cc, 2)
    )
  
  if(!verbose){
    results <- results %>%
      select(c(quantile, p_uc, p_ind, p_cc))
  }
  
  return(results)
}


# Example

alphas <- round(seq(0.1, 0.9, 0.1), 3)

size=5
mu=10
sample <- rnbinom(200, size=size, mu=mu)
F <- qnbinom(p=alphas, size=size, mu=mu)
hist(sample)

sample <- rnorm(500)
F <- qnorm(p=alphas)
hist(sample)

F <- data.frame(quantile=alphas, value=F)

df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
df$truth <- rep(sample, each=length(alphas))
df$index <- rep(1:length(sample), each=length(alphas))

res <- test_coverage(df)


sample_norm <- function(n=100, alphas=round(seq(0.1, 0.9, 0.1), 3)){
  sample <- rnorm(n)
  F <- qnorm(p=alphas)
  F <- data.frame(quantile=alphas, value=F)
  df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
  df$truth <- rep(sample, each=length(alphas))
  df$index <- rep(1:length(sample), each=length(alphas))
  return(df)
}

sample_negbin <- function(n=100, size=5, mu=10, alphas=round(seq(0.1, 0.9, 0.1), 3)){
  sample <- rnbinom(n, size=size, mu=mu)
  F <- qnbinom(p=alphas, size=size, mu=mu)
  F <- data.frame(quantile=alphas, value=F)
  df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
  df$truth <- rep(sample, each=length(alphas))
  df$index <- rep(1:length(sample), each=length(alphas))
  return(df)
}


res <- test_coverage(sample_norm())

results_df = data.frame()
for (i in 1:1000){
  res <- test_coverage(sample_norm(200))
  res$index <- i
  results_df <- bind_rows(results_df, res)
}
mean(results_df$p_cc < 0.05)

results_df %>%
  group_by(quantile) %>%
  summarize(false_positive = mean(p_cc < 0.05))




results_df = data.frame()
for (i in 1:1000){
  res <- test_coverage(sample_negbin(200))
  res$index <- i
  results_df <- bind_rows(results_df, res)
}
mean(results_df$p_cc < 0.05)

results_df %>%
  group_by(quantile) %>%
  summarize(false_positive = mean(p_cc < 0.05))




