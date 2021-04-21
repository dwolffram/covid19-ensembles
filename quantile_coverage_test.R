# Unconditional Coverage
test_coverage_uc <- function(df){
  df <- df %>%
    mutate(I = truth <= get(paste0('quantile_', upper)))
  lower <- 0
  
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
test_independence <- function(df, upper, lower){
  # if lower is missing use one-sided interval, i.e. test a quantile 
  if(missing(lower)){
    df <- df %>%
      mutate(I = truth <= get(paste0('quantile_', upper)))
    lower <- 0
  } else {
    df <- df %>%
      mutate(I = truth >= get(paste0('quantile_', lower)) & truth <= get(paste0('quantile_', upper)))
  }
  
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
test_coverage <- function(df, upper, lower){
  # if lower is missing use one-sided interval, i.e. test a quantile 
  if(missing(lower)){
    df <- df %>%
      mutate(I = truth <= get(paste0('quantile_', upper)))
    lower <- 0
  } else {
    df <- df %>%
      mutate(I = truth >= get(paste0('quantile_', lower)) & truth <= get(paste0('quantile_', upper)))
  }
  
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
alphas <- round(seq(0.1, 0.9, 0.1), 3)


size=5
mu=10
sample <- rnbinom(500, size=size, mu=mu)
F <- qnbinom(p=alphas, size=size, mu=mu)
hist(sample)

sample <- rnorm(200)
F <- qnorm(p=alphas)
hist(sample)

F <- data.frame(quantile=alphas, value=F)
F <- pivot_wider(F, names_from=quantile, names_prefix="quantile_", values_from=value)

df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
df$truth <- sample


temp <- df %>%
  mutate("I_alpha" = truth <= get(paste0('quantile_', 0.8)))


test_coverage_uc(df, 0.8, 0.2)

test_independence(df, 0.8, 0.2)

test_coverage(df, 0.8, 0.2)

test_coverage(df, 0.6)


size=5
mu=10
sample <- rnbinom(500, size=size, mu=mu)
F <- qnbinom(p=alphas, size=size, mu=mu)
hist(sample, freq=FALSE)

F <- data.frame(quantile=alphas, value=F)

df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
df$truth <- rep(sample, each=length(alphas))
df$index <- rep(1:length(sample), each=length(alphas))

df$I <- df$truth <= floor(df$value)

# df <- df %>%
#   group_by(quantile) %>%
#   summarize(n1 = sum(I), n0 = sum(1 - I), p1 = mean(I))


results <- df %>%
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
            
            n00 = sum(I00, na.rm=TRUE),
            n01 = sum(I01, na.rm=TRUE),
            n10 = sum(I10, na.rm=TRUE),
            n11 = sum(I11, na.rm=TRUE),
            
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
            p_cc = 1-pchisq(LR_cc, 1)
            )
    
         
         

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

test_uc <- function(p0, p1, n0, n1){
  L0 <- (1 - p0)^n0 * p0^n1
  L1 <- (1 - p1)^n0 * p1^n1
  
  LR_uc <- -2*log(L0/L1)
  pval <- 1-pchisq(LR_uc, 1)
  
  return(list(LR_uc=LR_uc, p_uc=pval))
}

test_ind <- function(){
  
}

test_uc(0.1, 0.144, 428, 72)

# a <- apply(df, 1, function(x) test_uc(x['quantile'], x['p1'], x['n0'], x['n1']))
# a$pval
# 
# df %>%
#   mutate(LR_uc = test_uc(quantile, p1, n0, n1)) %>% unnest()

b <- df %>%
  group_by_all() %>%
  bind_cols(test_uc(.$quantile, .$p1, .$n0, .$n1))
