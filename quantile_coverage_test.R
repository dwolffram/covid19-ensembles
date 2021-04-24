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
              p = mean(I), # = (n01 + n11)/(n00 + n01 + n10 + n11)
              
              n00 = sum(I00),
              n01 = sum(I01),
              n10 = sum(I10),
              n11 = sum(I11),
              
              # estimated transition probabilities for independence test
              p00 = n00/(n00 + n01),
              p01 = n01/(n00 + n01),
              p10 = n10/(n10 + n11),
              p11 = n11/(n10 + n11),
              
              # unconditional coverage
              L_uc_0 = (1 - unique(quantile))^n0 * unique(quantile)^n1,
              L_uc_1 = (1 - p)^n0 * p^n1,
              LR_uc = -2*log(L_uc_0/L_uc_1),
              p_uc = 1-pchisq(LR_uc, 1),
              
              # independence
              L_ind_0 = (1 - p)^(n00 + n10) * p^(n01 + n11),
              L_ind_1 = p00^n00 * p01^n01 * p10^n10 * p11^n11,
              LR_ind = -2*log(L_ind_0/L_ind_1),
              p_ind = 1-pchisq(LR_ind, 1),
              
              # conditional coverage
              LR_cc = -2*log(L_uc_0/L_ind_1),
              p_cc = 1-pchisq(LR_cc, 2)
    )
  
  if(!verbose){
    results <- results %>%
      select(c(quantile, p_uc, p_ind, p_cc))
  }
  
  return(results)
}


### EXAMPLE

alphas <- round(seq(0.1, 0.9, 0.1), 3)

# Negative binomial
size=5
mu=10
sample <- rnbinom(300, size=size, mu=mu)
F <- qnbinom(p=alphas, size=size, mu=mu)
hist(sample)

F <- data.frame(quantile=alphas, value=F)
df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
df$truth <- rep(sample, each=length(alphas))
df$index <- rep(1:length(sample), each=length(alphas))

res <- test_coverage(df, verbose=FALSE)

# Normal distribution
sample <- rnorm(300)
F <- qnorm(p=alphas)
hist(sample)

F <- data.frame(quantile=alphas, value=F)
df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
df$truth <- rep(sample, each=length(alphas))
df$index <- rep(1:length(sample), each=length(alphas))

res <- test_coverage(df, verbose=FALSE)


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
for (i in 1:10000){
  res <- test_coverage(sample_norm(500))
  res$index <- i
  results_df <- bind_rows(results_df, res)
}
# mean(results_df$p_cc < 0.05)

false_positives <- results_df %>%
  group_by(quantile) %>%
  summarize(std_norm = mean(p_uc < 0.05))




results_df = data.frame()
for (i in 1:10000){
  res <- test_coverage(sample_negbin(500, mu=100))
  res$index <- i
  results_df <- bind_rows(results_df, res)
}
# mean(results_df$p_cc < 0.05)

false_positives10 <- results_df %>%
  group_by(quantile) %>%
  summarize(false_positive = mean(p_uc < 0.05))

false_positives20 <- results_df %>%
  group_by(quantile) %>%
  summarize(false_positive = mean(p_uc < 0.05))

false_positives100 <- results_df %>%
  group_by(quantile) %>%
  summarize(false_positive = mean(p_uc < 0.05))


false_positives10_1000 <- false_positives10

fp <- left_join(left_join(false_positives10, false_positives20, by='quantile'), false_positives100, by='quantile')

fp <- fp %>%
  rename(mu10=false_positive.x, mu20=false_positive.y, mu100=false_positive)

write.csv(fp, "data/examples/false_positives.csv", row.names=FALSE)

fp2 <- left_join(false_positives, fp)
write.csv(fp2, "data/examples/false_positives_all.csv", row.names=FALSE)

print(xtable(fp2, digits=c(0, 2, 3, 3, 3, 3)), include.rownames=FALSE)



### Plot densities

size=5
mu=10
sample <- rnbinom(300, size=size, mu=mu)
hist(sample)
max(sample)
dnbinom(1:100, size=size, mu=mu)

x=1:150
df <- data.frame(x = x,
                 mu10 = dnbinom(x, size=5, mu=10),
                 mu20 = dnbinom(x, size=5, mu=20),
                 mu100 = dnbinom(x, size=5, mu=100))

df <- pivot_longer(df, cols=c(mu10, mu20, mu100), names_to='mu')

ggplot(df, aes(x=x, y=value, fill=mu)) + 
  geom_bar(stat='identity', position = 'dodge') +
  scale_fill_discrete(limits=c("mu10", "mu20", "mu100"),  labels = c("10", "20", "100"), name="mu") +
  labs(y='Probability') +
  theme_gray(base_size=20)

ggsave('plots/examples/neg_bin_ex.png', width=36, height=14, dpi=500, unit='cm', device='png')
