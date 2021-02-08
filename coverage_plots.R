coverage_plot <- function(df){
  df_temp <- df %>%
    select(starts_with('value') | truth)
  
  df_temp <- data.frame(1*sapply(subset(df_temp, select=-c(truth)), 
                                 function(x) x >= df$truth))
  
  #df_temp$value.1 <- 1*(df$value.0.99 < df$truth)
  
  df_temp <- data.frame(colMeans(df_temp))
  colnames(df_temp) <- 'value'
  
  quantiles <- c(get_quantile_levels())
  
  df_temp$quantile <- quantiles
  #widths <- c(0.01, diff(quantiles))
  ggplot(data=df_temp, aes(x=quantile, y=value)) +
    geom_line(stat="identity",
              color=viridis(3)[2]) +
    labs(x='Quantile',
         y='Observed Relative Frequency') +
    geom_segment(aes(x=0,xend=1,y=0,yend=1), linetype="dashed", color=viridis(3)[1])
  
  
}
df <- df_individual
coverage_plot(subset(df, location=='US'))

a <- subset(df, model=='CovidAnalytics-DELPHI')

df_temp <- a %>%
  select(quantile | starts_with('value') | truth)

df_temp$l <- df_temp$truth < floor(df_temp$value)
df_temp$u <- df_temp$truth <= floor(df_temp$value)

View(subset(df_temp, l!=u))

sum(df_temp$l == df_temp$u)

b <- df_temp %>%
  group_by(quantile) %>%
  summarize(l = mean(l), u=mean(u))

ggplot(b) +
  geom_errorbar(aes(x=quantile, ymin=l, ymax=u), 
               data=b, colour="black") +
  geom_segment(aes(x=0,xend=1,y=0,yend=1), linetype="dashed", colour="grey70")+
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = function(x) ifelse(x == 0, "0", x)) +
  scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
  xlab('Quantile') +
  ylab('')


plot_coverage <- function(df){
  df_temp <- df %>%
    select(quantile | value | truth)
  
  df_temp$l <- df_temp$truth < floor(df_temp$value)
  df_temp$u <- df_temp$truth <= floor(df_temp$value)
  
  df_temp <- df_temp %>%
    group_by(quantile) %>%
    summarize(l = mean(l), u=mean(u))
  
  ggplot(df_temp) +
    geom_errorbar(aes(x=quantile, ymin=l, ymax=u), width=0.05,
                  data=df_temp, colour="black") +
    geom_segment(aes(x=0,xend=1,y=0,yend=1), linetype="dashed", colour="grey70")+
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = function(x) ifelse(x == 0, "0", x)) +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    xlab('Quantile') +
    ylab('')
}




alphas <- round(seq(0.1, 0.9, 0.1), 3)

mu =10

sample <- rnbinom(1000, size=5, mu=mu)
F <- qnbinom(p=alphas, size=5, mu=mu)
# hist(sample, freq=FALSE)

F <- data.frame(quantile=alphas, value=F)
df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
df$truth <- rep(sample, each=length(alphas))








plot_coverage <- function(df){
  df_temp <- df
  df_temp$l <- df_temp$truth < floor(df_temp$value)
  df_temp$u <- df_temp$truth <= floor(df_temp$value)
  
  df_temp <- df_temp %>%
    group_by(mu, quantile) %>%
    summarize(l = mean(l), u=mean(u))
  
  ggplot(df_temp) +
    facet_wrap('mu') +
    geom_segment(aes(x=0,xend=1,y=0,yend=1), linetype="dashed", colour="grey70")+
    geom_errorbar(aes(x=quantile, ymin=l, ymax=u), width=0.05, size=0.5,
                  data=df_temp, colour="black") +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = function(x) ifelse(x == 0, "0", x)) +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    xlab('Quantile') +
    ylab('')
}

df_all <- data.frame()

for (mu in c(10, 20, 50, 75, 100, 200)){
  sample <- rnbinom(10000, size=5, mu=mu)
  F <- qnbinom(p=alphas, size=5, mu=mu)
  # hist(sample, freq=FALSE)
  
  F <- data.frame(quantile=alphas, value=F)
  df <- bind_rows(replicate(length(sample), F, simplify = FALSE))
  df$truth <- rep(sample, each=length(alphas))
  
  df$mu <- mu
  
  df_all <- bind_rows(df_all, df)
  
}


plot_coverage(df_all)

ggsave('plots/coverage_example.png', width=36, height=24, dpi=300, unit='cm', device='png')



df_temp$l <- df_temp$truth < floor(df_temp$value)
df_temp$u <- df_temp$truth <= floor(df_temp$value)

df_temp <- df_temp %>%
  group_by(mu, quantile) %>%
  summarize(mu=first(mu), l = mean(l), u=mean(u))
