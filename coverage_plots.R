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



### COVID-19 Forecasts

df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_noUS.csv", add_baseline = TRUE, 
                     remove_revisions=TRUE, add_truth=TRUE)

plot_coverage <- function(df, width=0.05, breaks){
  if(!missing(breaks)){
    df <- subset(df, quantile %in% round(breaks, 3))
  }
  
  df$model <- factor(df$model, levels = c('EWA', 'MED', 'INV', 'V2', 'V3', 'V4',
                                          'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4',
                                          "CovidAnalytics-DELPHI", "CU-select", "JHU_IDD-CovidSP", 
                                          "LANL-GrowthRate", "MOBS-GLEAM_COVID", "PSI-DRAFT", "UCLA-SuEIR", 
                                          "UMass-MechBayes", "YYG-ParamSearch", "Baseline"),
                     labels = c("EWA", "MED", "INV", "V[2]", "V[3]", "V[4]", 
                                "GQRA[2]", "GQRA[3]", "GQRA[4]", "QRA[2]", "QRA[3]", "QRA[4]", 
                                "DELPHI", "CU", "JHU_IDD", 
                                "LANL", "MOBS", "PSI", "UCLA", 
                                "UMass", "YYG", 'Baseline'))
  df_temp <- df
  df_temp$l <- df_temp$truth < floor(df_temp$value)
  df_temp$u <- df_temp$truth <= floor(df_temp$value)
  
  df_temp <- df_temp %>%
    group_by(model, quantile) %>%
    summarize(l = mean(l), u=mean(u))
  
  ggplot(df_temp) +
    facet_wrap('model', ncol=3, labeller = label_parsed) +
    geom_segment(aes(x=0,xend=1,y=0,yend=1), linetype="dashed", colour="grey70")+
    geom_errorbar(aes(x=quantile, ymin=l, ymax=u), width=width, size=0.5,
                  data=df_temp, colour="black") +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = function(x) ifelse(x == 0, "0", x)) +
    scale_y_continuous(labels = function(y) ifelse(y == 0, "0", y)) +
    xlab('Quantile') +
    ylab('')
}

plot_coverage(subset(df, location!='US' & window_size==4), breaks=seq(0, 1, 0.1))

plot_coverage(subset(df, location!='US' & window_size==4), width=0.02)

ggsave('plots/coverage_ensembles_23.png', width=20, height=24, dpi=500, unit='cm', device='png')





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

plot_coverage(subset(df, location!='US'), breaks=seq(0, 1, 0.1))

plot_coverage(subset(df, location!='US'), width=0.02)


ggsave('plots/coverage_individual_23.png', width=20, height=24, dpi=500, unit='cm', device='png')




