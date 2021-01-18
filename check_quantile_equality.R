


a <- df %>%
  group_by(target_end_date, location, model) %>%
  summarize(not_increasing = any(diff(value) <= 0))

View(subset(a, not_increasing==TRUE))

mean(a$not_increasing)

View(subset(df, model=='PSI-DRAFT' & location==50 & target_end_date=='2020-10-03'))

View(subset(df, model=='CovidAnalytics-DELPHI' & location==15 & target_end_date=='2020-07-11'))

b <- subset(df, model=='CU-select' & location==15 & target_end_date=='2020-08-08')


c <- subset(a, not_increasing==TRUE)


b <- subset(df, model=='UCLA-SuEIR' & location==35 & target_end_date=='2020-10-10')

alphas <- c(0.02,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
quantile_levels <- seq(0.1, 1, by=.1)

df2 <- subset(df, quantile %in% quantile_levels)

a <- df2 %>%
  group_by(target_end_date, location, model) %>%
  summarize(not_increasing = any(diff(value) <= 0))

mean(a$not_increasing)
c <- subset(a, not_increasing==TRUE)

