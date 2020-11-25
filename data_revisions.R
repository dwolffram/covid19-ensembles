
df <- load_truth()
df <- subset(df, weekdays(df$date)=='Saturday')
df$type = 'all'

# New Jersey: Rev. Date 2020-06-29
df2 <- load_truth('2020-08-10')
df2 <- load_truth('2020-06-29')
df2 <- load_truth('2020-07-06')
df2 <- load_truth('2020-07-27')
df2 <- load_truth('2020-08-03')

df2$type = 'unrevised'

df3 <- rbind(df, df2)

ggplot(subset(df3, location=='34'), aes(x=date, y=value, color=type)) +
  geom_line()
                 

# New York: Rev. Date 2020-07-06
df2 <- load_truth('2020-06-29')
df2 <- load_truth('2020-07-06')
df2 <- load_truth('2020-07-27')
df2 <- load_truth('2020-08-03')

df2$type = 'unrevised'

df3 <- rbind(df, df2)
df3 <- add_location_names(df3)

ggplot(subset(df3, location_name=='New York'), aes(x=date, y=value, color=type)) +
  geom_line()

compare_truth <- function(df, location_name, as_of){
  df2 <- load_truth(as_of)
  df2$type = paste('as of', as_of)
  
  df3 <- rbind(df, df2)
  df3 <- add_location_names(df3)
  
  df3 %>%
    filter(location_name== !!(location_name)) %>%
    ggplot(aes(x=date, y=value, color=type)) +
      geom_line() #+
      #geom_vline(xintercept=as.numeric(as.Date(as_of)), linetype=2)
}

compare_truth(df, 'New York', '2020-06-29')
compare_truth(df, 'New York', '2020-07-06')
compare_truth(df, 'New York', '2020-07-27')
compare_truth(df, 'New York', '2020-08-03')
compare_truth(df, 'New York', '2020-08-10')
compare_truth(df, 'New York', '2020-08-17')
compare_truth(df, 'New York', '2020-08-24')
compare_truth(df, 'New York', '2020-08-31')
compare_truth(df, 'New York', '2020-09-07')
# 2020-07-04


compare_truth(df, 'New Jersey', '2020-06-29')
compare_truth(df, 'New Jersey', '2020-07-06')
compare_truth(df, 'New Jersey', '2020-07-27')
compare_truth(df, 'New Jersey', '2020-08-03')
# 2020-06-27

compare_truth(df, 'Mississippi', '2020-06-08')
compare_truth(df, 'Mississippi', '2020-06-15')
compare_truth(df, 'Mississippi', '2020-06-22')
compare_truth(df, 'Mississippi', '2020-06-29')
compare_truth(df, 'Mississippi', '2020-07-06')

compare_truth(df, 'Delaware', '2020-06-08')
compare_truth(df, 'Delaware', '2020-06-15')
compare_truth(df, 'Delaware', '2020-06-22')
compare_truth(df, 'Delaware', '2020-06-29')
compare_truth(df, 'Delaware', '2020-07-06')

 
compare_truth(df, 'Massachusetts', '2020-08-24')
compare_truth(df, 'Massachusetts', '2020-08-31')
compare_truth(df, 'Massachusetts', '2020-09-07')
compare_truth(df, 'Massachusetts', '2020-09-14')
compare_truth(df, 'Massachusetts', '2020-09-21')
compare_truth(df, 'Massachusetts', '2020-09-28')

compare_truth(df, 'North Carolina', '2020-08-24')
compare_truth(df, 'North Carolina', '2020-08-31')
compare_truth(df, 'North Carolina', '2020-09-07')
compare_truth(df, 'North Carolina', '2020-09-14')
compare_truth(df, 'North Carolina', '2020-09-21')
compare_truth(df, 'North Carolina', '2020-09-28')

compare_truth(df, 'Michigan', '2020-06-08')
compare_truth(df, 'Michigan', '2020-06-15')
# 2020-06-20

df2 <- read_csv("data/jhu_historic_deaths/preprocessed/2020-06-13_time_series_covid19_deaths_US.csv")
df2 <- subset(df2, weekdays(df2$date)=='Saturday')
df2$type = 'unrevised'
df2 <- df2 %>%
  select(-location_name)

df3 <- rbind(df, df2)
df3 <- add_location_names(df3)

ggplot(subset(df3, location_name=='Michigan'), aes(x=date, y=value, color=type)) +
  geom_line()

compare_truth(df, 'Texas', '2020-06-29')
compare_truth(df, 'Texas', '2020-07-06')
compare_truth(df, 'Texas', '2020-07-27')
compare_truth(df, 'Texas', '2020-08-03')
compare_truth(df, 'Texas', '2020-08-10')
compare_truth(df, 'Texas', '2020-08-17')
compare_truth(df, 'Texas', '2020-08-24')
compare_truth(df, 'Texas', '2020-08-31')
compare_truth(df, 'Texas', '2020-09-07')
compare_truth(df, 'Texas', '2020-09-14')
compare_truth(df, 'Texas', '2020-09-21')
compare_truth(df, 'Texas', '2020-09-28')
compare_truth(df, 'Texas', '2020-10-05')
# 2020-08-08

df2 <- read_csv("data/jhu_historic_deaths/preprocessed/2020-08-08_time_series_covid19_deaths_US.csv")
df2 <- subset(df2, weekdays(df2$date)=='Saturday')
df2$type = 'unrevised'
df2 <- df2 %>%
  select(-location_name)

df3 <- rbind(df, df2)
df3 <- add_location_names(df3)

ggplot(subset(df3, location_name=='Texas'), aes(x=date, y=value, color=type)) +
  geom_line()


unique(subset(df3, location_name == 'Texas')$location)

revisions <- data.frame(location = c(26, 34, 36, 48),
                        location_name = c('Michigan', 'New Jersey', 'New York', 'Texas'),
                        first_valid_test_date = c('2020-06-20', '2020-06-27', 
                                                  '2020-07-04', '2020-08-08'))

write.csv(revisions, "data/revisions.csv", row.names=FALSE)



# df <- df %>%
#   group_by(location) %>%
#   mutate(value = c(NA, diff(value)))
# 
# df2 <- df2 %>%
#   group_by(location) %>%
#   mutate(value = c(NA, diff(value))/7)