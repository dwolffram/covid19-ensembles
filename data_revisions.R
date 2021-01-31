setwd("/home/wolffram/covid19-ensembles")
source("data_loading.R")

Sys.setlocale("LC_TIME", "C")

df <- load_truth()
df <- subset(df, weekdays(df$date)=='Saturday')
df$type <- 'revised'

# New Jersey: Rev. Date 2020-06-29
df2 <- load_truth('2020-06-22')
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
      geom_line() +
      geom_point() +
      theme_gray(base_size=10) +
      labs(title = location_name, 
           x = 'Date',
           y = 'Cumulative Deaths',
           color='Truth')+
      geom_vline(xintercept=as.numeric(as.Date(as_of)), linetype=2)
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

compare_truth(df, 'New Jersey', '2020-06-22')
compare_truth(df, 'New Jersey', '2020-06-29')
compare_truth(df, 'New Jersey', '2020-07-06')
compare_truth(df, 'New Jersey', '2020-07-27')
# 2020-06-28

df2 <- read_csv("data/jhu_historic_deaths/preprocessed/2020-06-28_time_series_covid19_deaths_US.csv")
df2 <- subset(df2, weekdays(df2$date)=='Saturday')
df2$type = 'unrevised'
df2 <- df2 %>%
  select(-location_name)

df3 <- rbind(df, df2)
df3 <- add_location_names(df3)

ggplot(subset(df3, location_name=='New Jersey'), aes(x=date, y=value, color=type)) +
  geom_line()



compare_truth2 <- function(df, location_name, as_of){
  df2 <- read_csv(paste0("data/jhu_historic_deaths/preprocessed/", as_of, "_time_series_covid19_deaths_US.csv"))
  df2 <- subset(df2, weekdays(df2$date)=='Saturday')
  df2$type = paste('as of', as_of)
  df2 <- df2 %>%
    select(-location_name)
  
  
  df3 <- rbind(df, df2)
  df3 <- add_location_names(df3)
  
  #start_date <- as.Date(as_of) - 60
  start_date <- as.Date("2020-03-01")
  end_date <- as.Date(as_of) + 14
  
  df3 %>%
    filter(location_name== !!(location_name) & date >= start_date & date <= end_date) %>%
    ggplot(aes(x=date, y=value, color=type)) +
    geom_line() +
    geom_point(size=0.4) +
    theme_gray(base_size=8) +
    theme(legend.background = element_rect(colour=NA, fill=NA),
          legend.title=element_blank(),
          legend.justification = c(0, 1),
          legend.position=c(0.05, 1)) +
          # legend.justification = c(1, 0),
          # legend.position=c(1, 0.05)) +
    labs(title = location_name, 
         x = 'Date',
         y = 'Cumulative Deaths',
         color='Truth')#+
    #geom_vline(xintercept=as.numeric(as.Date(as_of)), linetype=2)
}


compare_truth2(df, 'New Jersey', '2020-06-27')
ggsave('plots/revision/new_jersey_unrevised.png', width=7.5, height=5, dpi=500, unit='cm', device='png')
compare_truth2(df, 'New Jersey', '2020-07-04')
ggsave('plots/revision/new_jersey_revised.png', width=7.5, height=5, dpi=500, unit='cm', device='png')


compare_truth2(df, 'New York', '2020-06-27')
ggsave('plots/revision/new_york_unrevised.png', width=7.5, height=5, dpi=500, unit='cm', device='png')
compare_truth2(df, 'New York', '2020-07-04')
ggsave('plots/revision/new_york_revised.png', width=7.5, height=5, dpi=500, unit='cm', device='png')


compare_truth2(df, 'Texas', '2020-08-01')
ggsave('plots/revision/texas_unrevised.png', width=7.5, height=5, dpi=500, unit='cm', device='png')
compare_truth2(df, 'Texas', '2020-08-08')
ggsave('plots/revision/texas_revised.png', width=7.5, height=5, dpi=500, unit='cm', device='png')


compare_truth2(df, 'Michigan', '2020-06-13')
ggsave('plots/revision/michigan_unrevised.png', width=7.5, height=5, dpi=500, unit='cm', device='png')
compare_truth2(df, 'Michigan', '2020-06-20')
ggsave('plots/revision/michigan_revised.png', width=7.5, height=5, dpi=500, unit='cm', device='png')


compare_truth2(df, 'California', '2020-10-25')



### INCIDENCE

df1_inc <- df
df1_inc$value <- c(0,diff(df1_inc$value))
df1_inc <- df1_inc[-1,]

df2_inc <- df2
df2_inc$value <- c(0,diff(df2_inc$value))
df2_inc <- df2_inc[-1,]

compare_truth3 <- function(df, location_name, as_of){
  df1_inc <- add_location_names(df)%>%
    filter(location_name== !!(location_name))
  df1_inc$value <- c(0,diff(df1_inc$value))
  df1_inc <- df1_inc[-1,]%>%
    select(-location_name)
  
  df2 <- read_csv(paste0("data/jhu_historic_deaths/preprocessed/", as_of, "_time_series_covid19_deaths_US.csv"))
  df2 <- subset(df2, weekdays(df2$date)=='Saturday')%>%
    filter(location_name== !!(location_name))
  df2$type = paste('as of', as_of)
  df2 <- df2 %>%
    select(-location_name)
  
  df2_inc <- df2
  df2_inc$value <- c(0,diff(df2_inc$value))
  df2_inc <- df2_inc[-1,]
  
  
  df3 <- rbind(df1_inc, df2_inc)
  df3 <- add_location_names(df3)
  
  df3  %>%
    ggplot(aes(x=date, y=value, color=type)) +
    geom_line() +
    geom_point(size=0.4) +
    theme_gray(base_size=8) +
    theme(legend.background = element_rect(colour=NA, fill=NA),
          legend.title=element_blank(),
          # legend.justification = c(0, 1),
          # legend.position=c(0.05, 1)) +
          legend.justification = c(1, 0),
          legend.position=c(1, 0.05)) +
    labs(title = location_name, 
         x = 'Date',
         y = 'Cumulative Deaths',
         color='Truth')#+
  #geom_vline(xintercept=as.numeric(as.Date(as_of)), linetype=2)
}


compare_truth3(df, 'Michigan', '2020-06-13')
compare_truth3(df, 'Michigan', '2020-06-20')


compare_truth3(df, 'Texas', '2020-08-01')
compare_truth3(df, 'Texas', '2020-08-08')

df1_inc <- add_location_names(df)%>%
  filter(location_name== "Michigan")
df1_inc$value <- c(0,diff(df1_inc$value))
df1_inc <- df1_inc[-1,]

df2 <- read_csv(paste0("data/jhu_historic_deaths/preprocessed/", '2020-06-13', "_time_series_covid19_deaths_US.csv"))
df2 <- subset(df2, weekdays(df2$date)=='Saturday')%>%
  filter(location_name== "Michigan")
df2$type = paste('as of', '2020-06-13')
df2 <- df2 %>%
  select(-location_name)

df2_inc <- df2
df2_inc$value <- c(0,diff(df2_inc$value))
df2_inc <- df2_inc[-1,]


df3 <- rbind(df1_inc, df2)
df3 <- add_location_names(df3)

df3  %>%
  ggplot(aes(x=date, y=value, color=type)) +
  geom_line() +
  geom_point(size=0.4) +
  theme_gray(base_size=8) +
  theme(legend.background = element_rect(colour=NA, fill=NA),
        legend.title=element_blank(),
        # legend.justification = c(0, 1),
        # legend.position=c(0.05, 1)) +
        legend.justification = c(1, 0),
        legend.position=c(1, 0.05)) +
  labs(title = "location_name", 
       x = 'Date',
       y = 'Cumulative Deaths',
       color='Truth')#+


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
                        first_valid_test_date = c('2020-06-20', '2020-06-28', 
                                                  '2020-07-04', '2020-08-08'))

write.csv(revisions, "data/revisions.csv", row.names=FALSE)


### REMOVE REVISIONS

df <- load_scores("scores/ensemble_scores_1wk_noUS.csv", long_format=TRUE)
revisions <- read.csv("data/revisions.csv", colClasses = c(first_valid_test_date ="Date"))

apply(revisions, 1, function(x) df %>% # the 1 is for row wise 
        filter(location == x[1] & target_end_date < x[3]))

b <- df %>%
  rowwise() %>%
  filter(all(!(location == revisions[, 1] & target_end_date < revisions[, 3]))) %>%
  data.frame()

remove_revisions <- function(df, window_size=4){
  revisions <- read.csv("data/revisions.csv", colClasses = c(first_valid_test_date ="Date"))
  
  # infer horizon from target column
  horizon <- as.numeric(substr(unique(df$target), 1, 1))
  
  # to exclude forecasts trained on unrevised data
  time_delta <- window_size*7 + (horizon - 1)*7
  
  df <- df %>%
    rowwise() %>%
    filter(all(!(location == revisions[, 1] & target_end_date < revisions[, 3] + time_delta))) %>%
    data.frame()
  
  return(df)
}

c <- remove_revisions(df, 4)

#View(subset(c, location %in% c(26, 34, 36, 48)))

c %>%
  filter(location %in% c(26, 34, 36, 48)) %>%
  group_by(location) %>%
  summarize(min_date=min(target_end_date))

window_size = 4
horizon = 4
as.Date('2020-06-20') + window_size*7 + (horizon - 1)*7


# df <- df %>%
#   group_by(location) %>%
#   mutate(value = c(NA, diff(value)))
# 
# df2 <- df2 %>%
#   group_by(location) %>%
#   mutate(value = c(NA, diff(value))/7)