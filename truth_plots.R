setwd("/home/wolffram/covid19-ensembles")
source("data_loading.R")
library(ggplot2)

Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_ALL", "C")


df <- load_truth()
df <- add_location_names(df)

df <- subset(df, weekdays(df$date)=='Saturday')

ggplot(subset(df, location_name %in% c("Alaska", "California", "Georgia", 
                                   "Iowa", "Mississippi", "Missouri",
                                   "New Hampshire", "New York", "Vermont")),
       aes(x=date, y=value)) +
  geom_line() +
  theme_gray(base_size=10) +
  facet_wrap("location_name", scales="free_y") +
  xlab("Date") +
  ylab("Cumulative Deaths")

ggsave('plots/truth_by_state_example.png', width=14, height=9, dpi=500, unit='cm', device='png')


df <- df %>%
  filter(location == "US" & date >= "2020-02-22")

ggplot(df, aes(x=date, y=c(0, diff(value)))) +
  annotate("rect", xmin=as.Date("2020-05-23"), xmax=as.Date("2020-10-10"), ymin=0, ymax=Inf,
           fill="green", alpha=0.3) +
  annotate("rect", xmin=as.Date("2020-06-20"), xmax=as.Date("2020-10-10"), ymin=0, ymax=Inf,
           fill="blue", alpha=0.3) +geom_line() +
  geom_point(pch = 21, col = "black", bg = "white") +

  labs(title=NULL,
       x = "Date",
       y = "Cumulative Deaths") +
  theme_grey(base_size=10)#+


df1 <- df
df1$target <- "1 wk ahead cum death"
df1$train_start <- as.Date("2020-05-23")
df1$test_start <- as.Date("2020-06-20")
df1$test_end <- as.Date("2020-10-10")

df2 <- df
df2$target <- "4 wk ahead cum death"
df2$train_start <- as.Date("2020-06-13")
df2$test_start <- as.Date("2020-08-01")
df2$test_end <- as.Date("2020-10-31")

df3 <- bind_rows(df1, df2)

ggplot(df1, aes(x=date, y=c(0, diff(value)))) +
  annotate("rect", xmin=df1$train_start[1], xmax=df1$[1], ymin=0, ymax=Inf,
           fill="green", alpha=0.3) +
  annotate("rect", xmin=df1$test_start[1], xmax=df1$test_end[1], ymin=0, ymax=Inf,
           fill="blue", alpha=0.3) +geom_line() +
  geom_point(pch = 21, col = "black", bg = "white") +
  
  labs(title=NULL,
       x = "Date",
       y = "Cumulative Deaths") +
  theme_grey(base_size=10)#+


anno <- data.frame(target = c("1 wk ahead cum death", "4 wk ahead cum death"),
                   train_start = as.Date(c("2020-05-23", "2020-06-13")), 
                   test_start = as.Date(c("2020-06-20", "2020-08-01")), 
                   test_end = as.Date(c("2020-10-10", "2020-10-31")),
                   y1 = -Inf, y2 = Inf)


ggplot(df3, aes(x=date, y=value)) +
  geom_rect(data = anno, aes(xmin = train_start, xmax = test_start, ymin = y1, ymax = y2), 
            alpha = 0.3, fill="green", inherit.aes = FALSE)+
  geom_rect(data = anno, aes(xmin = test_start, xmax = test_end, ymin = y1, ymax = y2), 
            alpha = 0.3, fill="blue", inherit.aes = FALSE)+
  geom_line() + 
  geom_point(pch = 21, col = "black", bg = "white") +
  facet_wrap("target") +
  labs(title=NULL,
       x = "Date",
       y = "Cumulative Deaths") +
  theme_grey(base_size=10)#+



df4 <- df3

df4$value <- c(0,diff(df4$value))
df4 <- df4[-1,]

df1_inc <- df1
df1_inc$value <- c(0,diff(df1_inc$value))
df1_inc <- df1_inc[-1,]
df1_inc$target <- "1 wk ahead inc death"

df2_inc <- df2
df2_inc$value <- c(0,diff(df2_inc$value))
df2_inc <- df2_inc[-1,]
df2_inc$target <- "4 wk ahead inc death"

df3_inc <- bind_rows(df1_inc, df2_inc)

anno <- data.frame(target = c("1 wk ahead cum death", "4 wk ahead cum death", "1 wk ahead inc death", "4 wk ahead inc death"),
                   train_start = as.Date(c("2020-05-23", "2020-06-13", "2020-05-23", "2020-06-13")), 
                   test_start = as.Date(c("2020-06-20", "2020-08-01", "2020-06-20", "2020-08-01")), 
                   test_end = as.Date(c("2020-10-10", "2020-10-31", "2020-10-10", "2020-10-31")),
                   y1 = -Inf, y2 = Inf)

ggplot(df3_inc, aes(x=date, y=value)) +
  geom_rect(data = anno, aes(xmin = train_start, xmax = test_start, ymin = y1, ymax = y2), 
            alpha = 0.3, fill="green", inherit.aes = FALSE)+
  geom_rect(data = anno, aes(xmin = test_start, xmax = test_end, ymin = y1, ymax = y2), 
            alpha = 0.3, fill="blue", inherit.aes = FALSE)+
  geom_line() + 
  geom_point(pch = 21, col = "black", bg = "white") +
  facet_wrap("target") +
  labs(title=NULL,
       x = "Date",
       y = "Cumulative Deaths") +
  theme_grey(base_size=10)#+


df_all <- bind_rows(df3, df3_inc)

ggplot(df_all, aes(x=date, y=value)) +
  geom_rect(data = anno, aes(xmin = train_start, xmax = test_start, ymin = y1, ymax = y2), 
            alpha = 0.3, fill="yellow", inherit.aes = FALSE)+
  geom_rect(data = anno, aes(xmin = test_start, xmax = test_end, ymin = y1, ymax = y2), 
            alpha = 0.3, fill="green", inherit.aes = FALSE)+
  geom_line() + 
  geom_point(pch = 21, size=0.8, col = "black", bg = "white") +
  facet_wrap("target", scales="free_y") +
  labs(title=NULL,
       x = "Date",
       y = "Deaths") +
  theme_grey(base_size=10)#+


ggsave('plots/evaluation_periods.png', width=15.5, height=9, dpi=500, unit='cm', device='png')

