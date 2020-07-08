setwd("D:/Dokumente/Workspace2/covid19-forecast-hub")

path_hub <- "../covid19-forecast-hub/"

library(tidyverse)
library(dplyr)
library(readr)

load_df <- function(models=c("LANL-GrowthRate", "MIT_CovidAnalytics-DELPHI", 
                             "MOBS_NEU-GLEAM_COVID", "YYG-ParamSearch", "UCLA-SuEIR"),
                    exclude_locations=c("11", "66", "69", "72", "78")){
  
  truth <- read.csv(paste0(path_hub, "data-truth/truth-Cumulative Deaths.csv"),
                    colClasses = c(location = "character", date = "Date"))
  print("Loading forecasts...")
  df <- data.frame()
  for (m in models){
    print(m)
    df_temp <- list.files(path=paste0("data-processed/", m), pattern=".csv$", full.names = TRUE) %>% 
      lapply(read_csv, col_types = cols(
        forecast_date = col_date(format = ""),
        target = col_character(),
        target_end_date = col_date(format = ""),
        location = col_character(),
        location_name = col_character(),
        type = col_character(),
        quantile = col_double(),
        value = col_double()
      )) %>%
      bind_rows %>%
      filter(target %in% paste(1:4, "wk ahead cum death"))
    df_temp$model <- m
    df <- bind_rows(df, df_temp)
  }
  
  print("Cleaning data...")
  df <- df %>% filter(type=="quantile")
  
  # change location_name from None to US (where it's missing)
  df <- df %>% mutate(location_name = replace(location_name, 
                                             location=="US" & location_name=="None",
                                             "US"))
  
  # remove locations given by exclude_locations
  df <- df %>% filter(!(location %in% exclude_locations))
  
  # remove multiple forecasts for same target (only keep newest one)
  df <- df %>%
    group_by(model, target_end_date, location, target, quantile) %>%
    slice(which.max(forecast_date))
  
  # only keep those with all 23 quantiles
  df <- data.frame(
    df %>%
      group_by(model, forecast_date, location, target_end_date, target) %>%
      filter(n()==23))
  
  print("Adding ground truth...")
  colnames(truth)[colnames(truth) == "value"] <- "truth"
  df <- merge(df, truth[, c("date", "location", "truth")],
              by.x = c("target_end_date", "location"),
              by.y = c("date", "location"))
  print("Done.")
  return(df)
}



IS <- function(Y, l, u, alph){
  IS <- (u-l) + 2/alph * (l-Y) * (Y<l) + 2/alph * (Y-u) * (Y > u)
  return(IS=unlist(IS))
}



# input: vector of values alpha for WIS, df containing (Y, quantile forecast, levels)

WIS <- function(alpha, df){
  names(df)[2] <- "q"
  df <- df %>% spread(level,q)
  Y <- unlist(df[,1])
  K <- length(alpha)
  is <- matrix(NA, nrow = length(Y), ncol = K)
  colnames(is) <- alpha
  for (k in 1:K) {
    c.l <- which(names(df)==alpha[k]/2)
    c.u <- which(names(df)==(1-alpha[k]/2))
    l <- unlist(df[,c.l])
    u <- unlist(df[,c.u])
    is[,k] <- IS(Y, l, u, alph = alpha[k])
  }
  w <- alpha/2
  w0 <- 1/2
  m <- unlist(df[,which(names(df)==0.5)])
  wis <-  1/(K+1) * ( w0 * 2 * abs(m-Y) + is %*% w)
  return(mwis=mean(wis))
}



# evaluate WIS for a data.frame containing forecasts:
wis_table <- function(df){
  row_index <- names(df)[!(names(df) %in% c("quantile", "value"))]
  df_wide <- reshape(df, direction = "wide", timevar = "quantile",
                     v.names = "value", idvar = row_index)
  
  coverage_levels <- c(0:9/10, 0.95, 0.98) # median can be treated like the 0% PI
  
  # get weighted interval widths. Note that this already contains the weighting with alpha/2
  for(coverage in coverage_levels){
    df_wide[, paste0("wgt_iw_", coverage)] <-
      (1 - coverage)/2*(
        df_wide[paste0("value.", 1 - (1 - coverage)/2)] -
          df_wide[paste0("value.", (1 - coverage)/2)]
      )
  }
  
  # get weighted penalties. Note that this already contains the weighting with alpha/2,
  # which makes the terms simpler
  for(coverage in coverage_levels){
    q_u <- 1 - (1 - coverage)/2
    df_wide[, paste0("wgt_pen_u_", coverage)] <-
      pmax(0, df_wide$truth - df_wide[, paste0("value.", q_u)])
    
    q_l <- (1 - coverage)/2
    df_wide[, paste0("wgt_pen_l_", coverage)] <-
      pmax(0, df_wide[, paste0("value.", q_l)] - df_wide$truth)
  }
  
  df_wide$wgt_iw <- rowMeans(df_wide[, grepl("wgt_iw", colnames(df_wide))])
  df_wide$wgt_pen_u <- rowMeans(df_wide[, grepl("wgt_pen_u", colnames(df_wide))])
  df_wide$wgt_pen_l <- rowMeans(df_wide[, grepl("wgt_pen_l", colnames(df_wide))])
  df_wide$wis <- df_wide$wgt_iw + df_wide$wgt_pen_u + df_wide$wgt_pen_l
  
  return(df_wide)
}

mean_wis <- function(df){
  df_scores <- wis_table(df)
  mean_wis_score <- mean(df_scores$wis)
  return(mean_wis_score)
}