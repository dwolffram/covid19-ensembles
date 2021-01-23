ae <- function(df){
  ae_df <- df %>%
    filter(quantile==0.5) %>%
    mutate(ae = abs(value - truth)) %>%
    select(-c(quantile, value))
  return(ae_df)
}

# wis <- function(df){
#   row_index <- names(df)[!(names(df) %in% c("quantile", "value"))]
#   df_wide <- reshape(df, direction = "wide", timevar = "quantile",
#                      v.names = "value", idvar = row_index)
#   
#   coverage_levels <- c(0:9/10, 0.95, 0.98) # median can be treated like the 0% PI
#   
#   # get weighted interval widths. Note that this already contains the weighting with alpha/2
#   for(coverage in coverage_levels){
#     df_wide[, paste0("wgt_iw_", coverage)] <-
#       (1 - coverage)/2*(
#         df_wide[paste0("value.", 1 - (1 - coverage)/2)] -
#           df_wide[paste0("value.", (1 - coverage)/2)]
#       )
#   }
#   
#   # get weighted penalties. Note that this already contains the weighting with alpha/2,
#   # which makes the terms simpler
#   for(coverage in coverage_levels){
#     q_u <- 1 - (1 - coverage)/2
#     df_wide[, paste0("wgt_pen_u_", coverage)] <-
#       pmax(0, df_wide$truth - df_wide[, paste0("value.", q_u)])
#     
#     q_l <- (1 - coverage)/2
#     df_wide[, paste0("wgt_pen_l_", coverage)] <-
#       pmax(0, df_wide[, paste0("value.", q_l)] - df_wide$truth)
#   }
#   
#   df_wide$wgt_iw <- rowMeans(df_wide[, grepl("wgt_iw", colnames(df_wide))])
#   df_wide$wgt_pen_u <- rowMeans(df_wide[, grepl("wgt_pen_u", colnames(df_wide))])
#   df_wide$wgt_pen_l <- rowMeans(df_wide[, grepl("wgt_pen_l", colnames(df_wide))])
#   df_wide$wis <- df_wide$wgt_iw + df_wide$wgt_pen_u + df_wide$wgt_pen_l
#   
#   df_wide <- df_wide %>%
#     select(!ends_with(as.character(0:9)))
#   
#   return(df_wide)
# }

wis <- function(df){
  row_index <- names(df)[!(names(df) %in% c("quantile", "value"))]
  df_wide <- reshape(df, direction = "wide", timevar = "quantile",
                     v.names = "value", idvar = row_index)
  
  coverage_levels <- c(1:9/10, 0.95, 0.98) # treat median separately
  
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
  
  # median
  df_wide$wgt_pen_u_0 <- pmax(0, df_wide$truth - df_wide$value.0.5)/2
  df_wide$wgt_pen_l_0 <- pmax(0, df_wide$value.0.5 - df_wide$truth)/2
  
  
  df_wide$wgt_iw <- rowSums(df_wide[, grepl("wgt_iw", colnames(df_wide))])/11.5
  df_wide$wgt_pen_u <- rowSums(df_wide[, grepl("wgt_pen_u", colnames(df_wide))])/11.5
  df_wide$wgt_pen_l <- rowSums(df_wide[, grepl("wgt_pen_l", colnames(df_wide))])/11.5
  df_wide$wis <- df_wide$wgt_iw + df_wide$wgt_pen_u + df_wide$wgt_pen_l
  
  df_wide <- df_wide %>%
    select(!ends_with(as.character(0:9)))
  
  return(df_wide)
}

score_forecasts <- function(df, scores=c("ae", "wis"), keep_truth=FALSE){
  if(!'truth' %in% colnames(df)){
    warning('No truth data provided. Automatically added latest data.')
    df <- add_truth(df)
  }
  
  df_scores <- list()
  for (s in scores){
    df_scores[[s]] <- get(s)(df)
  }
  df_scores <- reduce(df_scores, merge)
  
  if(!keep_truth){
    df_scores <- df_scores %>% select(-truth)
  }
  
  return(df_scores)
}

mean_wis <- function(df){
  df_scores <- wis(df)
  mean_wis_score <- mean(df_scores$wis)
  return(mean_wis_score)
}

