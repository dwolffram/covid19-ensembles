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

wis_decomposition <- function(df){
  df_scores <- wis_table(df)
  mean_wis_decomposition <- colMeans(df_scores[c("wgt_iw", "wgt_pen_u", "wgt_pen_l", "wis")])
  return(mean_wis_decomposition)
}
