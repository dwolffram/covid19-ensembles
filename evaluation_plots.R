setwd("/home/dwolffram/covid19-ensembles")

results = read_csv('results/results2.csv')

results_long <- results %>% 
  pivot_longer(names(results)[3:length(results)], names_to = "model", values_to = "wis")

ggplot(data = results_long, aes(x = test_date, y = wis, colour = model)) +
  geom_line() +
  facet_wrap(~window_size)

ggplot(data=results, aes(x=test_date, y=V2)) +
  geom_point()  +
  facet_wrap(~window_size) + 
  geom_segment( aes(x=test_date, xend=test_date, y=0, yend=V2))

results$median <- apply(results[3:length(results)], 1, median, na.rm = T)

plot_comparison <- function(model_name="EWA"){
  results %>% mutate(Color = ifelse(get(model_name)-median < 0, "green", "red")) %>%
    ggplot(aes(x=test_date, y=get(model_name)-median, color=Color)) +
    geom_point()  +
    facet_wrap(~window_size) + 
    geom_segment( aes(x=test_date, xend=test_date, y=0, yend=get(model_name)-median))+
    scale_color_identity() +
    ylab(paste(model_name, "-", "median"))
}

plot_comparison("EWA")
plot_comparison("V3")
plot_comparison("GQRA2")
plot_comparison("V2")
