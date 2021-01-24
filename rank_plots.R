df <- load_scores("scores/ensemble_scores_1wk.csv", remove_revisions=TRUE)

temp <- df %>%
  filter(window_size==4, location!="US")

temp %>%
  group_by(target_end_date, model) %>%
  summarize(meanWIS = mean(wis))

temp$model <- factor(temp$model, labels=c("EWA", "MED", "INV", "V[2]", "V[3]", "V[4]", "GQRA[2]", "GQRA[3]", "GQRA[4]", 
                                                "QRA[2]", "QRA[3]", "QRA[4]", "Baseline"))
factor(temp$model)

df_rank <- temp %>%
  group_by(target_end_date, model) %>%
  summarize(meanWIS = mean(wis)) %>%
  group_by(target_end_date) %>%
  arrange(model, meanWIS) %>% 
  mutate(rank=rank(meanWIS)) %>%
  arrange(target_end_date)

df_rank <- df_rank %>%
  group_by(model) %>%
  mutate(meanRank=mean(rank))

df_rank$is_ensemble <- (df_rank$model != "Baseline")

ggplot(df_rank, aes(x=reorder(model, -meanRank), fill=is_ensemble, y=rank)) + 
  geom_boxplot(alpha=0.3) +
  stat_summary(fun=mean, geom="point", shape=3, size=2) +
  coord_flip() +
  xlab("Model") +
  ylab("Rank") +
  scale_x_discrete("Model", labels = parse(text = levels(reorder(df_rank$model, -df_rank$meanRank))))

factor(df$model)
unique(df$model)
df$model

df_rank$model <- factor(df_rank$model, labels=c("EWA", "MED", "INV", "V[2]", "V[3]", "V[4]", "GQRA[2]", "GQRA[3]", "GQRA[4]", 
                          "QRA[2]", "QRA[3]", "QRA[4]", "Baseline"))
