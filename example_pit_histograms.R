library(ggplot2)
library(tidyverse)


pit_histogram <- function(pits){
  ggplot(data.frame(PIT=pits), aes(PIT)) + 
    geom_histogram(breaks = seq(0, 1, 0.1), aes(y=..density..), color="black", fill = "black", alpha = 0.3) +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = function(x) ifelse(x == 0, "0", x)) +
    scale_y_continuous(limits=c(0, 1.75), labels = function(y) ifelse(y == 0, "0", y)) +
    labs(x="PIT", y="Density") +
    geom_segment(aes(x=0,xend=1,y=1,yend=1), linetype="dashed", color="black") +
    theme_gray(base_size=12)
}

s <- rnorm(1000, 0, 1)

# uniform
pits <- pnorm(s, 0, 1)
pit_histogram(pits)
ggsave('plots/examples/PIT/pit_uniform.png', width=6, height=6, dpi=500, unit='cm', device='png')


# overdispersed
pits <- pnorm(s, 0, 3)
pit_histogram(pits)
ggsave('plots/examples/PIT/pit_overdispersed.png', width=6, height=6, dpi=500, unit='cm', device='png')

# underdispersed
pits <- pnorm(s, 0, 0.5)
pit_histogram(pits)
ggsave('plots/examples/PIT/pit_underdispersed.png', width=6, height=6, dpi=500, unit='cm', device='png')

# overprediction (positive bias)
pits <- pnorm(s, 1, 1)
pit_histogram(pits)
ggsave('plots/examples/PIT/pit_pos_bias.png', width=6, height=6, dpi=500, unit='cm', device='png')

# underprediction (negative bias)
pits <- pnorm(s, -1, 1)
pit_histogram(pits)
ggsave('plots/examples/PIT/pit_neg_bias.png', width=6, height=6, dpi=500, unit='cm', device='png')


