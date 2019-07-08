library(tidyverse)

# Plot of observed "data" with effect size
#============================

n     <- 30
mu    <- c(1.25, 1.3, 1.4)
sigma <- .3

set.seed(88)

df <- tibble(expression = c(rnorm(n = n, mean = mu[1], sd = sigma),
                   rnorm(n = n, mean = mu[2], sd = sigma),
                   rnorm(n = n, mean = mu[3], sd = sigma)),
             type = rep(LETTERS[1:3], each = n))

p1 <- ggplot(df, aes(x = type, y = expression)) +
  geom_boxplot() +
  theme_bw()

ggsave("observed-plot.png", p1, width = 6.5, height = 5)

F_observed <- anova(lm(expression ~ type, data = df))$`F value`[1]


# Plots and Fs under the null
#============================

mu    <- 1.3
nsims <- 6

null_df <- tibble(expression = rnorm(n = n * 3, mean = mu, sd = sigma),
                 type = rep(LETTERS[1:3], each = n),
                 it   = 1)
null_df$`F` <- anova(lm(expression ~ type, data = null_df))$`F value`[1] %>%
  round(digits = 3)

for (i in 2:nsims) {
  new_df <- tibble(expression = rnorm(n = n * 3, mean = mu, sd = sigma),
                   type = rep(LETTERS[1:3], each = n),
                   it   = i)
  new_df$`F` <- anova(lm(expression ~ type, data = new_df))$`F value`[1] %>%
    round(digits = 3)
  null_df <- bind_rows(null_df, new_df)
}

null_df$`F` = factor(null_df$`F`, levels = pull(distinct(null_df, `F`)))

p2 <- ggplot(null_df, aes(x = type, y = expression)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~`F`, labeller = label_both, ncol = 3)

ggsave("null-plots.png", p2, width = 6.5, height = 5)


# Observed F against reference dist
#==================================

x_max <- round(F_observed) + 1
p3 <- ggplot(data.frame(x = c(0, x_max)), aes(x = x)) +
  stat_function(fun = stats::df, args = list(df1 = 2, df2 = 3 * (n - 1))) +
  geom_vline(xintercept = F_observed, col = "tomato", lty = 2) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, x_max), expand = c(0, 0)) +
  theme(axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank()) +
  ggtitle("Reference Distribution of F")

ggsave("null-dist.png", p3, width = 5, height = 3)