library("ggplot2")
df <- data.frame(x = rnorm(500))
n <- nrow(df)
df_stats <- with(df, data.frame(mean = mean(x),
                          ci95_lb = mean(x) - 1.96*sd(x)/sqrt(500),
                          ci95_ub = mean(x) + 1.96*sd(x)/sqrt(500))
)
## Mean as star and, CI as error bars
ggplot() + 
  geom_jitter(aes(x = 1, y = x), df, alpha = .5, width = .3, height = 0, shape = 3) +
  geom_point(aes(x = 1, y = mean), df_stats, size = 4, shape = 8, color = "red") +
  geom_errorbar(aes(x = 1, y = mean, ymin = ci95_lb, ymax = ci95_ub), df_stats, 
                width = .8, lwd = 1.1, color = "red") +
  theme_minimal()

## Mean and CI as crossbars (boxplot without error bars)
ggplot() + 
  geom_jitter(aes(x = 1, y = x), df, alpha = .5, width = .3, height = 0, shape = 3) +
  geom_crossbar(aes(x = 1, y = mean, ymin = ci95_lb, ymax = ci95_ub), df_stats, 
                width = .8, lwd = 1, fatten = 1, color = "red") +
  theme_minimal()


## ?geom_errrorbar example:
.df <- data.frame(
  trt = factor(c(1, 1, 2, 2)),
  resp = c(1, 5, 3, 4),
  group = factor(c(1, 2, 1, 2)),
  upper = c(1.1, 5.3, 3.3, 4.2),
  lower = c(0.8, 4.6, 2.4, 3.6)
)

p <- ggplot(.df, aes(trt, resp, colour = group))
p + geom_linerange(aes(ymin = lower, ymax = upper))
p + geom_pointrange(aes(ymin = lower, ymax = upper))
p + geom_crossbar(aes(ymin = lower, ymax = upper), width = 0.8, fatten = 3)
p + geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2)

