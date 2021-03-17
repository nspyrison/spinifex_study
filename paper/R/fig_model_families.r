require("ggforce")
require("ggplot2")
palette(RColorBrewer::brewer.pal(8, "Dark2"))
this_theme <- list(
  theme_minimal(),
  scale_color_manual(values = palette()[1:8]),
  scale_fill_manual( values = palette()[1:8]),
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
)

###### model_families.png ------
## EEE, EEV, VVV*
model_fam_df <- data.frame(
  name = factor(c(rep(c("EEE", "EEV"), each = 3), rep("banana transformed EVV", 7)),
                levels = c("EEE", "EEV", "banana transformed EVV")),
  cluster = as.factor(c(rep(c("a", "b", "c"), 3), rep("b", 4))),
  x = c(rep(c(-1, 1, -1), 3),              .5,   0,  .5,  0),
  y = c(rep(c(-1, -1, 1), 3),              -1.5, -2, -.5, 0),
  a = c(rep(1, 6),  rep(c(.5, .4, 1), 1),  rep(.4, 4)),
  b = c(rep(.5, 6), rep(c(.5, .4, .5), 1), rep(.4, 4)),
  angle = c(rep(pi / 4, 3),           ## EEE
            rep(pi / 4, 2), -pi / 4,  ## EEV
            0, 0, -pi / 4, rep(0, 4)) ## EVV_banana
)

## RENDER
ggplot(data = model_fam_df) +
  geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                   angle = angle, color = cluster), size = 1.5) +
  geom_text(aes(x = x, y = y, label = cluster, color = cluster), size = 10) +
  facet_wrap(vars(name)) +
  coord_fixed() +
  this_theme
  
ggsave("./paper/figures/figModelFam.png", width = 8, height = 8/3, units = "in")
