require("ggforce"); require("ggplot2")
palette(RColorBrewer::brewer.pal(8, "Dark2"))
theme_mine <- function(){
  list(theme_minimal(),
       scale_color_manual(values = palette()[1:8]),
       scale_fill_manual( values = palette()[1:8]))
}


## EEE, EEV, VVV*
df <- data.frame(name = rep(c("EEE", "EEV", "VVV*"), each = 3), 
                 cl = as.factor(rep(c("a", "b", "c"), 3)), 
                 x = rep(c(-1, 1, -1), 3), 
                 y = rep(c(-1, -1, 1), 3),
                 a = c(rep(1, 6), rep(c(.5, .7, 1), 1)),
                 b = c(rep(.5, 6), rep(c(.5, .3, .5), 1)),
                 angle = c(rep(pi / 4, 3),          ## EEE
                           rep(pi / 4, 2), -pi / 4, ## EEV
                           rep(pi / 4, 2), -pi / 4) ## EVV_banana
                 )

## RENDER
ggplot(data = df) +
  geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                   angle = angle, color = cl), size = 1.5) +
  geom_text(aes(x = x, y = y, label = cl, color = cl), size = 10) +
  facet_wrap(vars(name)) +
  coord_fixed() +
  theme_mine()


#######
## demonstrate VVV > VVV_banana
df2 <- data.frame(name = c(rep("VVV input", 3), rep("VVV_banana", 7)), 
                  cl = as.factor(c(rep(c("a", "b", "c"), 2), rep("b", 4))), 
                  x = c(rep(c(-1, 1, -1), 2),     .5,   0,  .5, 0),
                  y = c(rep(c(-1, -1, 1), 2),     -1.5, -2, -.5, 0),
                  a = c(rep(c(.5, .4, 1), 2),     rep(.4, 4)), 
                  b = c(rep(c(.5, .4, .5), 2),    rep(.4, 4)),
                  angle = c(rep(c(0, 0, -pi / 4), 2), rep(0, 4)) ## EVV_banana
)

## RENDER
ggplot(data = df2) +
  geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                   angle = angle, color = cl), size = 1.5) +
  geom_text(aes(x = x, y = y, label = cl, color = cl), size = 10) +
  facet_wrap(vars(name)) +
  coord_fixed() +
  theme_mine()


#######
## demonstrate VVV > VVV_banana
df3 <- data.frame(
  name = factor(c(rep("VVV input", 3), "PCS, Cl b", rep("VVV_boomerang", 3)),
                   levels = c("VVV input", "PCS, Cl b", "VVV_boomerang")),
  cl = as.factor(c("a", "b", "c", "b", "a", "b", "c")),
  x = c(rep(c(-1, 1, -1),  1), 0,  rep(c(-1, 1, -1),  1)),
  y = c(rep(c(-1, -1, 1),  1), 0,  rep(c(-1, -1, 1),  1)),
  a = c(rep(c(.5, .7, 1),  1), .7, rep(c(.5, .7, 1),  1)), 
  b = c(rep(c(.5, .3, .5), 1), .3, rep(c(.5, .3, .5), 1)),
  angle = c(0, pi / 4, -pi / 4, 0, 0, pi / 4, -pi / 4) ## EVV_banana
)

## RENDER
ggplot(data = df3) +
  geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                   angle = angle, color = cl), size = 1.5) +
  geom_text(aes(x = x, y = y, label = cl, color = cl), size = 10) +
  facet_wrap(vars(name)) +
  coord_fixed() +
  theme_mine()

