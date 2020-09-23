require("ggforce"); require("ggplot2")
?geom_ellipse

df <- data.frame(name = rep(c("EEE", "EEV", "VVV_banana", "EVV_boomerang"), each = 3), 
                 cl = as.factor(rep(c("a", "b", "c"), 4)), 
                 x = rep(c(-1, 1, -1), 4), 
                 y = rep(c(-1, -1, 1), 4),
                 a = c(rep(1, 6), rep(c(.4, .3, 1), 2)),
                 b = c(rep(.5, 6), rep(c(.4, .3, .5), 2)),
                 angle = c(rep(pi / 4, 3),          ## EEE
                           rep(pi / 4, 2), -pi / 4, ## EEV
                           rep(pi / 4, 2), -pi / 4, ## EVV_banana
                           rep(pi / 4, 2), pi / 4)  ## EVV_boomerang
)

palette(RColorBrewer::brewer.pal(8, "Dark2"))
theme_mine <- function(){
  list(theme_minimal(),
       scale_color_manual(values = palette()[1:8]),
       scale_fill_manual( values = palette()[1:8]))
}

# Rotation
# Note that it expects radians and rotates the ellipse counter-clockwise
ggplot(data = df) +
  ## cl a
  geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                   angle = angle, color = cl), size = 1.5) +
  geom_text(aes(x = x, y = y, label = cl, color = cl), size = 10) +
  facet_wrap(vars(name)) +
  coord_fixed() +
  theme_mine()


