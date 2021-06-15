require("ggforce")
require("ggplot2")
require("ggExtra")
require("magrittr")
palette(RColorBrewer::brewer.pal(8, "Dark2"))
this_theme <- list(
  theme_bw(),
  scale_color_manual(values = palette()[1:8]),
  scale_fill_manual( values = palette()[1:8]),
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "off")
)

###### Factor ------

###### Location ------
### 0/100 33/66, 50/50
# cl1 <- rnorm(140, 0, 1)
# cl2 <- rnorm(140, 2, 1)
##     Cluster A         Cluster B
x <- c(rnorm(140, 0, 1), rnorm(140, 1, 1)) ## signal
y <- c(rnorm(140, 0, 1), rnorm(140, 0, 1)) ## noise
location_df <- data.frame( ## angles are 0, 30, 45 respectively
  name = factor(rep(c("0/100%", "33/66%", "50/50%"), each = 2 * 140),
                levels = c("0/100%", "33/66%", "50/50%")),
  cluster = as.factor(rep(rep(c("a", "b"), each = 140), times = 3)),
  signal = c(cos(0)*x + sin(0)*y, cos(pi/6)*x + sin(pi/6)*y, cos(pi/4)*x + sin(pi/4)*y),
  noise  = c(cos(0)*y + sin(0)*x, cos(pi/6)*y + sin(pi/6)*x, cos(pi/4)*y + sin(pi/4)*x)
)
lvls <- levels(location_df$name)
for(i in 1:length(lvls)){
  g <- location_df[location_df$name==lvls[i], ] %>% 
    ggplot(data = ) +
    geom_point(aes(signal, noise, color = cluster, shape = cluster), size = 1.5) +
    #facet_wrap(vars(name)) +
    coord_fixed() +
    this_theme + 
    theme(axis.title =  element_text()) +
    ggplot2::labs(x = "signal", y = "noise") +
    ggtitle(lvls[i])
  assign(paste0("loc", i, "_gg"), ggMarginal(g, groupFill = TRUE), envir = globalenv())
}
require("cowplot") ## Cannot patchwork ggmarginals.
loc1_gg
loc2_gg
loc3_gg

###### Shape ------
## EEE, EEV, EVV*
shape_df <- data.frame(
  name = factor(c(rep(c("EEE", "EEV"), each = 3), rep("EVV, banana transformed", 7)),
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
## ggplot
(shape_gg <-
    ggplot(data = shape_df) +
    geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                     angle = angle, color = cluster), size = 1.5) +
    geom_text(aes(x = x, y = y, label = cluster, color = cluster), size = 10) +
    facet_wrap(vars(name)) +
    coord_fixed() +
    this_theme
)



###### Dim ------



### Output figure ------
.w = 6.25
.h = 9
.u = "in"
if(F)
  ggsave("./paper/figures/figModelFam.pdf", XXX,
         device = "pdf", width = .w, height = .w / 2, units = .u)
