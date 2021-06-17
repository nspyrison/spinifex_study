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
        legend.position = "off",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
)

###### Factor ------
load("./apps_supplementary/data/EEE_p4_0_1_rep1.rda") ## load obj EEE_p4_0_1_rep1
clas <- attr(EEE_p4_0_1_rep1, "cluster")
bas1 <- spinifex::basis_pca(EEE_p4_0_1_rep1)
gt <- tourr::save_history(EEE_p4_0_1_rep1, tour_path = grand_tour(), max_bases = 1)
bas2 <- matrix(gt[[1]], nrow=4, ncol=2)
bas3_st <- basis_half_circle(EEE_p4_0_1_rep1)
mt <- manual_tour(bas3_st, manip_var = 2)
bas3 <- mt[,, 17]

fct1 <- spinifex::view_frame(
  bas1, EEE_p4_0_1_rep1, axes = "left",
  aes_args = list(color = clas, shape = clas),
  identity_args = list(size = 1.5, alpha = .7)) +
  this_theme + 
  theme(axis.title =  element_text()) +
  labs(x = "PC1", y = "PC2", subtitle = "PCA")
fct2 <- spinifex::view_frame(
  bas2, EEE_p4_0_1_rep1, axes = "left",
  aes_args = list(color = clas, shape = clas),
  identity_args = list(size = 1.5, alpha = .7)) +
  this_theme + 
  theme(axis.title =  element_text()) +
  labs(x = "Random basis", y = "", subtitle = "grand tour")
fct3 <- spinifex::view_frame(
  bas3, EEE_p4_0_1_rep1, 
  axes = "left", manip_var = 2,
  aes_args = list(color = clas, shape = clas),
  identity_args = list(size = 1.5, alpha = .7)) +
  this_theme + 
  theme(axis.title =  element_text()) +
  labs(x = "Variable 2, full contribution", y = "", subtitle = "radial tour")


###### Location ------
### 0/100 33/66, 50/50
# cl1 <- rnorm(140, 0, 1)
# cl2 <- rnorm(140, 2, 1)
##     Cluster A         Cluster B
x <- c(rnorm(140, 0, 1), rnorm(140, 2, 1)) ## signal
y <- c(rnorm(140, 0, 1), rnorm(140, 0, 1)) ## noise
location_df <- data.frame( ## angles are 0, 30, 45 respectively
  name = factor(rep(c("0/100%", "33/66%", "50/50%"), each = 2 * 140),
                levels = c("0/100%", "33/66%", "50/50%")),
  cluster = as.factor(rep(rep(c("a", "b"), each = 140), times = 3)),
  signal = c(cos(0)*x + sin(0)*y, cos(pi/6)*x + sin(pi/6)*y, cos(pi/4)*x + sin(pi/4)*y),
  noise  = c(-sin(0)*x + cos(0)*y, -sin(pi/6)*x + cos(pi/6)*y, -sin(pi/4)*x + cos(pi/4)*y)
)
lvls <- levels(location_df$name)
x_nms <- c("V1 (signal)", "cos(30)*V1 + sin(30)*V2", "cos(45)*V1 + sin(45)*V2")
y_nms <- c("V2 (noise)", "-sin(30)*V1 + cos(30)*V2", "-sin(45)*V1 + cos(45)*V2")
for(i in 1:length(lvls)){
  g <- location_df[location_df$name==lvls[i], ] %>% 
    ggplot(data = ) +
    geom_point(aes(signal, noise, color = cluster, shape = cluster), size = 1.5) +
    #facet_wrap(vars(name)) +
    coord_fixed() +
    this_theme + 
    theme(axis.title =  element_text()) +
    ggplot2::labs(x = x_nms[i], y = y_nms[i]) +
    labs(subtitle = lvls[i])
  assign(paste0("loc", i), ggMarginal(g, groupFill = TRUE), envir = globalenv())
}


###### Shape ------
## EEE, EEV, EVV*
shape_df <- data.frame(
  name = factor(c(rep(c("EEE", "EEV"), each = 3), rep("EVV, banana transformed", 7)),
                levels = c("EEE", "EEV", "EVV, banana transformed")),
  cluster = as.factor(c(rep(c("a", "b", "c"), 3), rep("b", 4))),
  x = c(rep(c(-1, 1, -1), 3),              .5,   0,  .5,  0),
  y = c(rep(c(-1, -1, 1), 3),              -1.5, -2, -.5, 0),
  a = c(rep(1, 6),  rep(c(.5, .4, 1), 1),  rep(.4, 4)),
  b = c(rep(.5, 6), rep(c(.5, .4, .5), 1), rep(.4, 4)),
  angle = c(rep(pi / 4, 3),           ## EEE
            rep(pi / 4, 2), -pi / 4,  ## EEV
            0, 0, -pi / 4, rep(0, 4)) ## EVV_banana
)
lvls <- levels(shape_df$name)
for(i in 1:length(lvls)){
  g <- shape_df[shape_df$name == lvls[i],] %>%
    ggplot() +
    geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                     angle = angle, color = cluster), size = 1.5) +
    geom_text(aes(x = x, y = y, label = cluster, color = cluster), size = 8) +
    coord_fixed() +
    this_theme +
    labs(subtitle = lvls[i])
  assign(paste0("shp", i), g, envir = globalenv())
}
shp3


###### Dim ------
##     Cluster A         Cluster B
load("./apps_supplementary/data/EEE_p4_0_1_rep1.rda") ## load obj EEE_p4_0_1_rep1
load("./apps_supplementary/data/EEE_p6_0_1_rep1.rda") ## load obj EEE_p5_0_1_rep1
str(EEE_p4_0_1_rep1)
bas4 <- spinifex::basis_pca(EEE_p4_0_1_rep1)
bas6 <- spinifex::basis_pca(EEE_p6_0_1_rep1)
clas4 <- attr(EEE_p4_0_1_rep1, "cluster")
clas6 <- attr(EEE_p6_0_1_rep1, "cluster")
dim4 <- spinifex::view_frame(
  bas4, EEE_p4_0_1_rep1, axes = "left",
  aes_args = list(color = clas4, shape = clas4),
  identity_args = list(size = 1.5, alpha = .7)) +
  this_theme + 
  theme(axis.title =  element_text()) +
  ggplot2::labs(x = "PC1", y = "PC2", subtitle = "3 cluster in 4 dim")
dim6 <- spinifex::view_frame(
  bas6, EEE_p6_0_1_rep1, axes = "left",
  aes_args = list(color = clas6, shape = clas6),
  identity_args = list(size = 1.5, alpha = .7)) +
  this_theme + 
  theme(axis.title =  element_text()) +
  ggplot2::labs(x = "PC1", y = "PC2", subtitle = "4 cluster in 6 dim")

### Cowplot munging ------
require("cowplot")
.gg_empty <- ggplot() + theme_void()
fct_row <- plot_grid(fct1, fct2, fct3, nrow = 1)
loc_row <- plot_grid(loc1, loc2, loc3, nrow = 1)
shp_row <- plot_grid(shp1, shp2, shp3, nrow = 1)
dim_row <- plot_grid(dim4, dim6, .gg_empty, nrow = 1)
gc()
gg_matrix <- plot_grid(fct_row, loc_row, shp_row, dim_row, ncol = 1, rel_heights = c(1,1,.7,1))

header_row <- ggplot() + 
  labs(title = "Levels of the block") + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
header_matrix <- plot_grid(header_row, gg_matrix, ncol = 1, rel_heights = c(0.03, 1))

# t_blk <- ggplot() + 
#   labs(subtitle = "Experimental factors:") + 
#   theme_void()+
#   theme(plot.title = element_text(hjust = 0.5))
t_fct <- ggplot() + 
  labs(title = "factor") + 
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, angle = 90))
t_loc <- ggplot() + 
  labs(title = "location") + 
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, angle = 90))
t_shp <- ggplot() + 
  labs(title = "shape") + 
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, angle = 90))
t_dim <- ggplot() + 
  labs(title = "dimension") + 
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, vjust = .5, angle = 90))
tbl_col <- plot_grid(.gg_empty, t_fct, t_loc, t_shp, t_dim, ncol = 1, rel_heights = c(.7, 1.3,1.3,1,1))

final <- plot_grid(tbl_col, header_matrix, nrow = 1, rel_widths = c(0.05, 1))

.w = 6.25
.h = 9
.u = "in"
if(F)
  ggsave("./paper/figures/figExpFactors.pdf", final,
         device = "pdf", width = .w, height = .h, units = .u)

