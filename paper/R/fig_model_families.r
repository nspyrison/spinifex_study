require("ggforce")
require("ggplot2")
require("ggExtra")
require("magrittr")
require("spinifex")
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
bas3 <- spinifex:::interpolate_manual_tour(mt, .05)[,,17]


df <- data.frame(c(-1,1),c(-1,1),c(-1,1),c(-1,1))
fct1 <- spinifex::view_frame(bas1, EEE_p4_0_1_rep1,
  aes_args = list(color = clas, shape = clas),
  identity_args = list(size = 1.5, alpha = 0)) +
  this_theme +
  theme(axis.title =  element_text()) +
  labs(x = expression(paste(PC_j)), y = expression(paste(PC_k)), subtitle = "PCA \n\n Discrete jump to \n selected pair")# +
  #geom_text(aes(x = 0, y = .7, label = "Discrete jump to selected PC"), size = 3)
fct2 <- spinifex::view_frame(bas2, EEE_p4_0_1_rep1,
  aes_args = list(color = clas, shape = clas),
  identity_args = list(size = 1.5, alpha = 0)) +
  this_theme +
  #theme(axis.title =  element_text()) +
  labs(subtitle = "grand tour \n\n Animation through \n random bases") #+
  # geom_text(aes(x = 0, y = .7, label = "Animation through random \n  
  #               target bases"), size = 3)
fct3 <- spinifex::view_frame(
  bas3, EEE_p4_0_1_rep1, manip_var = 2,
  aes_args = list(color = clas, shape = clas),
  identity_args = list(size = 1.5, alpha = 0)) +
  this_theme +
  #theme(axis.title =  element_text()) +
  labs(subtitle = "radial tour \n\n Animation changing \n contribution of \n selected variable")# +
  # geom_text(aes(x = 0, y = .7, label = "Animation changing contribution \n 
  #               of selected variable"), size = 3)


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
.rang <- range(location_df$signal)
lvls <- levels(location_df$name)
x_nms <- c("1*V1 + 0*V2 \n (signal & noise respectively)", ".866*V1 + .5*V2", ".7071*V1 + .7071*V2")
# y_nms <- c("V2 (noise)", "-sin(30)*V1 + cos(30)*V2", "-sin(45)*V1 + cos(45)*V2")
for(i in 1:length(lvls)){
  g <- location_df[location_df$name==lvls[i], ] %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype=1) +
    geom_vline(xintercept = 2, linetype=2) +
    geom_density(aes(signal, fill = cluster), alpha = .5) +
    this_theme +
    theme(axis.title =  element_text()) +
    ggplot2::labs(x = x_nms[i], y = "") +
    labs(subtitle = lvls[i]) +
    xlim(.rang)
  assign(paste0("loc", i), g, envir = globalenv())
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
clust_d <- data.frame(
  name = c("EEI", "EEI", "EVI"),
  cluster = rep("(d)", 3),
  x = rep(-1, 3),
  y = rep(-1, 3),
  a = rep(.425, 3),
  b = rep(.425, 3),
  angle = rep(0, 3)
)
lvls <- levels(shape_df$name)
for(i in 1:length(lvls)){
  g <- shape_df[shape_df$name == lvls[i],] %>%
    ggplot() +
    ## Clusters a:c
    geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                     angle = angle, color = cluster), size = 1) +
    ## Cluster d
    geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b,
                     angle = angle, color = cluster),
                 data = clust_d[i, ],
                 size = .6, linetype = 2, alpha = .5) +
    coord_fixed() +
    this_theme +
    labs(subtitle = lvls[i])
  if(i != length(lvls)) ## Add text on first 2, but not the last one.
    
    g <- g + 
      ## Cluster letters a-c
      geom_text(aes(x = x, y = y, label = cluster, color = cluster), size = 7) +
      ## Cluster letter d
      geom_text(aes(x = x - .5, y = y + .5, color = cluster),
                data = clust_d[i, ], size = 4, alpha =.7,
                label = "(d)")
  assign(paste0("shp", i), g, envir = globalenv())
}



###### Dim ------
load("./apps_supplementary/data/EEE_p4_0_1_rep1.rda") ## load obj EEE_p4_0_1_rep1
load("./apps_supplementary/data/EEE_p6_0_1_rep1.rda") ## load obj EEE_p5_0_1_rep1
str(EEE_p4_0_1_rep1)
bas4 <- spinifex::basis_half_circle(EEE_p4_0_1_rep1)
bas6 <- spinifex::basis_half_circle(EEE_p6_0_1_rep1)
clas4 <- attr(EEE_p4_0_1_rep1, "cluster")
clas6 <- attr(EEE_p6_0_1_rep1, "cluster")
dim4 <- spinifex::view_frame(
  bas4, EEE_p4_0_1_rep1,
  aes_args = list(color = clas4, shape = clas4),
  identity_args = list(size = 1.5, alpha = 0)) +
  this_theme + 
  ggplot2::labs(subtitle = "3 cluster in 4 dim")
dim6 <- spinifex::view_frame(
  bas6, EEE_p6_0_1_rep1,
  aes_args = list(color = clas6, shape = clas6),
  identity_args = list(size = 1.5, alpha = 0)) +
  this_theme + 
  ggplot2::labs(subtitle = "4 cluster in 6 dim")
dim_txt <- ggplot() +
  geom_text(aes(0, 0), size = 3,
    label = "Cluster 'd', above, only exists \n when there are 6 dim, is \n spherical like the noise dim, \n but has cluster separation \n behind the plane of the \n other 3 isodensities.") +
  theme_void() +
  theme(text = element_text(hjust = .5, vjust = .5))
  
### Cowplot munging ------
require("cowplot")
.gg_empty <- ggplot() + theme_void()
fct_row <- plot_grid(fct1, fct2, fct3, nrow = 1)
loc_row <- plot_grid(loc1, loc2, loc3, nrow = 1)
shp_row <- plot_grid(shp1, shp2, shp3, nrow = 1)
dim_row <- plot_grid(dim4, dim6, dim_txt, nrow = 1)
gc()
gg_matrix <- plot_grid(fct_row, loc_row, shp_row, dim_row, ncol = 1, rel_heights = c(1,.8,.8,1))

header_row <- ggplot() + 
  labs(title = "Levels of the block") + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
header_matrix <- plot_grid(header_row, gg_matrix, ncol = 1, rel_heights = c(0.03, 1))


t_fct <- ggplot() + 
  labs(title = "factor") +
  theme_void()+
  theme(plot.title = element_text(angle = 90))
t_loc <- ggplot() +
  labs(title = "location") +
  theme_void()+
  theme(plot.title = element_text(angle = 90))
t_shp <- ggplot() +
  labs(title = "shape") +
  theme_void()+
  theme(plot.title = element_text(angle = 90))
t_dim <- ggplot() +
  labs(title = "dimension") +
  theme_void() +
  theme(plot.title = element_text(angle = 90))
tbl_col <- plot_grid(.gg_empty, t_fct, t_loc, t_shp, t_dim, ncol = 1, rel_heights = c(.8, 1.2,1.2,1,1))

final <- plot_grid(tbl_col, header_matrix, nrow = 1, rel_widths = c(0.05, 1))

.w = 6.25
.h = 9
.u = "in"
if(F)
  ggsave("./paper/figures/figExpFactors.pdf", final,
         device = "pdf", width = .w, height = .h, units = .u)

