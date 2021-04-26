require("ggplot2")
require("spinifex")

.u = "in"
.w = 6.25
.h = 9

tgt_sim_nm <- "EEV_p6_0_1_rep3"
tgt_fp <- paste0("./apps_supplementary/data/", tgt_sim_nm, ".rda") 
## Make data plot
load(tgt_fp, envir = globalenv())
dat <- EEV_p6_0_1_rep3
clas <- attr(dat, "cluster")
source("./paper/R/ggproto_pca_biplot.r")
if(F)
  file.edit("./paper/R/ggproto_pca_biplot.r")
this_theme <- list(
  scale_color_brewer(palette = "Dark2"),
  theme_void(),
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "off"),
  coord_fixed(),
  labs(x = "", y = "")
)
  

### PCA -----
pc_x <- c(2:4, 3)
pc_y <- c(rep(1, 3), 2)
for(i in 1:4){ ## creates P1:P4
  p <- ggplot() +
  ggproto_pca_biplot(dat, aes_clas = clas, x_pc_num = pc_x[i], y_pc_num = pc_y[i]) +
  this_theme + labs(x = paste0("PC", pc_x[i]))
  if(i %in% c(1, 4)) p <- p + labs(y = paste0("PC", pc_y[i]))
  assign(paste0("p", i), p, envir = globalenv())
}

### Grand tour -----
tpath_fp <- "./apps_supplementary/data/tpath_p6.rda"
load(tpath_fp, envir = globalenv()) ## tpath_p6
for(i in 1:4){ ## creates P1:P4
  bas <- matrix(tpath_p6[,, i], nrow = 6, ncol = 2)
  p <- view_frame(bas, dat, axes = "left",
                  aes_args = list(color = clas, shape = clas),
                  identity_args = list(size = 1)) + this_theme
  
  assign(paste0("p", 4 + i), p, envir = globalenv())
}

### Radial tour -----
## bas_p6
.ang <- seq(0, pi, length.out = 7)[-7] ## p + 1
.u_circ_p6 <- as.matrix(data.frame(x = sin(.ang), y = cos(.ang)))
bas_p6 <- tourr::orthonormalise(.u_circ_p6)
mv <- 6
mt <- manual_tour(bas_p6, mv, ang = .29)
if(F)
  for(i in 1:dim(mt)[3]){ ## creates P1:P4
    print(paste0("i: ", i, ", norm=", sqrt(mt[mv,, i][1]^2 + mt[mv,, i][2]^2)))
  }## Use bases: 1, 6, 12, 16
tgt_bases <- c(1, 6, 12, 16)
for(i in 1:4){
  this_bas <- mt[,, tgt_bases[i]]
  p <- view_frame(this_bas, dat, axes = "left", manip_var = 1,
                  aes_args = list(color = clas, shape = clas),
                  identity_args = list(size = 1)) + this_theme
  assign(paste0("p", 8 + i), p, envir = globalenv())
}

### Text cells -----
require("ggpmisc")
require("dplyr")

ggplot() + geom_table(data = data.tb, aes(x,y,label = tb))
text1 <- tibble(`PCA                                                               ` = 
                c("- Inputs: x, y axes in [PC1, ... PC4]",
                  "- Differce: not animated, discrete change",
                  "- Illustrated: 3 of the 12 unique",
                  "     PC combinations"))
tb1 <- tibble(x = 0, y = 0, text1 = list(text1))
gt1 <- ggplot() + 
  geom_table(data = tb1, aes(x,y,label = text1),
             table.theme = ttheme_gtminimal, table.hjust = 0 ) + this_theme

text2 <- tibble(`Grand                                                               ` = 
                  c("- Inputs: none",
                    "- Differce: animated through randomly",
                    "     selected target bases",
                    "- Illustrated: first 3 such target bases"))
tb2 <- tibble(x=0, y=0, text2 = list(text2))
gt2 <- ggplot() + 
  geom_table(data = tb2, aes(x,y,label = text2),
             table.theme = ttheme_gtminimal, table.hjust = 0 ) + this_theme
text3 <- 
  tibble(`Radial                                                               ` =
           c("- Inputs: manipulation variable in [1, ... 6]",
             "- Differce: animates selected variable to",
             "     norm=1, norm=0, then back to start",
             "- Illustrated: target bases rotating variable 6"))
tb3 <- tibble(x = 0, y = 0, text3 = list(text3))
gt3 <- ggplot() + 
  geom_table(data = tb3, aes(x,y,label = text3),
             table.theme = ttheme_gtminimal, table.hjust = 0 ) + this_theme
### Bring it together ------
## a littel finachy, may need to restart and clear envirnment to get it to work.
gc()
(fig <- cowplot::plot_grid(gt1, p1, p2, p3, #p4, ## pca
                          gt2, p5, p6, p7, #p8, ## grand
                          gt3, p9, p10, p11, #p12, ## radial
                          nrow = 3, ncol = 4, rel_widths = c(3, 2,2,2)))

if(F)
  ggsave("./paper/figures/figFactor.pdf", fig,
         device = "pdf", width = .h, height = 3/5*.h, units = .u)

