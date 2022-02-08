## Setup ----
{
  require("ggplot2")
  require("spinifex")
  require("ggpmisc")
  require("dplyr")
  require("GGally")
  
  this_theme <- list(
    scale_color_brewer(palette = "Dark2"),
    scale_fill_brewer( palette = "Dark2"),
    theme_void(),
    theme(axis.title = element_text(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "off"),
    coord_fixed(),
    labs(x = "", y = "")
  )
  .u = "in"; .w = 6.25; .h = 9;
  
  .sim_nm <- "EEV_p6_33_66_rep2" #"EEV_p6_0_1_rep3"
  .fp <- paste0("./apps_supplementary/data/", .sim_nm, ".rda")
  ## Make data plot
  load(.fp, envir = globalenv())
  dat <- get(.sim_nm)
  clas <- as.factor(attr(dat, "cluster"))
  source("./paper/R/9_util_funcs.r")
  if(F)
    file.edit("./paper/R/9_util_funcs.r")
}


## figFactor.pdf, NOT IN USE  -----
# a figure showing 3 example frames across each factor
# same data, text description and 3 frames from each visualization
message("NOT IN USE, SKIPPING")
if(F){
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
  gt_fp <- "./apps_supplementary/data/tpath_p6.rda"
  load(gt_fp, envir = globalenv()) ## loads an obj, tpath_p6
  for(i in 1:4){ ## creates P1:P4
    bas <- matrix(tpath_p6[,, i], nrow = 6, ncol = 2)
    p <- ggtour(bas, dat) +
      proto_basis() +
      proto_point(list(color = clas, shape = clas)) + this_theme
    assign(paste0("p", 4 + i), p, envir = globalenv())
  }
  
  
  ### Radial tour -----
  ## bas_p6
  bas_p6 <- basis_half_circle(dat)
  mv <- 6
  mt <- manual_tour(bas_p6, mv)
  ggt <- ggtour(mt, dat, angle = .29) +
    proto_default()
  animate_plotly(ggt)
  
  mt_interp <- mt %>%
    spinifex:::interpolate_manual_tour(angle = .29)
  if(F) ## check mv norms of each frame
    for(i in 1:dim(mt_interp)[3]){ ## creates P1:P4
      print(paste0("i = ", i, ", manip_var norm = ",
                   round(sqrt(mt_interp[mv, 1, i]^2 + mt_interp[mv, 2, i]^2), 2)))
    }## Use bases: 1, 6, 12, 16
  tgt_bases <- c(1, 4, 11, 13)
  for(i in 1:4){
    this_bas <- mt_interp[,, tgt_bases[i]]
    p <- ggtour(this_bas, dat) +
      proto_point(aes_args = list(color = clas, shape = clas)) +
      proto_basis() +
      proto_origin() +
      this_theme
    assign(paste0("p", 8 + i), p, envir = globalenv())
  }
  
  
  ### Text cells -----
  ggplot() + geom_table(data = data.tb, aes(x,y,label = tb))
  text1 <- tibble(`PCA                                                               ` =
                    c("- Inputs: x, y axes in [PC1, ... PC4]",
                      "- Transition: not animated, discrete change",
                      "- Illustrated: 3 of the 12 unique",
                      "     PC combinations"))
  tb1 <- tibble(x = 0, y = 0, text1 = list(text1))
  gt1 <- ggplot() +
    geom_table(data = tb1, aes(x,y,label = text1),
               table.theme = ttheme_gtminimal, table.hjust = 0 ) + this_theme
  
  text2 <- tibble(`Grand                                                               ` =
                    c("- Inputs: none",
                      "- Transition: animated through randomly",
                      "     selected target bases",
                      "- Illustrated: first 3 such target bases"))
  tb2 <- tibble(x=0, y=0, text2 = list(text2))
  gt2 <- ggplot() +
    geom_table(data = tb2, aes(x,y,label = text2),
               table.theme = ttheme_gtminimal, table.hjust = 0 ) + this_theme
  text3 <-
    tibble(`Radial                                                               ` =
             c("- Inputs: manipulation variable in [1, ... 6]",
               "- Transition: animates selected variable: ",
               "     norm=1, norm=0, then back to start",
               "- Illustrated: target bases rotating variable 6"))
  tb3 <- tibble(x = 0, y = 0, text3 = list(text3))
  gt3 <- ggplot() +
    geom_table(data = tb3, aes(x,y,label = text3),
               table.theme = ttheme_gtminimal, table.hjust = 0 ) + this_theme
  ### Bring it together ------
  ## a littel finachy, may need to restart and clear envirnment to get it to work.
  gc()
  (fig <- cowplot::plot_grid(gt1, p1, p2, p3,   #p4, ## pca
                             gt2, p5, p6, p7,   #p8, ## grand
                             gt3, p9, p10, p11, #p12, ## radial
                             nrow = 3, ncol = 4, rel_widths = c(3, 2, 2, 2)))
  
  if(F)
    ggsave("./paper/figures/figFactor.pdf", fig,
           device = "pdf", width = .h, height = 3/5 * .h, units = .u)
}

## fig_pca_splom.pdf ----

### PCA -----
str(dat)
str(clas)
pca_obj <- prcomp(dat)
pca_proj <- as.data.frame(cbind(pca_obj$x[, 1:4], as.factor(clas)))

gg_pca <- GGally::ggpairs(
  pca_proj,
  mapping = aes(color = clas, fill = clas, shape = clas),
  columns = 1:4,
  #diag = "blank",
  upper = "blank",
  lower = list(continuous = wrap("points", alpha = 0.7, size=1)),
  columnLabels = paste0("PC", 1:4)) + 
  theme_bw() +
  theme(axis.ticks = element_blank(), 
        axis.text = element_blank()) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer( palette = "Dark2")

if(F)
  ggsave("./paper/figures/fig_pca_splom.pdf", gg_pca,
         device = "pdf", width = .w, height = .w, units = .u)

