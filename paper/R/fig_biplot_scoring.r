## Setup -----
{
  require("ggplot2")
  require("spinifex")
  require("magrittr")
  require("patchwork")
  
  ## For figClSep
  set.seed(2022)
  .n <- 100
  cl1  <- data.frame(
    V1 = rnorm(n = .n, mean = 0, sd = 1),
    V2 = rnorm(n = .n, mean = 0, sd = 1),
    V3 = rnorm(n = .n, mean = 0, sd = 1),
    V4 = rnorm(n = .n, mean = 0, sd = 1)
  )
  cl2  <- data.frame(
    V1 = rnorm(n = .n, mean = 0, sd = 1),
    V2 = rnorm(n = .n, mean = 4, sd = 1),
    V3 = rnorm(n = .n, mean = 0, sd = 3),
    V4 = rnorm(n = .n, mean = 0, sd = 1)
  )
  dat  <- rbind(cl1, cl2)
  clas <- rep(c("A", "B"), each = .n)
  bas1 <- basis_olda(dat, clas, d = 4)[, c(4, 3)]
  
  bas2 <- tourr::basis_random(4, 2)
  bas2[2,] <- bas2[2, ] / 20
  bas2 <- tourr::orthonormalise(bas2)
  # fig_cl_sep -----
  (ClSep1 <- ggtour(bas1, dat) +
     proto_basis() +
     proto_point(aes_args = list(color = clas, shape = clas), list(size=1)) +
     labs(color = "Cluster", shape = "Cluster", title= "a") +
     theme(panel.border = element_rect(fill = NA)))
  (ClSep2 <- ggtour(bas2, dat) +
      proto_basis() +
      proto_point(aes_args = list(color = clas, shape = clas), list(size=1)) +
      labs(color = "Cluster", shape = "Cluster", title= "b") +
      theme(legend.position = "none",
            panel.border = element_rect(fill = NA)))
  (pw <- ClSep1 + ClSep2)
}
if(F){
  ggsave("./paper/figures/figClSep.pdf", pw,
         device = "pdf", width = 3.48*2+0.17, height = 3, units = "in")
}

## fig_radial_tour -----
bas <- basis_olda(dat, clas, d = 4)[, c(4, 3)]
# y <- as.matrix(dat) %*% bas %>% as.data.frame()
# GGally::ggpairs(y, ggplot2::aes(color = clas))
mt_path <- manual_tour(bas, 2, data = dat)

ggt <- ggtour(mt_path, dat, .3) +
  proto_point(list(color = clas, shape = clas)) +
  proto_basis(line_size = .6) + theme(legend.position = "off")
## Use this to pick frame 2 (full), 8 (half), 10 (0)
(fs <- filmstrip(ggt, ncol = 3))

interp <- spinifex:::interpolate_manual_tour(mt_path, angle = .3)
bas_full <- interp[,,2]
bas_half <- interp[,,5]
bas_zero <- interp[,,8]
attr(bas_full, "manip_var") <-
  attr(bas_half, "manip_var") <-
  attr(bas_zero, "manip_var") <- attributes(mt_path)$manip_var

full <- ggtour(bas_full, dat, .3) +
  proto_point(list(color = clas, shape = clas), list(size=1)) +
  proto_basis(line_size = .6) +
  proto_origin() +
  ggtitle("a") +
  theme(legend.position = "off",
        panel.border = element_rect(fill = NA))
half <- ggtour(bas_half, dat, .3) +
  proto_point(list(color = clas, shape = clas), list(size=1)) +
  proto_basis(line_size = .6) + 
  proto_origin() +
  ggtitle("b") +
  theme(panel.border = element_rect(fill = NA),
        legend.title = element_text()) +
  labs(color = "Cluster", size="Cluster", shape="Cluster")
zero <- ggtour(bas_zero, dat, .3) +
  proto_point(list(color = clas, shape = clas), list(size=1)) +
  proto_basis(line_size = .6) +
  proto_origin() +
  ggtitle("c") +
  theme(legend.position = "off",
        panel.border = element_rect(fill = NA))
(pw <- full + half + zero)
if(F){
  ggsave("./paper/figures/figRadialTour.pdf", pw, device = "pdf",
         width = 3.48*2+0.17, height = 2.2, units = "in")
}


## Make figBiplotScoring.pdf -----

## For figBiplotScoring
tgt_sim_nm <- "EEV_p6_33_66_rep2"
tgt_fp <- paste0("./apps_supplementary/data/", tgt_sim_nm, ".rda") 
load(tgt_fp, envir = globalenv())
dat  <- get(tgt_sim_nm)
bas  <- basis_pca(dat, d = 4)[, c(1, 4)]
clas <- attr(dat, "cluster")
source("./paper/R/9_util_funcs.r") ## previously ggproto_pca_biplot.r
if(F)
  file.edit("./paper/R/9_util_funcs.r")

### Left pane ----
proj <- as.matrix(dat) %*% bas %>% as.data.frame()
(gg1 <- ggplot(proj, aes(PC1, PC4)) +
    geom_point(aes(shape = clas, color = clas), size=1) +
    draw_basis(bas, proj) + coord_fixed() +
    theme_void() +
    theme(axis.title = element_text(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "off",
          axis.title.y = element_text(angle = 90)) +
    scale_color_brewer(palette = "Dark2") +
    scale_fill_brewer( palette = "Dark2") +
    labs(subtitle = "Visual: PCA, location: 33/66%, \n Shape: EEV, dimension: 6 & 4 clusters",
         x = "PC1", y = "PC4", color = "color", shape = "shape"))

### right pane, w and acc bars ----
ans_tbl    <- readRDS("./apps/spinifex_study/www/ans_tbl.rds")
sub        <- ans_tbl %>% dplyr::filter(sim_nm == tgt_sim_nm)
sub_longer <- pivot_longer_resp_ans_tbl(dat = sub)

gg2 <- ggplot() + theme_bw() +
  ggproto_ans_plot(sub_longer) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(hjust = .5)) +
  labs(y = "Bars: observed cluster separation\nLines: accuracy weights if selected", x = "Variable") +
  theme(legend.position = "off")

(final <- cowplot::plot_grid(gg1, gg2 + theme(aspect.ratio = 8/6), rel_widths = c(1.8, 1)))
if(F){
  ggsave("./paper/figures/figBiplotScoring.pdf", final,
         device = "pdf", width = 3.48*2+0.17, height = 3, units = "in")
}

