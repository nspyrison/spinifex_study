## Setup -----
{
  require("ggplot2")
  require("spinifex")
  require("magrittr")
  
  ## For figBiplotScoring
  tgt_sim_nm <- "EEV_p6_33_66_rep2"
  tgt_fp <- paste0("./apps_supplementary/data/", tgt_sim_nm, ".rda") 
  load(tgt_fp, envir = globalenv())
  dat  <- get(tgt_sim_nm)
  bas  <- basis_pca(dat, d = 4)[, c(1, 4)]
  clas <- attr(dat, "cluster")
  
  ## For figClSep
  set.seed(2022)
  .n <- 100
  cl1  <- data.frame(
    V1 = rnorm(n = .n, mean = 0, sd = 1),
    V2 = rnorm(n = .n, mean = 0, sd = 1),
    V3 = rnorm(n = .n, mean = 0, sd = 1),
    v4 = rnorm(n = .n, mean = 0, sd = 1)
  )
  cl2  <- data.frame(
    V1 = rnorm(n = .n, mean = 0, sd = 1),
    V2 = rnorm(n = .n, mean = 4, sd = 1),
    V3 = rnorm(n = .n, mean = 0, sd = 3),
    v4 = rnorm(n = .n, mean = 0, sd = 1)
  )
  dat2  <- rbind(cl1, cl2)
  clas2 <- rep(c("A", "B"), each = .n)
  bas2  <- basis_olda(dat2, clas2, d = 4)
  
  source("./paper/R/9_util_funcs.r") ## previously ggproto_pca_biplot.r
  if(F)
    file.edit("./paper/R/9_util_funcs.r")
  
  set.seed(2022)
  rand <- tourr::basis_random(4, 2)
  rand[2,] <- rand[2, ] / 20
  rand <- tourr::orthonormalise(rand)
}

# fig_cl_sep -----
(ClSep1 <- ggtour(bas2[, c(3, 4)], dat2) + 
  proto_basis() + proto_point(aes_args = list(color = clas2, shape = clas2)) +
   labs(color = "Cluster", shape = "Cluster"))
(ClSep2 <- ggtour(rand, dat2) + 
    proto_basis() + proto_point(aes_args = list(color = clas2, shape = clas2)) +
    labs(color = "Cluster", shape = "Cluster"))
(ClSep <- cowplot::plot_grid(ClSep1, ClSep2, nrow = 1, labels = c("a", "b")))
if(F){
  ggsave("./paper/figures/figClSep.pdf", ClSep,
         device = "pdf", width = 6, height = 2.5, units = "in")
}




## Make figBiplotScoring.pdf -----

### Left pane ----
proj <- as.matrix(dat) %*% bas %>% as.data.frame()
(gg1 <- ggplot(proj, aes(PC1, PC4)) +
    geom_point(aes(shape=clas, color=clas)) +
    draw_basis(bas, proj) + coord_fixed() +
    theme_void() +
    theme(axis.title = element_text(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "off",
          axis.title.y = element_text(angle = 90)) +
    labs(subtitle = "Factor: PCA, location: 33/66%, \n Shape: EEV, dimension: 6 & 4 clusters",
         x = "PC1", y = "PC4", color = "color", shape = "shape"))

###  right pane, ClSep and accuracy bars -----
ans_tbl <- readRDS("./apps/spinifex_study/www/ans_tbl.rds")
sub <- ans_tbl %>% dplyr::filter(sim_nm == tgt_sim_nm)
sub_longer <- pivot_longer_resp_ans_tbl(dat = sub)

gg2 <- ggplot() + theme_bw() +
  ggproto_ans_plot(sub_longer) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(y = "Bars: observed cluster separation\nLines: accuracy marks if selected", x = "Variable") + 
  theme(legend.position = "off")

(final <- cowplot::plot_grid(gg1, gg2 + theme(aspect.ratio = 8 / 6), 
                             rel_widths = c(1.2, 1)))
.w = 6.25
.h = 9
.u = "in"
if(F){
  ggsave("./paper/figures/figBiplotScoring.pdf", final,
         device = "pdf", width = .w, height = .w / 2, units = .u)
}

