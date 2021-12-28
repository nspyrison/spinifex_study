## Setup -----
{
  require("ggplot2")
  require("spinifex")
  require("magrittr")
  
  tgt_sim_nm <- "EEV_p6_33_66_rep2"
  tgt_fp <- paste0("./apps_supplementary/data/", tgt_sim_nm, ".rda") 
  ## Make data plot
  load(tgt_fp, envir = globalenv())
  dat <- EEV_p6_33_66_rep2
  bas <- basis_pca(dat, d = 4)[, c(1, 4)]
  clas <- attr(dat, "cluster")
  
  source("./paper/R/9_util_funcs.r") ## previously ggproto_pca_biplot.r
  if(F)
    file.edit("./paper/R/9_util_funcs.r")
}


## Make figBiplotScoring.pdf -----

### Left pane ----
## More issues with geom_existence issue, even after change to theme_spinifex;
#### going back to ggplot2 approach
# ggtour(bas, dat) +
# proto_point(aes_args = list(shape = clas, color = clas)) +
# proto_basis() + ## removes cluster d when used....
# proto_origin() +
# coord_fixed(xlim = c(-8, 3), ylim = c(-1.5, 6)))
# ylim(-.5, 7))
# expand_limits(x = c(-7, 3), y = c(-1.5, 7)))
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

## Make ans_plot
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

