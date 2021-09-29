require("ggplot2")
require("spinifex")
require("magrittr")

tgt_sim_nm <- "EEV_p6_33_66_rep2"
tgt_fp <- paste0("./apps_supplementary/data/", tgt_sim_nm, ".rda") 
## Make data plot
load(tgt_fp, envir = globalenv())
dat <- EEV_p6_33_66_rep2
clas <- attr(dat, "cluster")
source("./paper/R/9_util_funcs.r") ## previously ggproto_pca_biplot.r
if(F)
  file.edit("./paper/R/9_util_funcs.r")


gg1 <- ggplot() + theme_void() +
  ggproto_pca_biplot(dat, aes_clas = clas, x_pc_num = 1L, y_pc_num = 4L) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "factor=PCA, location=33/66%, \n shape=EEV, dimension=6&4 clusters",
       x = "PC1", y = "PC4")

## Make ans_plot
ans_tbl <- readRDS("./apps/spinifex_study/www/ans_tbl.rds")
sub <- ans_tbl %>% dplyr::filter(sim_nm == tgt_sim_nm)
sub_longer <- pivot_longer_resp_ans_tbl(dat = sub)
sub_longer$weight[c(3,5)] <- sub_longer$weight[c(3,5)] / sum(sub_longer$weight[c(3,5)])
sub_longer$weight[c(-3,-5)] <- -sub_longer$weight[c(-3,-5)] / sum(sub_longer$weight[c(-3,-5)])

gg2 <- ggplot() + theme_bw() +
  ggproto_ans_plot(sub_longer) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(subtitle = "Cluster separation & weights") + 
  theme(legend.position = "off")

(final <- cowplot::plot_grid(gg1, gg2 , scale = c(1, 1)))
.w = 6.25
.h = 9
.u = "in"
if(F){
  ggsave("./paper/figures/figBiplotScoring.png", final,
         device = "png", width = .w, height = .w / 2, units = .u)
}

