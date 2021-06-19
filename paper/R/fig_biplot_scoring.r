require("ggplot2")
require("spinifex")

tgt_sim_nm <- "EEV_p6_33_66_rep2"
tgt_fp <- paste0("./apps_supplementary/data/", tgt_sim_nm, ".rda") 
## Make data plot
load(tgt_fp, envir = globalenv())
dat <- EEV_p6_33_66_rep2
clas <- attr(dat, "cluster")
source("./paper/R/ggproto_pca_biplot.r")
if(F)
  file.edit("./paper/R/ggproto_pca_biplot.r")


gg1 <- ggplot() + theme_void() +
  ggproto_pca_biplot(dat, aes_clas = clas, x_pc_num = 1L, y_pc_num = 4L) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "factor=pca, location:33/66%, \n shape=EEV, dim=6",
       x = "PC1", y = "PC4")

## Make ans_plot
ans_tbl <- readRDS("./apps/spinifex_study/www/ans_tbl.rds")
sub <- ans_tbl %>% dplyr::filter(sim_nm == tgt_sim_nm)
source("./paper/R/9_util_clean_participant_data.r")
if(F)
  file.edit("./paper/R/9_util_clean_participant_data.r")
sub_longer <- pivot_longer_resp_ans_tbl(dat = sub)

source("./paper/R/ggproto_ans_plot.r")
if(F)
  file.edit("./paper/R/ggproto_ans_plot.r")
gg2 <- ggplot() + theme_bw() +
  ggproto_ans_plot(sub_longer) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Cluster separation & weights") + 
  theme(legend.position = "off")

(final <- cowplot::plot_grid(gg1, gg2 , scale = c(1, .7)))
.w = 6.25
.h = 9
.u = "in"
if(F)
  ggsave("./paper/figures/figBiplotScoring.pdf", final,
         device = "pdf", width = .w, height = .w / 2, units = .u)

