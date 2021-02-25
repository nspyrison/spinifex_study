tgt_sim_nm <- "EEV_p6_33_66_rep2"
tgt_fp <- paste0("./apps_supplementary/data/", tgt_sim_nm, ".rda") 
## Make data plot
load(tgt_fp, envir = globalenv())
dat <- EEV_p6_33_66_rep2
clas <- attr(dat, "cluster")
source("./paper/R/ggproto_pca_biplot.r")

gg1 <- ggplot() + theme_void() +
  ggproto_pca_biplot(dat, aes_clas = clas, x_pc_num = 1L, y_pc_num = 4L) +
  theme(axis.title = element_text(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "PCA biplot", 
       subtitle = "EEV_p6_33_66_rep2",
       x = "PC1", y = "PC4")

## Make ans_plot
ans_tbl <- readRDS("./apps/spinifex_study/www/ans_tbl.rds")
sub <- ans_tbl %>% dplyr::filter(sim_nm == tgt_sim_nm)
source("./paper/R/pivot_longer_resp_ans_tbl.r")
sub_longer <- pivot_longer_resp_ans_tbl(dat = sub)

source("./paper/R/ggproto_ans_plot.r")
gg2 <- ggplot() + theme_minimal() +
  ggproto_ans_plot(sub_longer) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),) +
  labs(title = "Variable cluster seperation & weights",
       subtitle = "EEV_p6_33_66_rep2")

cowplot::plot_grid(gg1, gg2, scale = c(1, .66))
ggsave("./paper/figures/figBiplotScoring.png", width = 8, height = 8/2.2, units = "in")
