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


## fig_pca_splom.pdf ----

### PCA
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

