### init from global.r
require("spinifex");require("tidyverse");
pal <- RColorBrewer::brewer.pal(8, "Dark2")[c(1, 2, 3, 6, 8)]
bas_p4 <- matrix(c(.5, .5,
                   -.5, .5,
                   -.5, -.5,
                   .5,  -.5),
                 ncol = 2, nrow = 4, byrow = TRUE)
bas_p6 <- matrix(c(.2887,  .5,
                   -.2887, .5,
                   -.5774, 0,
                   -.2887, -.5,
                   .2887,  -.5,
                   .5774,  0),
                 ncol = 2, nrow = 6, byrow = TRUE)
###

### Setup
library("spinifex")
?play_manual_tour

dat_std <- tourr::rescale(tourr::flea[, 1:6])
cluster <- tourr::flea$species
mv      <- manip_var_pca(dat_std)
###
### Aesthetic options
angle <- .1
fps   <- 6L
max_frames <- 90L ## 90 frame for 15 sec @ fps = 6
axes_position <- "left"
pt_size <- 3L
###

### Saving

## _GIF -- Export gganimate to .gif
play_manual_tour(basis = bas_p6, data = flea_std, manip_var = mv,
                 axes = axes_position, fps = fps, angle = angle,
                 aes_args = list(color = cluster, shape = cluster),
                 identity_args = list(size = pt_size),
                 ggproto = list(theme_spinifex(),
                                theme(legend.position = "none"),
                                scale_colour_manual(values = pal)
                 ),
                 render_type = render_gganimate,
                 gif_filename = "radialTour_example.gif",
                 gif_path = "./apps/study/images"
)
## _GIF -- Export gganimate to .gif
view_frame(basis = bas_p6, data = flea_std, manip_var = mv,
                 axes = axes_position, 
                 aes_args = list(color = cluster, shape = cluster),
                 identity_args = list(size = pt_size),
                 ggproto = list(theme_spinifex(),
                                theme(legend.position = "none"),
                                scale_colour_manual(values = pal)
                 )
)
ggsave("./apps/study/images/png_example.png")

