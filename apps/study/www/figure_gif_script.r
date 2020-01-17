library("tourr")
library("ggplot2")
library("spinifex")
theme_set(theme_minimal())

dat <- tourr::rescale(flea[, 1:6])
col <- spinifex::col_of(flea$species)
pch <- spinifex::pch_of(flea$species) - 6

##### PCA custom SPLOM pairs

f_pca <- prcomp(dat)

f_pc_x <- as.data.frame(f_pca$x[, 1:3])
f_pca_pct_var <- round(100 * f_pca$sdev^2 / sum(f_pca$sdev^2), 1)
GGally::ggpairs(f_pc_x)



gbase <- ggplot() +
  theme(aspect.ratio = 1) + theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  theme(panel.grid.major = element_blank(), # no grid lines
        panel.grid.minor = element_blank(), # no grid lines
        axis.text.x = element_blank(),      # no axis marks
        axis.text.y = element_blank(),      # no axis marks
        axis.title.x = element_text(size = 18, face = "bold"),
        axis.title.y = element_text(size = 18, face = "bold")
  )

# g1: PC1&2
x_axis <- "PC1"; y_axis <- "PC2" 
x_lab <-paste0(x_axis, " (", f_pca_pct_var[as.integer(substr(x_axis,3,3))], "% Var)")
y_lab <-paste0(y_axis, " (", f_pca_pct_var[as.integer(substr(y_axis,3,3))], "% Var)")
(g1 <- gbase + geom_point(data = f_pc_x, aes(x = PC1, y = PC2), color = col, shape = pch) +
  labs(x = x_lab, y = y_lab))

# blank
g2 <- ggplot()

# g3: PC1&3
x_axis <- "PC1"; y_axis <- "PC3" 
x_lab <-paste0(x_axis, " (", f_pca_pct_var[as.integer(substr(x_axis,3,3))], "% Var)")
y_lab <-paste0(y_axis, " (", f_pca_pct_var[as.integer(substr(y_axis,3,3))], "% Var)")
(g3 <- gbase + geom_point(data = f_pc_x, aes(x = PC1, y = PC3), color = col, shape = pch) +
    labs(x = x_lab, y = y_lab))

# g4: PC2&3
x_axis <- "PC2"; y_axis <- "PC3" 
x_lab <-paste0(x_axis, " (", f_pca_pct_var[as.integer(substr(x_axis,3,3))], "% Var)")
y_lab <-paste0(y_axis, " (", f_pca_pct_var[as.integer(substr(y_axis,3,3))], "% Var)")
(g4 <- gbase + geom_point(data = f_pc_x, aes(x = PC2, y = PC3), color = col, shape = pch) +
    labs(x = x_lab, y = y_lab))

(gg_pca <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol = 2))
ggsave("./apps/study/www/training_pca.png", plot = gg_pca)




##### MANUAL Tour training .gif

?spinifex::play_manual_tour
f_pc_bas <- f_pca$rotation[, 1:2]
view_basis(f_pc_bas)

animation::ani.options(ani.width = 200, ani.height = 200, ani.res = 800)

play_manual_tour(basis = f_pc_bas, data = dat, manip_var = 5, 
                 render_type = render_gganimate, col = col, pch = pch, fps = 5)


gganimate::anim_save("./apps/study/www/training_manual.gif") # can't find a good way to change resolution 

##### GRAND Tour training .gif

rb <- tourr::basis_random(n = ncol(dat))
tpath <- save_history(dat, tour_path = grand_tour(), max = 7)

play_tour_path(tour_path = tpath, data = dat, angle = .08, fps = 5,
               render_type = render_gganimate, col = col, pch = pch, 
               axes = "center")


gganimate::anim_save("./apps/study/www/training_grand.gif") # can't find a good way to change resolution 



