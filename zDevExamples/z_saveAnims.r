
library("spinifex")
library("gganimate")

flea_std <- tourr::rescale(tourr::flea[, 1:6])
rb <- tourr::basis_random(n = ncol(flea_std))
flea_class <- tourr::flea$species


anim <- play_manual_tour(basis = rb, data = flea_std, manip_var = 6,
                 theta = .5 * pi, axes = "right", fps = 5,
                 render_type = render_gganimate)

?gganimate::animate

# Change duration and framerate
animate(anim, fps = 20, duration = 15)


## In render_gganimate, NEED AN `animate_args` ARG FOR THE GGANIMATE::ANIMATE() , 
animate(warming_points, fps = 10, width = 750, height = 450)