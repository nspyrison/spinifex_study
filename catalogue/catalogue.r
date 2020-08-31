require("tibble")
require("dplyr")
require("ggplot2")
require("ggforce")
source("./R/sim_pDim_kCl.r") ## Cluster levels stored in attribute "cl_lvl". Try attr(mySim, "cl_lvl")
set.seed(20200813)
args(sim_pDim_kCl)

p2 <- 4
params <- 
  tibble(id     = 1:p2,
         dim    = rep(2, p2),
         x = rep(c(0, 10), each = 2),
         y = rep(0, each = p2),
         a = c(3, 5, 3, 5),
         b = c(3, 3, 3, 5),
         ang = c()
         distribution = rep(c("Spherical", "Diagonal"), each = 2),  ## %in% c("Spherical", "Diagonal", "Ellipsoidal")
         Volume       = rep(c("Equal", "Variable"), times = 2),     ## %in% c("Equal", "Variable")
         Shape        = rep(c("Equal", "Variable"), each = 2),      ## %in% c("Equal", "Variable")
         Orientation  = rep(c("---", "Coordinate axes"), each = 2), ## %in% c("---", "Coordinate axes", "Equal", "Variable")
         name         = c("EE-", "VE-", "EVC", "VVC")
  )

ggplot(data = z) + 
  geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b, angle = ang)) +
  coord_fixed()

ggplot() + 
  geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = 0)) +
  coord_fixed()

l <- 4
r <- 2 * l
nms <- c("EE-", "VE-", "EVC", "VVC")[1:l]
z <- data.frame(facet = rep(nms, each = 2),
                cl = paste0("Cl ", rep(letters[1:2], length.out = r)),
                x  = rep(c(0, 16), times = l),
                y  = rep(0, length.out = r),
                a  = c(4, 4,  4, 2,  4, 16/3,  4, 4), 
                b  = c(4, 4,  4, 2,  4, 3,     4, 2), 
                ang = rep(0, length.out = r)
)
ggplot(data = z) + 
  geom_ellipse(aes(x0 = x, y0 = y, a = a, b = b, angle = ang, color = cl),
               size = 2) + 
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(vars(facet)) +
  coord_fixed()

