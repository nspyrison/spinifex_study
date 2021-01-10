require(spinifex)
require(tourr)
## Basis to half clock:

#### PREVIOUS, uniform full clock.
.sqrt_5 <- sqrt(.5) ## .7071
bas_p4 <- matrix(c(0, .sqrt_5,
                   .5, .5,
                   .sqrt_5, 0,
                   .5, -.5),
                 ncol = 2L, nrow = 4L, byrow = TRUE)
.long <- (4 / 3) * .5
bas_p6 <- matrix(c(0, 1,
                   .5, .long,
                   .long, .5,
                   1, 0,
                   .long, -.5,
                   .5, -.long),
                 ncol = 2L, nrow = 6L, byrow = TRUE)

#### NEW, uniform half clock.
## bas_p4
.ang <- seq(0, pi, length.out = 5)[-5] ## p + 1
u_circ_p4 <- as.matrix(data.frame(x = sin(.ang), y = cos(.ang)))
bas_p4 <- tourr::orthonormalise(u_circ_p4)
tourr::is_orthonormal(bas_p4)
spinifex::view_frame(bas_p4)

## bas_p6
.ang <- seq(0, pi, length.out = 7)[-7] ## p + 1
u_circ_p6 <- as.matrix(data.frame(x = sin(.ang), y = cos(.ang)))
bas_p6 <- tourr::orthonormalise(u_circ_p6)
tourr::is_orthonormal(bas_p6)
spinifex::view_frame(bas_p6)
