### Example of extending ggplot2
## Following along with
if(F){
  browseURL("https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html")
  browseURL("https://ggplot2-book.org/programming.html")
  browseURL("https://ggplot2-book.org/spring1.html")
}

require("ggplot2")

## 1) make a CammelCase stat (ggproto object)
## 2) make a geom wrapper
## ==---==
## remember: only the mapping aes() uses NSE, will more likely want to use
## aes_(substitute(arg)).
## may also need some sorts of %*% opperators.


z<-ggplot2::ggproto()
str(z)

#### Example 1, stat_cdwhull ----

### _1) StatChull -----
## Bare bones, 
## Importantly 1) the compute group, computing the stat,
## and 2) the required_aes, so the geom knows the requirements.
if(F)
  browseURL("https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html")

StatChull <- ggplot2::ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     
                     required_aes = c("x", "y")
)


### _2) stat_chull -----
## Make the new, snake_case, stat_chull, a function wrapper for layer.
## for notes on the layer see:
if(F)
  browseURL("https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html")


stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

### _3) Test drive -----
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  stat_chull(fill = NA, colour = "black")


ggplot(mpg, aes(displ, hwy, colour = drv)) + 
  geom_point() + 
  stat_chull(fill = NA)

#### Example 2, geom_spring -----
if(F)
  browseURL("https://ggplot2-book.org/spring1.html")

### _0) data setup & idea for stat -----

circle <- tibble(
  x = sin(seq(0, 2 * pi, length.out = 100)),
  y = cos(seq(0, 2 * pi, length.out = 100)),
  index = 1:100,
  type = "circle"
)
spring <- circle
spring$x <- spring$x + seq(0, 1.5, length.out = 100)
spring$type <- "spring"
ggplot(rbind(circle, spring)) + 
  geom_path(
    aes(x = x, y = y, group = type, alpha = index), 
    show.legend = FALSE
  ) + 
  facet_wrap(~ type, scales = "free_x")

#### _1) data transform -----
## this is roughly equivalent to the chull() function
## ?chull

create_spring <- function(x, y, xend, yend, diameter = 1, tension = 0.75, n = 50) {
  if (tension <= 0) {
    rlang::abort("`tension` must be larger than zero.")
  }
  if (diameter == 0) {
    rlang::abort("`diameter` can not be zero.")
  }
  if (n == 0) {
    rlang::abort("`n` must be greater than zero.")
  }
  # Calculate direct length of segment
  length <- sqrt((x - xend)^2 + (y - yend)^2)
  
  # Figure out how many revolutions and points we need
  n_revolutions <- length / (diameter * tension)
  n_points <- n * n_revolutions
  
  # Calculate sequence of radians and x and y offset
  radians <- seq(0, n_revolutions * 2 * pi, length.out = n_points)
  x <- seq(x, xend, length.out = n_points)
  y <- seq(y, yend, length.out = n_points)
  
  # Create the new data
  data.frame(
    x = cos(radians) * diameter/2 + x,
    y = sin(radians) * diameter/2 + y
  )
}


#### _1) Stat_spring -----

## Define %||% to be a coalesce like function; use x, if x is NA, then use y
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

StatSpring <- ggproto("StatSpring", Stat, 
                      setup_data = function(data, params) {
                        if (anyDuplicated(data$group)) {
                          data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
                        }
                        data
                      },
                      compute_panel = function(data, scales, 
                                               diameter = 1, 
                                               tension = 0.75, 
                                               n = 50) {
                        cols_to_keep <- setdiff(names(data), c("x", "y", "xend", "yend"))
                        springs <- lapply(seq_len(nrow(data)), function(i) {
                          spring_path <- create_spring(
                            data$x[i], data$y[i], 
                            data$xend[i], data$yend[i], 
                            diameter = diameter, 
                            tension = tension, 
                            n = n
                          )
                          cbind(spring_path, unclass(data[i, cols_to_keep]))
                        })
                        do.call(rbind, springs)
                      },
                      required_aes = c("x", "y", "xend", "yend")
)