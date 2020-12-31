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

### _1) StatChull
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


### _2) stat_chull
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

## _3) Test drive
## note that while we have added a new layer for chull, we didn't need to define
## a new geom. stat_chull will uses geom_polygon, and geom_point will add the points.
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  stat_chull(fill = NA, colour = "black")


ggplot(mpg, aes(displ, hwy, colour = drv)) + 
  geom_point() + 
  stat_chull(fill = NA)

#### Example 2, geom_spring -----
if(F)
  browseURL("https://ggplot2-book.org/spring1.html")

### _0) brainstorming

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

#### _1) data transform
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


#### _2) StatSpring

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


#### _3) stat_spring

stat_spring <- function(mapping = NULL, data = NULL, geom = "path", 
                        position = "identity", ..., diameter = 1, tension = 0.75, 
                        n = 50, na.rm = FALSE, show.legend = NA, 
                        inherit.aes = TRUE) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = StatSpring, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(
      diameter = diameter, 
      tension = tension, 
      n = n, 
      na.rm = na.rm, 
      ...
    )
  )
}

#### _3) geom_spring

geom_spring <- function(mapping = NULL,
                        data = NULL, 
                        stat = "spring",
                        position = "identity", 
                        ..., 
                        diameter = 1, 
                        tension = 0.75,
                        n = 50, 
                        arrow = NULL, 
                        lineend = "butt", 
                        linejoin = "round",
                        na.rm = FALSE, 
                        show.legend = NA, 
                        inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      diameter = diameter,
      tension = tension,
      n = n,
      arrow = arrow,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}



## _4) Test drive
## __A) Without group
some_data <- tibble(
  x = runif(5, max = 10),
  y = runif(5, max = 10),
  xend = runif(5, max = 10),
  yend = runif(5, max = 10),
  class = sample(letters[1:2], 5, replace = TRUE)
)

ggplot(some_data) + 
  geom_spring(aes(x = x, y = y, xend = xend, yend = yend))

## __B) With group
ggplot(some_data) + 
  geom_spring(
    aes(x, y, xend = xend, yend = yend, colour = class),
    size = 1
  ) + 
  facet_wrap(~ class)

#### You turn, planning -----
## Idea: consider linear projection as a data transformation.
## Following example 2, create_proj, StatProj, stat_proj, and grom_proj


#### _1) data transform
## this is roughly equivalent to the chull() function
create_proj <- function(data, basis){
  if(missing(data) == true)
    rlang::abort("`data` is missing.")
  is_num <- sapply(data, is.numeric)
  if(all(is_num == TRUE) == FALSE){
    rlang::warn("Not all 'data' is numeric, subsetting to only numeric columns.")
    data <- data[is_num]
  }
  p <- ncol(data)
  if(missing(basis) == true){
    rlang::warn("`basis` is missing, assigning basis to PCA basis of `data`.")
    basis <- prcomp(data)$rotation[, 2]
  }
  if(ncol(basis) != 2){
    rlang::abort("`basis` doesn't have 2 columns. Only projections to 2 dimensions supported.")
    basis_pca <- prcomp(data)$rotation
  }
  if(nrow(basis) != p){
    rlang::abort("`basis` doesn't have p columns. Make sure basis has 1 row for each numeric column of`data`.")
    basis_pca <- prcomp(data)$rotation
  }
  
  ## Linear projection of only numeric data
  proj <- as.matrix(data) %*% as.matrix(basis)
  tibble::as_tibble(proj)
}


#### _2) StatProj
StatProj <- ggproto("StatProj", Stat, 
                    compute_group = function(data, scales) {
                      create_proj(data, basis)
                    },
                    
                    required_aes = c("x", "y")
                    ## requires `data` and `basis`, but not aes() mappings
                    )


#### _3) stat_proj
stat_proj <- function(mapping = NULL, data = NULL, geom = "point",
                      position = "identity", ..., basis = NULL, 
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){
  browser()
  layer(data = data, 
        mapping = mapping, 
        stat = StatProj, 
        geom = geom, 
        position = position, 
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(basis = basis,
                      na.rm = na.rm,
                      ...
        )
  )
}

#### _4) geom_proj

geom_proj <- function(mapping = NULL,
                      data = NULL,
                      stat = "proj",
                      position = "identity",
                      ..., 
                      basis = NULL,
                      na.rm = FALSE, 
                      show.legend = NA,
                      inherit.aes = TRUE){
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomPoint,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(basis,
                      na.rm = na.rm,
                      ...
        )
  )
}


## _4) Test drive
## __A) Without group
dat <- tourr::flea[, 1:6]
bas <- spinifex::basis_pca(dat)
clas <- tourr::flea[, 7]

ggplot(dat) + 
  geom_proj(basis = bas, aes(x = tars1, y = tars2))

## __B) With group

