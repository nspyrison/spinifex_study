#### MAIN DEV MOVED OVER TO 
if(F)
  browseURL("https://github.com/Sayani07/q2gs")


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


z <- ggplot2::ggproto()
str(z)

#### Example 1, stat_chull ----

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
                        browser()
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

#### You turn, geom_proj -----
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
StatProj <- ggproto(
  "StatProj", Stat, 
  compute_group = function(data, scales) {
    create_proj(data, basis)
  },
  ## requires `data` and `basis`, but not aes() mappings
  ## this is where the issues will be.
  required_aes = c("x", "y")
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


#### You turn, geom_quantile_ribbon -----

#### name: geom_quantile_ribbon 
## symetric & asymetric coloring (handles even number of q)
## if odd number; middle line. if even, none
## eventually want to do internal aggregation 

#### name: geom_quantile_ci (nice to have)
## accepts alpha, uses MKmisc::quantileCI, or similar to calc the CI
## -http://finzi.psych.upenn.edu/R/library/MKmisc/html/quantileCI.html
## symetric & asymetric coloring (handles even number of q)
## Quantiles as lines, color matching to the CI geom_errorbars

#### Mitch did one for geom_hdr, but that is a complex example.
## Alternatively, he did a simpler one a while ago, ggquiver
## https://cran.r-project.org/web//packages/ggquiver/index.html

#### Example data
toy <- tsibbledata::vic_elec
#skimr::skim(toy) ## plotting demand across time.




## geom ribbon example
library(tidyverse)
huron <- data.frame(year = 1875:1972,
                    value = LakeHuron,
                    std = runif(length(LakeHuron),0,1))

huron %>% 
  ggplot(aes(year, value)) + 
  geom_ribbon(aes(ymin = value - std,
                  ymax = value + std), ## shadowing cnf intervals
              fill = "steelblue2") + 
  geom_line(color = "firebrick",
            size = 1)                  ## point estimate

z <- data.frame(
  x = rep(letters[1:3], each = 5),
  x_num = rep(1:3, each = 5),
  y = c(1:5, 
        seq(10, 25, length.out = 5),
        seq(-10, -2, length.out = 5))
)
data <- z; probs <- seq(.3, .7, .1); type = 1

#### _1) data transform
## Computes quantiles for y, for each level of x, facet, and group
create_quantile_ribbon <- function(data, prob, type = 1){
  if(length(probs) %% 2 == 0)
    rlang::abort("Expects an odd number of `probs`")
  
  u_x <- unique(data$x)
  ## Quantiles as a list within each unique x (applied to group and facet later)
  lapply(seq_len(length(u_x)), function(i){
    sub_y <- data$y[which(data$x %in% u_x[i])]
    quantile(sub_y, probs, na.rm = TRUE, type = type)
  })
}


#### _2) StatProj
StatQuan <- ggproto(
  "StatMyquantile", Stat, 
  ## stat operates on multiple rows; use compute_group() over compute_panel()
  setup_data = function(data, params){
    if(anyDuplicated(data$group)){
      data$group <- paste(data$group, seq_len(nrow(data)), sep = "-")
    }
    data
  },
  compute_group = function(data, scales,
                           probs, type = 1){
    cols_to_keep <- setdiff(names(data), c("x", "y"))
    l_quantiles <- lapply(seq_len(nrow(data)), function(i){
      single_quantile_group <- create_myquantile(data, probs, type)
      cbind(single_quantile_group, unclass(data[i, cols_to_keep]))
    })
    do.call(rbind, l_quantiles)
  },
  required_aes = c("x", "y")
)


#### _3) stat_proj
stat_myquantile <- function(mapping = NULL, data = NULL, geom = "ribbon",
                            position = "identity", ...,
                            probs = NULL, type = 1,
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE){
  browser()
  layer(data = data, 
        mapping = mapping, 
        stat = StatMyquantile,
        geom = geom, 
        position = position, 
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(probs = probs,
                      type = tpye,
                      na.rm = na.rm,
                      ...
        )
  )
}

#### _4) geom_proj

geom_quantile_ribbon <- function(mapping = NULL,
                      data = NULL,
                      stat = "myquantile",
                      position = "identity",
                      ...,
                      basis = NULL,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE){
  layer(data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRibbon,
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
z <- data.frame(
  x_fct = as.factor(rep(letters[1:3], each = 5)),
  x_num = rep(1:3, each = 5),
  measure = c(1:5,
        seq(10, 25, length.out = 5),
        seq(-10, -2, length.out = 5))
)

ggplot(z) + 
  geom_quantile_ribbon(aes(x_fct, measure))

## __B) With group


