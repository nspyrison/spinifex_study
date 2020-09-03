#' ggpcapairs(tourr::flea[, 1:6], class = tourr::flea$species)
ggpcapairs <- function(x, class, top_n = ncol(x)){
  require("GGally")
  require("ggplot2")
  pca_x <- prcomp(x)$x[, 1:top_n]
  GGally::ggpairs(as.data.frame(pca_x), ggplot2::aes(color = as.factor(class)))
}


