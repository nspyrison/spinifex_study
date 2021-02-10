#' @example 
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea[, 7]
#' ggplot() + theme_void() +
#'   ggproto_pca_biplot(dat, aes_clas = clas)

ggproto_pca_biplot <- function(
  dat,
  x_pc_num = 1L,
  y_pc_num = 2L,
  aes_clas,
  brewer_pal = RColorBrewer::brewer.pal(8L, "Dark2")[c(1L, 2L, 3L, 6L, 8L)]
){
  axes_position <- "left"
  pca_obj <- prcomp(as.matrix(dat))
  proj <- as.data.frame(pca_obj$x[, c(x_pc_num, y_pc_num)])
  bas <- data.frame(pca_obj$rotation[, c(x_pc_num, y_pc_num)])
  bas <- scale_axes(bas, axes_position, proj)
  colnames(proj) <- c("x", "y")
  proj$aes_clas  <- aes_clas
  colnames(bas)  <- c("x", "y")
  
  angle <- seq(0L, 2L * pi, length = 360L)
  circ  <- spinifex::scale_axes(data.frame(x = cos(angle), y = sin(angle)),
                                axes_position, proj)
  zero  <- spinifex::scale_axes(data.frame(x = 0L, y = 0L),
                                axes_position, proj)
  
  asp_r <- 
    diff(range(c(proj[, 2L], circ[, 2L]))) /
    diff(range(c(proj[, 1L], circ[, 1L])))
  point_aes <- ggplot2::aes(x, y) ## Initialize
  if(missing(aes_clas) == FALSE){
    point_aes <- ggplot2::aes(
      x = x, y = y,
      color = aes_clas,
      fill  = aes_clas,
      shape = aes_clas)
  }
  
  
  ### ggproto
  list(
    ggplot2::scale_colour_manual(values = brewer_pal),
    ggplot2::scale_fill_manual(values = brewer_pal),
    ## Themes and aesthetics
    ggplot2::theme(legend.position = "none", ## no legend
                   aspect.ratio = asp_r),
    ## Data points
    ggplot2::geom_point(mapping = point_aes,
                        proj,
                        size = 1.5),
    ## Axis segments
    ggplot2::geom_segment(ggplot2::aes(x = x, xend = zero[, 1L],
                                       y = y, yend = zero[, 2L]),
                          bas,
                          size = 1L, colour = "grey50"),
    ## Axis label text
    ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = colnames(dat)),
                       bas,
                       size = 5L, colour = "grey50", fontface = "bold",
                       vjust = "outward", hjust = "outward"),
    ## Circle path
    ggplot2::geom_path(ggplot2::aes(x = x, y = y),
                       circ,
                       color = "grey80", size = 1L, inherit.aes = FALSE)
  )
}
