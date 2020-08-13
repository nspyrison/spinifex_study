#' @examples 
#' dat  <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' ggplot2::ggplot() + geom_lda_points(dat, clas)
#' 
#' palette(RColorBrewer::brewer.pal(12, "Dark2"))
#' ggplot2::ggplot() + geom_lda_points(data = dat, class = clas) + 
#'   ggplot2::theme_minimal()
geom_lda_points <- function(data, class){
  data <- as.data.frame(data)
  lda_obj <- MASS::lda(data, grouping = class)
  lda_obj
  lda_predict <- predict(lda_obj)
  lda_proj <- data.frame(lda_predict$x, class = class)
  
  list(
    ggplot2::geom_point(ggplot2::aes(x = LD1, y = LD2, color = class), 
                        data = lda_proj, size = 2.5),
    ggplot2::scale_color_manual(values = palette()[1:length(unique(class))])
  )
}
