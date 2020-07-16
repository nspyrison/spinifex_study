#' Produce a screeplot of the variance explained by the Principal Component.
#' 
#' @examples 
#' dat  <- tourr::flea[, 1:6]
#' palette(RColorBrewer::brewer.pal(3, "Dark2")) 
#' ggplot2::ggplot() + ggproto_screeplot_pca(dat)
#' 
#' ggplot2::ggplot() +
#'   ggproto_screeplot_pca(data = dat, rescale = FALSE) +
#'   ggplot2::theme_bw()

ggproto_screeplot_pca <- function(data = NULL, 
                                  rescale = TRUE){
  if (rescale == TRUE) data <- tourr::rescale(as.matrix(data))
  data <- as.data.frame(data)
  p <- ncol(data)
  
  ## PCA VARIANCE EXPLAINED
  pca_obj <- prcomp(data)
  df_scree_pcaVar <- data.frame(
    pc_num = paste0("PC", 1:p),
    PC_var = pca_obj$sdev^2 / sum(pca_obj$sdev^2),
    cumsum_var = cumsum(pca_obj$sdev^2) / sum(pca_obj$sdev^2)
  )
  
  axis_labs <- c("Principal component", "Variance explained")
  lgnd_labs <- c("PC variance explained", "Cummulative variance explained")
  ## List of ggproto's that is addable to a ggplot object.
  list(
    ## Individual feature bars
    ggplot2::geom_bar(data = df_scree_pcaVar, stat = "identity", 
                      mapping = ggplot2::aes(x = pc_num, y = PC_var, 
                                             fill = lgnd_labs[1])),
    ## Cummulative feature line
    ggplot2::geom_line(data = df_scree_pcaVar, lwd = 1.2,
                       mapping = ggplot2::aes(x = pc_num, y = cumsum_var,
                                              color = lgnd_labs[2], group = 1)),
    ggplot2::geom_point(data = df_scree_pcaVar, shape = 18, size = 4,
                        mapping = ggplot2::aes(
                          x = pc_num, y = cumsum_var,
                          color = lgnd_labs[2])),
    ## Titles and colors
    ggplot2::labs(x = axis_labs[1], y = axis_labs[2], 
                  colour = "", fill = ""),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30)),
    ggplot2::scale_fill_manual(values = palette()[1]),
    ggplot2::scale_colour_manual(values = palette()[2])
  )
}

