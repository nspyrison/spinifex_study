#' Creates a data frame of the variance explained by the Principal Components.
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' 
#' df_scree_clSep(dat, clas, 1, 2)
# data=dat;class=clas;num_class_lvl_a=1;num_class_lvl_b=2;do_scale=T;
df_scree_clSep <- function(data, 
                           class,
                           num_class_lvl_a = 1,
                           num_class_lvl_b = 2,
                           do_scale = TRUE) {
  data <- as.data.frame(data)
  p <- ncol(data)
  .tgt_lvls <- levels(as.factor(class))[c(num_class_lvl_a, num_class_lvl_b)]
  
  ## Find Cluster means
  ls_clMns_ab <- list()
  ls_clCov_ab <- list()
  ls_n_ab <- list()
  for (i in 1:length(.tgt_lvls)) {
    .lvl_nm <- class == .tgt_lvls[i]
    .lvl_df <- data[.lvl_nm, ]
    ls_clMns_ab[[i]] <- colMeans(.lvl_df)
    ls_clCov_ab[[i]] <- cov(.lvl_df)
    ls_n_ab[[i]]     <- nrow(.lvl_df)
  }
  
  ##### Close to Fisher's linear discriminant
  #### Like LDA, but doesn't assume equal covariances within group
  ## p-dim vector, different of cluster means
  .numerator_vect <- matrix((ls_clMns_ab[[2]] - ls_clMns_ab[[1]]), ncol = p) 
  .tgt_lvls_n <- ls_n_ab[[1]] + ls_n_ab[[2]]
  ## Pooled covariances of the groups. Note that FDA sums the within cluster cov rather than pooling it.
  .denominator_mat <- 
    (ls_clCov_ab[[1]] * ls_n_ab[[1]] + ls_clCov_ab[[2]] * ls_n_ab[[2]]) / .tgt_lvls_n
  ## The cluster seperation of a and b
  #### When accounting for: difference in the cluster means and pooled within cluster covariances.
  clSep <- .numerator_vect %*% solve(.denominator_mat)
  
  ## Looking at magnidue seperation alone:
  a_clSep <- abs(clSep)
  .ord <- order(a_clSep, decreasing = T)
  clSep_rate <- t(a_clSep[.ord])
  if (do_scale == TRUE) clSep_rate <- clSep_rate / sum(clSep_rate)
  colnames(clSep_rate) <- colnames(clSep)[.ord]
  vars_fct <- factor(x = colnames(clSep_rate),
                     levels = unique(colnames(clSep_rate)))
  
  ## Return data frame of scree table for cluster seperation
  data.frame(data_colnum = (1:p)[.ord],
             var = vars_fct,
             var_clSep = as.vector(clSep_rate),
             cumsum_clSep = cumsum(clSep_rate)
  )
}

#' Creates a screeplot of the cluster seperation between 2 selected levels.
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' palette(RColorBrewer::brewer.pal(3, "Dark2"))
#' ggplot2::ggplot() + ggproto_screeplot_clSep(dat, clas)
#' 
#' ggplot2::ggplot() +
#'   ggproto_screeplot_clSep(data = dat, class = clas,
#'                           num_class_lvl_a = 2, num_class_lvl_b = 3) +
#'   ggplot2::theme_bw()

ggproto_screeplot_clSep <- function(data,
                                    class,
                                    num_class_lvl_a = 1,
                                    num_class_lvl_b = 2) {
  .df_scree_clSep <- df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b)
  axis_labs <- c("Variable", "Cluster seperation")
  lgnd_labs <- c("Variable cluster seperation",
                 "Cummulative cluster seperation")
  
  ## List of ggproto's that is addable to a ggplot object.
  list(
    ## Individual feature bars
    ggplot2::geom_bar(ggplot2::aes(x = var, y = var_clSep, fill = lgnd_labs[1]),
                      .df_scree_clSep, stat = "identity"),
    ## Cummulative feature line
    ggplot2::geom_line(ggplot2::aes(x = var, y = cumsum_clSep,
                                    color = lgnd_labs[2], group = 1),
                       .df_scree_clSep, lwd = 1.2),
    ggplot2::geom_point(ggplot2::aes(x = var, y = cumsum_clSep, color = lgnd_labs[2]),
                        .df_scree_clSep, shape = 18, size = 4),
    ## Titles and colors
    ggplot2::labs(x = axis_labs[1], y = axis_labs[2], 
                  colour = "", fill = ""),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30),
                   legend.position = "bottom"),
    ggplot2::scale_fill_manual(values = palette()[1]),
    ggplot2::scale_colour_manual(values = palette()[2])
  )
}



#' Creates a screeplot of the cluster seperation AND  between 2 selected levels.
#' 
#' @examples 
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' palette(RColorBrewer::brewer.pal(3, "Dark2"))
#' ggplot2::ggplot() + ggproto_screeplot_clSep(dat, clas)
#' 
#' ggplot2::ggplot() +
#'   ggproto_screeplot_clSep(data = dat, class = clas,
#'                           num_class_lvl_a = 2, num_class_lvl_b = 3) +
#'   ggplot2::theme_bw()

ggproto_screeplot_clSep <- function(data,
                                    class,
                                    num_class_lvl_a = 1,
                                    num_class_lvl_b = 2) {
  .df_scree_clSep <- df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b)
  axis_labs <- c("Variable", "Cluster seperation")
  lgnd_labs <- c("Variable cluster seperation",
                 "Cummulative cluster seperation")
  
  ## List of ggproto's that is addable to a ggplot object.
  list(
    ## Individual feature bars
    ggplot2::geom_bar(ggplot2::aes(x = var, y = var_clSep, fill = lgnd_labs[1]),
                      .df_scree_clSep, stat = "identity"),
    ## Cummulative feature line
    ggplot2::geom_line(ggplot2::aes(x = var, y = cumsum_clSep,
                                    color = lgnd_labs[2], group = 1),
                       .df_scree_clSep, lwd = 1.2),
    ggplot2::geom_point(ggplot2::aes(x = var, y = cumsum_clSep, color = lgnd_labs[2]),
                        .df_scree_clSep, shape = 18, size = 4),
    ## Titles and colors
    ggplot2::labs(x = axis_labs[1], y = axis_labs[2], colour = "", fill = ""),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30),
                   legend.position = "bottom"),
    ggplot2::scale_fill_manual(values = palette()[1]),
    ggplot2::scale_colour_manual(values = palette()[2])
  )
}



