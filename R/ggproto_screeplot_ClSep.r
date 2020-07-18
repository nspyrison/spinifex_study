#' Creates a data frame of the variance explained by the Principal Components.
#' 
#' @examples 
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' 
#' df_scree_ClSep(dat, clas, 1, 2)

df_scree_ClSep <- function(data, 
                           class,
                           num_class_lvl_A = 1,
                           num_class_lvl_B = 2) {
  data <- as.data.frame(data)
  p <- ncol(data)
  
  ### CLUSTER SEPERATION EXPLAINED
  ## Identify and subset
  tgt_lvls <- levels(as.factor(class))[num_class_lvl_A:num_class_lvl_B]

  ## Find Cluster means
  df_clMns_AB <- NULL
  for (i in 1:length(tgt_lvls)) {
    .lvl_rows <- class ==tgt_lvls[i]
    .lvl_sub <- data[.lvl_rows, ]
    .row_ClMns <- apply(.lvl_sub, 2, mean)
    df_clMns_AB <- rbind(df_clMns_AB, .row_ClMns)
  }
  df_clMns_AB <- as.data.frame(df_clMns_AB)
  rownames(df_clMns_AB) <- paste0("ClMn ", tgt_lvls)
  
  ## p-dim line between ClMns
  df_pLine_AB <- rbind(df_clMns_AB[1, ],
                       df_clMns_AB[2, ] - df_clMns_AB[1, ])
  rownames(df_pLine_AB) <- c(
    paste0("constants: ClMn ", tgt_lvls[1]),
    paste0("coefficients: ClMn ", tgt_lvls[2], "-", tgt_lvls[1]))
  
  ##TODO: SHOULD THE INVERSE COVAR BE ON THE FULL, OR WITHIN CL??
  df_ClSep <- df_pLine_AB[2,] * (1 / cov(data))
  
  a_ClSep <- abs(df_ClSep)
  clSep_rate <- t(a_ClSep[order(a_ClSep, decreasing = T)]) / sum(a_ClSep)
  var_ord <- factor(x = rownames(clSep_rate), 
                    levels = unique(rownames(clSep_rate)))
  
  data.frame(var = var_ord,
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
#' ggplot2::ggplot() + ggproto_screeplot_ClSep(data = dat, clas, 1, 2)
#' 
#' ggplot2::ggplot() +
#'   ggproto_screeplot_ClSep(data = dat, class = clas,
#'                           num_class_lvl_A = 2, num_class_lvl_B = 3) +
#'   ggplot2::theme_bw()

ggproto_screeplot_ClSep <- function(data, 
                                    class,
                                    num_class_lvl_A = 1,
                                    num_class_lvl_B = 2) {
  .df_scree_ClSep <- df_scree_ClSep(data, class, num_class_lvl_A, num_class_lvl_B)
  axis_labs <- c("Variable", "Cluster seperation")
  lgnd_labs <- c("Variable cluster seperation", 
                 "Cummulative cluster seperation ")
  ## List of ggproto's that is addable to a ggplot object.
  list(
    ## Individual feature bars
    ggplot2::geom_bar(data = .df_scree_ClSep, stat = "identity", 
                      mapping = ggplot2::aes(x = var, y = var_clSep, 
                                             fill = lgnd_labs[1])),
    ## Cummulative feature line
    ggplot2::geom_line(data = .df_scree_ClSep, lwd = 1.2,
                       mapping = ggplot2::aes(x = var, y = cumsum_clSep,
                                              color = lgnd_labs[2], group = 1)),
    ggplot2::geom_point(data = .df_scree_ClSep, shape = 18, size = 4,
                        mapping = ggplot2::aes(
                          x = var, y = cumsum_clSep,
                          color = lgnd_labs[2])),
    ## Titles and colors
    ggplot2::labs(x = axis_labs[1], y = axis_labs[2], 
                  colour = "", fill = ""),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30),
                   legend.position = "bottom"),
    ggplot2::scale_fill_manual(values = palette()[1]),
    ggplot2::scale_colour_manual(values = palette()[2])
  )
}

