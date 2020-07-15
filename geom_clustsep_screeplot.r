#' Produce a screeplot of the cluster seperation between 2 selected levels. 
#' 
#' @examples 
#' data  <- tourr::flea[, 1:6]
#' class <- tourr::flea$species
#' palette(RColorBrewer::brewer.pal(3, "Dark2")) 
#' ggplot2::ggplot() + ggproto_screeplot_ClSep(data, class, 1, 2)

ggproto_screeplot_ClSep <- function(data = NULL, 
                                    class = NULL,
                                    num_class_lvl_A = 1,
                                    num_class_lvl_B = 2,
                                    rescale = TRUE){
  data <- as.matrix(data)
  if (rescale == TRUE) data <-as.data.frame(tourr::rescale(data))
  p <- ncol(data)
  
  ### PCA VARIANCE EXPLAINED
  # pca_obj <- prcomp(data)
  # df_pcaVar <- data.frame(
  #   pc_number = 1:p,
  #   rate_var_explained = pca_obj$sdev^2 / sum(pca_obj$sdev^2),
  #   cumsum_rate_var_explained = cumsum(pca_obj$sdev^2) / sum(pca_obj$sdev^2)
  # )
  
  ### CLUSTER SEPERATION EXPLAINED
  ## Identify and subset
  tgt_lvls <- levels(as.factor(class))[num_class_lvl_A:num_class_lvl_B]
  #sub <- data[class %in% tgt_lvls, ]
  ## Find Cluster means
  df_clMns_AB <- NULL
  for (i in 1:length(tgt_lvls)) {
    .lvl_rows <- class ==tgt_lvls[i]
    .lvl_sub <- data[.lvl_rows, ]
    .row_ClMns <- apply(.lvl_sub, 2, mean)
    df_clMns_AB <- rbind(df_clMns_AB, .row_ClMns)
  }
  ## find constants and coefficients of the p-dim line between
  df_clMns_AB <- as.data.frame(df_clMns_AB)
  rownames(df_clMns_AB) <- paste0("ClMn ", tgt_lvls)
  
  ## p-dim line between ClMns
  df_pLine_AB <- rbind(df_clMns_AB[1, ],
                       df_clMns_AB[2, ] - df_clMns_AB[1, ])
  rownames(df_pLine_AB) <- c(
    paste0("constants: ClMn ", tgt_lvls[1]),
    paste0("coefficients: ClMn ", tgt_lvls[2], "-", tgt_lvls[1]))
  
  ##TODO: SHOULD THE INVERSE COVAR BE ON THE FULL, OR CL SUBSET OF THE DATA??
  df_ClSep <- df_pLine_AB[2,] * (1 / cov(data))
  
  a_ClSep <- abs(df_ClSep)
  clSep_rate <- t(a_ClSep[order(a_ClSep, decreasing = T)]) / sum(a_ClSep)
  var_ord <- factor(x = rownames(clSep_rate), 
                    levels = unique(rownames(clSep_rate)))
  
  df_scree_ClSep <- data.frame(var = var_ord,
                               var_clSep = as.vector(clSep_rate),
                               cumsum_var_clSep = cumsum(clSep_rate))
  
  labs <- c("Individual var", "Cumsum of var explained")
  ## List of ggproto's that is addable to a ggplot.
  list(
    ## Individual variance bars
    ggplot2::geom_bar(data = df_scree_ClSep, stat = "identity", 
                      mapping = ggplot2::aes(x = var, y = var_clSep, 
                                             fill = labs[1])),
    ## Cummulative var line
    ggplot2::geom_line(data = df_scree_ClSep, lwd = 1.2,
                       mapping = ggplot2::aes(x = var, y = cumsum_var_clSep,
                                              color = labs[2], group = 1)),
    ggplot2::geom_point(data = df_scree_ClSep, shape = 18, size = 4,
                        mapping = ggplot2::aes(
                          x = var, y = cumsum_var_clSep,
                          color = labs[2])),
    ## Titles, and colors 
    ggplot2::labs(x = "Variable", 
                  y = paste0("Cluster seperation (B/T ", tgt_lvls[1], ", ", tgt_lvls[2], ")"), 
                  colour = "", fill = ""),
    ggplot2::scale_fill_manual(values = palette()[1]),
    ggplot2::scale_colour_manual(values = palette()[2])
    
  )
}

