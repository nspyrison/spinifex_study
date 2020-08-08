#' Creates a data frame of a single variable permutation of the data
#' (specified variable has it's values shuffled, resampled without replacment).
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' permute_var(dat)
#' 
#' permute_var(data = dat, permute_var_num = 4)
source("./R/ggproto_screeplot_clSep.r")
permute_var <- function(data,
                        permute_var_num,
                        do_rescale = TRUE) {
  if(do_rescale == TRUE) data <- tourr::rescale(data)
  data <- as.data.frame(data)
  n <- nrow(data)
  ## Draw the permutation order
  row_ord <- sample(1:n, size = n)
  ## Apply the ordering
  data[, permute_var_num] <- data[row_ord, permute_var_num]
  data
}



#' Creates a data frame for the variable clSep of a single variable permutation
#' of the data.
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' 
#' df_rep_permute_var_clSep(dat, clas)
#' 
#' df_rep_permute_var_clSep(data = dat, class = clas, permute_rank_num = 2,
#'                          num_class_lvl_a = 2, num_class_lvl_b = 3, n_reps = 250)
df_rep_permute_var_clSep <- function(data,
                                     class,
                                     permute_rank_num = 1,
                                     num_class_lvl_a = 1,
                                     num_class_lvl_b = 2,
                                     n_reps = 500,
                                     do_rescale = TRUE) {
  data <- as.data.frame(data)
  ## clSep on original dataset
  df_orig_clSep <- 
    df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b, do_rescale)
  ## Decode rank_num to the varible number of the original data
  ord <- df_orig_clSep$data_colnum
  permute_var_num <- ord[permute_rank_num]
  
  ## Long df for the df_scree_clSep: var, var_clSep, cumsum_clSep of the pemputed data
  df_permuted_scree_clSep <- NULL
  for(i in 1:n_reps) {
    ## Permuted data, i-th iteration
    df <- permute_var(data, permute_var_num)
    ## Scree values of clSep for the permuted data, i-th iteration
    df_scree <- data.frame(
      df_scree_clSep(df, class, num_class_lvl_a, num_class_lvl_b),
      rep = i
    )
    df_permuted_scree_clSep <- rbind(df_permuted_scree_clSep, df_scree)
  }
  
  ## Return long df of df_permuted_scree_clSep without original data df_scree_clSep
  df_permuted_scree_clSep
}



#' Produces a ggproto object; bars of the original clSep, jittered points-, 
#' and a boxplot- of the permutationed clSep.
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' palette(RColorBrewer::brewer.pal(8, "Dark2"))
#' 
#' ggplot2::ggplot() + ggproto_rep_permute_var_clSep(dat, clas)
#' 
#' ggplot2::ggplot() +
#'   ggproto_rep_permute_var_clSep(data = dat, class = clas, permute_rank_num = 2
#'                                 num_class_lvl_a = 2, num_class_lvl_b = 3, n_reps = 250)
ggproto_rep_permute_var_clSep <- function(data,
                                          class,
                                          permute_rank_num = 1,
                                          num_class_lvl_a = 1,
                                          num_class_lvl_b = 2,
                                          n_reps = 500,
                                          do_rescale = TRUE) {
  data <- as.data.frame(data)
  p <- ncol(data)
  ## clSep on original dataset
  df_orig_clSep <- 
    df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b, do_rescale)
  ## clSep on all permuted datasets
  df_permuted_clSep <-
    df_rep_permute_var_clSep(data, class, permute_rank_num,
                             num_class_lvl_a, num_class_lvl_b, n_reps)
  
  ## List of the geom_jitters for the repitions on the permuted data
  proto_perm_jitter <- list()
  df_clSep_orig_n_perm <- NULL
  .alp <- .5
  .cols <- rep("lightgrey", p)
  .cols[permute_rank_num] <- "black"
  .boxplot_cols <- rep("darkgrey", p)
  .boxplot_cols[permute_rank_num] <- "black"
  for (i in 1:p) {
    tgt_var <- df_orig_clSep$var[i]
    tgt_df <- df_permuted_clSep[df_permuted_clSep$var == tgt_var, ]
    
    ## Add jitter'd points
    proto_perm_jitter[[i]] <-
      ggplot2::geom_jitter(ggplot2::aes(x = !!enquo(i), y = var_clSep), tgt_df, 
                           width = .3, height = 0, 
                           color = .cols[i], alpha = .alp, shape = 3)
    ## Add boxplots for permuted pts
    proto_perm_jitter[[p + i]] <-
      ggplot2::geom_boxplot(ggplot2::aes(x = !!enquo(i), var_clSep), tgt_df, 
                            alpha = .4, size = 1, fatten = 1, 
                            color = .boxplot_cols[i], fill = .cols[i])
  }
  
  ## Palette, labels, and screeplot of clSep on unpermuted (orig) data
  .tgt_lvls <- levels(as.factor(class))[c(num_class_lvl_a, num_class_lvl_b)]
  .var_nm <- colnames(data)[permute_rank_num]
  .title <- paste0(.var_nm, "-permuted cluster seperation (n_reps=", n_reps, ")")
  .subtitle <- paste0("Against original clSep sreeplot between the clusters ",
                      .tgt_lvls[1], " and ", .tgt_lvls[2])
  
  ## Return ggproto list
  list(
    ggproto_screeplot_clSep(data, class, num_class_lvl_a, num_class_lvl_b),
    proto_perm_jitter,
    ggplot2::labs(title = .title, subtitle = .subtitle)
  )
}



#' Produces a data frame of the mean, mean permuted cluster seperation. 
#' Doesn't calculate Cummulative or order by MMP clSep
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' df_scree_MMP_clSep(dat, clas)
#' 
#' df_scree_MMP_clSep(data = dat, class = clas, num_class_lvl_a = 2, 
#'                    num_class_lvl_b = 3, n_reps = 250, do_reorder_by_MMP = TRUE)
df_scree_MMP_clSep <- function(data,
                               class,
                               num_class_lvl_a = 1,
                               num_class_lvl_b = 2,
                               n_reps = 500,
                               do_rescale = TRUE,
                               do_reorder_by_MMP = FALSE) {
  require("magrittr") ## For %>% namespace.
  data <- as.data.frame(data)
  p <- ncol(data)
  ## clSep on original dataset
  df_orig_clSep <- 
    df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b, do_rescale)
  df_orig_clSep$var_rank_num <- 1:p
  
  ## clSep on all permuted datasets
  df_mean_perm <- NULL
  for (i in 1:p){
    this_df_permuted_clSep <-
      df_rep_permute_var_clSep(data, class, permute_rank_num = i,
                               num_class_lvl_a, num_class_lvl_b, n_reps)
    ## Aggregated means of n_reps of permuted clSep
    this_df_mean_perm_clSep <- 
      dplyr::group_by(this_df_permuted_clSep, var, data_colnum) %>%
      dplyr::summarise(.groups = "drop_last",
                       mean_perm_clSep = mean(var_clSep)) %>%
      data.frame(permute_rank_num = i)
    df_mean_perm <- rbind(df_mean_perm, this_df_mean_perm_clSep)
  }
  
  ## Remove the diagonal elements from the aggregation, because they are the permuted variables.
  diag(df_mean_perm) <- NULL ## mean on NULL works as expected.
  ## Aggregate for LMP (largest, mean permuted) and MMP (mean, mean permuted) or "very mean permuted"
  df_MMP <- dplyr::group_by(df_mean_perm, var, data_colnum) %>%
    dplyr::summarise(.groups = "drop_last",
                     MMP_clSep = mean(mean_perm_clSep))
  
  if(do_reorder_by_MMP == TRUE){
    ## Order and find cummulative
    .ord <- order(df_MMP$MMP_clSep, decreasing = TRUE)
    df_MMP <- df_MMP[.ord, ]
    df_MMP$cumsum_MMP_clSep <- cumsum(df_MMP$MMP_clSep)
    df_MMP$var <- factor(x = df_MMP$var, levels = unique(df_MMP$var))
  }
  
  df_MMP
}

#' Produces a ggproto object; side-by-side bars of the original clSep and the
#' mean, mean permuted (MMP) clSep. As ordered, by the original clSep, rather than MMP. 
#' Also adds their respective cummulative lines.
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' palette(RColorBrewer::brewer.pal(8, "Dark2"))
#' 
#' ggplot2::ggplot() + ggproto_origxMMP_clSep(data = dat, class = clas)
#' 
#' ggplot2::ggplot() +
#'   ggproto_origxMMP_clSep(data = dat, class = clas,
#'                          num_class_lvl_a = 2, num_class_lvl_b = 3, n_reps = 250)
ggproto_origxMMP_clSep <- function(data,
                                   class,
                                   num_class_lvl_a = 1,
                                   num_class_lvl_b = 2,
                                   n_reps = 500,
                                   do_rescale = TRUE) {
  require("magrittr") ## For %>% namespace.
  ## clSep on original dataset
  df_orig_clSep <- 
    df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b, do_rescale)
  df_orig_clSep$var_rank_num <- 1:ncol(data)
  ## MMP_clSep
  df_MMP <- df_scree_MMP_clSep(data, class, num_class_lvl_a, num_class_lvl_b,
                               n_reps, do_reorder_by_MMP = FALSE, do_rescale)
  ## Left join aggregated permuted data
  df_lj_origxMMP <-
    dplyr::left_join(df_orig_clSep, df_MMP, by = "var") %>% 
    dplyr::mutate(cumsum_MMP_clSep = cumsum(MMP_clSep))
  ## Pivot longer for side-by-side barcharts
  df_long_origxMMP <- df_lj_origxMMP %>% 
    tidyr::pivot_longer(cols = c(var_clSep, MMP_clSep),
                        names_to = "clSep_type", values_to = "clSep")
  ## Factor ordering
  df_long_origxMMP$clSep_type <- factor(df_long_origxMMP$clSep_type, 
                                        levels = c("var_clSep", "MMP_clSep"))
  
  ## Initalize color handling
  lab_col  <- c("original", "MMP") ## Cummulative: geom_pt & geom_line colors; orig, adj LMP
  lab_fill <- c("original", "MMP") ## geom_bar fills; orig, MMP
  n_col    <- length(lab_col)
  n_fill   <- length(lab_fill)
  if(length(palette()) < n_col + n_fill) 
    warning("Using more colors than the palette() has. May need to set new palette, similar to `palette(RColorBrewer::brewer.pal(8, 'Dark2'))`.")
  ## List of ggproto objects
  list(
    ggplot2::geom_bar(ggplot2::aes(x = var, y = clSep, fill = clSep_type),
                      df_long_origxMMP,
                      position = "dodge", stat = "identity"),
    ## Cumsum clSep
    ggplot2::geom_line(ggplot2::aes(x = as.integer(var), y = cumsum_clSep,
                                    color = "z col1 -Cumsum clSep", group = 1),
                       df_long_origxMMP, lwd = 1.2),
    ggplot2::geom_point(ggplot2::aes(x = as.integer(var), y = cumsum_clSep,
                                     color = "z col1 -Cumsum clSep"),
                        df_long_origxMMP, shape = 18, size = 4),
    ## Cumsum adj. clSep
    ggplot2::geom_line(ggplot2::aes(x = var, y = cumsum_MMP_clSep, 
                                    color = "z col2 -Cumsum adj. clSep", group = 1),
                       df_long_origxMMP, lwd = 1.2),
    ggplot2::geom_point(ggplot2::aes(x = var, y = cumsum_MMP_clSep, 
                                     color = "z col2 -Cumsum adj. clSep"),
                        df_long_origxMMP, shape = 17, size = 4),
    ## Titles and colors
    ggplot2::labs(x = "Variable", y = "Cluster seperation"),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30),
                   legend.position = "bottom",
                   legend.direction = "vertical"),
    ggplot2::scale_fill_manual(values = palette()[1:n_fill],
                               name = "Variable cluster seperation",
                               labels = lab_fill),
    ggplot2::scale_colour_manual(values = palette()[(n_fill + 1):(n_fill + n_col)],
                                 name = "Cummulative cluster seperation",
                                 labels = lab_col)
  )
}



#' Produces a ggproto object; barchart of the mean, mean permuted (MMP) clSep 
#' and cummulative MMP.
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' palette(RColorBrewer::brewer.pal(8, "Dark2"))
#' 
#' ggplot2::ggplot() + ggproto_MMP_clSep(dat, clas)
#' 
#' ggplot2::ggplot() +
#'   ggproto_MMP_clSep(data = dat, class = clas,
#'                     num_class_lvl_a = 2, num_class_lvl_b = 3, n_reps = 250)
ggproto_MMP_clSep <- function(data,
                              class,
                              num_class_lvl_a = 1,
                              num_class_lvl_b = 2,
                              n_reps = 500,
                              do_rescale = TRUE) {
  require("magrittr") ## For %>% namespace.
  ## MMP_clSep
  df_MMP <- df_scree_MMP_clSep(data, class, num_class_lvl_a, num_class_lvl_b,
                               n_reps, do_reorder_by_MMP = TRUE, do_rescale)
  
  ## List of ggproto objects
  lab_fill <- "Variable MMP"
  lab_col  <- "Cummulative MMP"
  list(
    ggplot2::geom_bar(ggplot2::aes(x = var, y = MMP_clSep, fill = lab_fill),
                      df_MMP, position = "dodge", stat = "identity"),
    ## Cumsum clSep
    ggplot2::geom_line(ggplot2::aes(x = as.integer(var), y = cumsum_MMP_clSep,
                                    color = lab_col, group = 1),
                       df_MMP, lwd = 1.2),
    ggplot2::geom_point(ggplot2::aes(x = as.integer(var), y = cumsum_MMP_clSep,
                                     color = lab_col),
                        df_MMP, shape = 18, size = 4),
    ## Titles and colors
    ggplot2::labs(x = "Variable", y = "Cluster seperation"),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30),
                   legend.position = "bottom",
                   legend.direction = "vertical"),
    ggplot2::scale_fill_manual(values = palette()[1],
                               name = "", labels = lab_fill),
    ggplot2::scale_colour_manual(values = palette()[2],
                                 name = "", labels = lab_col)
  )
}

