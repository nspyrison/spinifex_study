## CAREFUL WITH SOURCING HERE BEACAUSE RELATIVE PATHS FOR KNITTING .RMD
try(source("./R/permutation_feature_importance.r"),  silent = T)
try(source("../R/permutation_feature_importance.r"), silent = T) ## Relative, kniting

#' Produces a data frame of the mean, mean permuted cluster seperation. 
#' Doesn't calculate Cummulative or order by MMP clSep
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' df_scree_MMP_clSep(dat, clas)
#' 
#' df_scree_MMP_clSep(data = dat, class = clas, num_class_lvl_a = 2, 
#'                    num_class_lvl_b = 3, n_reps = 250)
df_scree_MMP_clSep <- function(data,
                               class,
                               num_class_lvl_a = 1,
                               num_class_lvl_b = 2,
                               n_reps = 500,
                               do_rescale = TRUE) {
  require("magrittr") ## For %>% namespace.
  require("dplyr")
  data <- as.data.frame(data)
  p <- ncol(data)
  ## clSep on original dataset
  df_orig_clSep <- 
    df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b, do_rescale)
  df_orig_clSep$var_rank_num <- 1:p
  var_rank_lookup <- df_orig_clSep[c("data_colnum", "var", "var_rank_num")]
  
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
    ## Remove rows when they are the perumtation variable as they are bringing down the means.
    remove_var_nm <- var_rank_lookup[var_rank_lookup$var_rank_num == i, 1]
    this_df_mean_perm_clSep <- this_df_mean_perm_clSep %>% 
      dplyr::filter(var != remove_var_nm)
    
    df_mean_perm <- rbind(df_mean_perm, this_df_mean_perm_clSep)
  }
  
  ## Aggregate for LMP (largest, mean permuted) and MMP (mean, mean permuted) or "very mean permuted"
  df_MMP <- dplyr::group_by(df_mean_perm, var, data_colnum) %>%
    dplyr::summarise(.groups = "drop_last",
                     MMP_clSep = mean(mean_perm_clSep))
  
  ## Order and find cummulative
  ord <- order(df_MMP$MMP_clSep, decreasing = TRUE)
  df_MMP <- df_MMP[ord, ]
  df_MMP$cumsum_MMP_clSep <- cumsum(df_MMP$MMP_clSep)
  df_MMP$var <- factor(df_MMP$var, levels = df_MMP$var)
  
  ## Return 
  as.data.frame(df_MMP)
}

#' Produces a ggproto object; side-by-side bars of the original clSep and the
#' mean, mean permuted (MMP) clSep. As ordered, by the original clSep, rather than MMP. 
#' Also adds their respective cummulative lines.
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' palette(RColorBrewer::brewer.pal(12, "Dark2"))
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
                                   n_reps = 200,
                                   do_rescale = TRUE) {
  require("magrittr") ## For %>% namespace.
  ## clSep on original dataset
  df_orig_clSep <- 
    df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b, do_rescale)
  df_orig_clSep$var_rank_num <- 1:ncol(data)
  ## MMP_clSep
  df_MMP <- df_scree_MMP_clSep(data, class, num_class_lvl_a, num_class_lvl_b,
                               n_reps, do_rescale)
  ## Left join aggregated permuted data
  df_lj_origxMMP <-
    dplyr::left_join(df_orig_clSep, df_MMP, by = "var") %>% 
    dplyr::mutate(cumsum_MMP_clSep = cumsum(MMP_clSep))
  ## Pivot longer for side-by-side barcharts
  df_long_origxMMP <- df_lj_origxMMP %>% 
    tidyr::pivot_longer(cols = c(var_clSep, MMP_clSep),
                        names_to = "clSep_type", values_to = "clSep")
  ## Factor with level ordering, variable order
  df_long_origxMMP$clSep_type <- factor(df_long_origxMMP$clSep_type, 
                                        levels = c("var_clSep", "MMP_clSep"))
  ## Variable order on MMP_clSep
  ord <- order(df_long_origxMMP$MMP_clSep, decreasing = TRUE)
  df_long_origxMMP <- df_long_origxMMP[ord, ]
  
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
#' palette(RColorBrewer::brewer.pal(12, "Dark2"))
#' 
#' ggplot2::ggplot() + ggproto_MMP_clSep(dat, clas)
#' 
#' ggplot2::ggplot() +
#'   ggproto_MMP_clSep(data = dat, class = clas,
#'                     num_class_lvl_a = 2, num_class_lvl_b = 3, n_reps = 250,
#'                     do_rescale = TRUE, do_overlay_answer = TRUE)
ggproto_MMP_clSep <- function(data,
                              class,
                              num_class_lvl_a = 1,
                              num_class_lvl_b = 2,
                              n_reps = 500,
                              do_rescale = TRUE,
                              do_overlay_answer = FALSE) {
  require("magrittr") ## For %>% namespace.
  ## MMP_clSep
  df_MMP <- df_scree_MMP_clSep(data, class, num_class_lvl_a, num_class_lvl_b,
                               n_reps, do_rescale)
  
  ## List of ggproto objects
  lab_fill <- "Variable MMP"
  lab_col  <- "Cummulative MMP"
  ret <- list(
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
  
  ## Add answer overlay if requested
  if (do_overlay_answer == TRUE){
    p <- ncol(data)
    bar_unif <- 1 / p
    
    df_MMP$exampleResponse <- sample(c(0,1), size = p, replace = TRUE)
    df_MMP_eval <- df_MMP %>% 
      dplyr::mutate(.keep = "all",
                    diff   = MMP_clSep - bar_unif,
                    weight = sign(diff) * sqrt(abs(diff)),
                    marks  = weight * exampleResponse) %>% 
      dplyr::arrange(desc(MMP_clSep))
    
    col <- dplyr::if_else(sign(df_MMP_eval$diff) == 1, "green", "red")
    ggproto_overlay <- 
      list(
        ggplot2::geom_hline(yintercept = bar_unif, size = 1), 
        ggplot2::geom_text(ggplot2::aes(x = 7, y = bar_unif + .03, 
                                        label = paste0("Uniform Wt, 1/p = ", bar_unif)),
                           size = 4, hjust = 1), 
        ggplot2::geom_segment(data = df_MMP_eval, colour = col, size = 2,
                              ggplot2::aes(x = 1:p, y = bar_unif, xend = 1:p, yend = weight + bar_unif))
      )
    
    ret <- c(ret, ggproto_overlay)
  }
  
  return(ret)
}



