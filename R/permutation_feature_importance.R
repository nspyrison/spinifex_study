#' Creates a data frame of a single variable permutation of the data
#' (specified variable has it's values shuffled, resampled without replacment).
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' permute_var(dat, 1)
#' 
#' permute_var(data = dat, permute_var_num = 4)
source("./R/ggproto_screeplot_clSep.r")
permute_var <- function(data,
                        permute_var_num) {
  data <- as.data.frame(data)
  n <- nrow(data)
  
  ## Draw the permutation order
  row_ord <- sample(1:n, size = n)
  ## Apply the ordering
  data[, permute_var_num] <- data[, permute_var_num][row_ord]
  
  data
}



#' Creates a data frame for the variable clSep of a single variable permutation
#' of the data.
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' 
#' df_rep_permute_var_clSep(data = dat, class = clas, permute_rank_num = 2)
df_rep_permute_var_clSep <- function(data,
                                     class,
                                     permute_rank_num = 1,
                                     num_class_lvl_a = 1,
                                     num_class_lvl_b = 2,
                                     n_reps = 500) {
  data <- as.data.frame(data)
  ## clSep on original dataset
  df_real_clSep <- df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b)
  ## Decode rank_num to the varible number of the original data
  ord <- df_real_clSep$data_colnum
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
#' ggplot2::ggplot() + ggproto_rep_permute_var_clSep(data = dat, class = clas, permute_rank_num = 2)
ggproto_rep_permute_var_clSep <- function(data,
                                          class,
                                          permute_rank_num = 1,
                                          num_class_lvl_a = 1,
                                          num_class_lvl_b = 2,
                                          n_reps = 500) {
  data <- as.data.frame(data)
  p <- ncol(data)
  ## clSep on original dataset
  df_real_clSep <- df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b)
  ## clSep on all permuted datasets
  df_permuted_clSep <-
    df_rep_permute_var_clSep(data, class, permute_rank_num,
                             num_class_lvl_a, num_class_lvl_b, n_reps)
  
  ## List of the geom_jitters for the repitions on the permuted data
  proto_perm_jitter <- list()
  df_clSep_real_n_perm <- NULL
  .alp <- .5
  .cols <- rep("lightgrey", p)
  .cols[permute_rank_num] <- "black"
  .boxplot_cols <- rep("darkgrey", p)
  .boxplot_cols[permute_rank_num] <- "black"
  for (i in 1:p) {
    tgt_var <- df_real_clSep$var[i]
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
  
  ## Palette, labels, and screeplot of clSep on unpermuted (real) data
  .tgt_lvls <- levels(as.factor(class))[c(num_class_lvl_a, num_class_lvl_b)]
  .var_nm <- colnames(data)[permute_rank_num]
  .title <- paste0(.var_nm, "-permuted cluster seperation clSep (n_reps=", n_reps, ")")
  .subtitle <- paste0("Against original clSep sreeplot between the clusters ",
                      .tgt_lvls[1], " and ", .tgt_lvls[2])
  
  ## Return ggproto list
  list(
    ggproto_screeplot_clSep(data, class, num_class_lvl_a, num_class_lvl_b),
    proto_perm_jitter,
    ggplot2::labs(title = .title, subtitle = .subtitle)
  )
}


#' Produces a ggproto object; side-by-side bars of the original clSep and the
#' max of the mean raise in permuted clSep.
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' palette(RColorBrewer::brewer.pal(8, "Dark2"))
#' 
#' ggplot2::ggplot() + ggproto_exhaustive_clSep(data = dat, class = clas)
ggproto_exhaustive_clSep <- function(data,
                                     class,
                                     num_class_lvl_a = 1,
                                     num_class_lvl_b = 2,
                                     n_reps = 500) {
  data <- as.data.frame(data)
  p <- ncol(data)
  ## clSep on original dataset
  df_real_clSep <- df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b)
  
  ## clSep on all permuted datasets
  df_exhaus_mean <- NULL
  for (i in 1:p){
    df_permuted_clSep <-
      df_rep_permute_var_clSep(data, class, permute_rank_num = i,
                               num_class_lvl_a, num_class_lvl_b, n_reps)
    ## Aggregated means of n_reps of permuted clSep
    df_perm_mean_clSep <- dplyr::group_by(df_permuted_clSep, var) %>%
      dplyr::summarise(mean_perm_clSep = mean(var_clSep)) %>%
      dplyr::ungroup() %>%
      data.frame(permute_rank_num = i)
    df_exhaus_mean <- rbind(df_exhaus_mean, df_perm_mean_clSep)
  }
  df_exhaus_max_mean <- dplyr::group_by(df_exhaus_mean, var) %>%
    dplyr::filter(mean_perm_clSep == max(mean_perm_clSep)) %>%
    dplyr::mutate(.keep = "unused", max_mean_perm_clSep = mean_perm_clSep) %>%
    dplyr::ungroup() %>% 
    dplyr::arrange(desc(max_mean_perm_clSep))
  df_exhaus_max_mean$cumsum_max_mean_perm_clSep <-
    cumsum(df_exhaus_max_mean$max_mean_perm_clSep)
  
  df_real_clSep <- cbind(df_real_clSep, var_rank_num = 1:p)
  ## Left join aggregated permuted data
  df_clSep_real_n_perm <-
    dplyr::left_join(df_real_clSep, df_exhaus_max_mean, by = "var")
  ## Subset to var_clSep, max_mean_perm_clSep and pivot longer; df1
  .df_long1_clSep_real_n_perm <-
    dplyr::select(df_clSep_real_n_perm, data_colnum, var, var_rank_num, 
                  permute_rank_num, var_clSep, max_mean_perm_clSep) %>%
    tidyr::pivot_longer(cols = c(var_clSep, max_mean_perm_clSep),
                        names_to = "clSep_type", values_to = "clSep")
  ## Subset to cumsum_clSep, cumsum_max_mean_perm_clSep and pivot longer; df2
  .df_long2_clSep_real_n_perm <-
    dplyr::select(df_clSep_real_n_perm, cumsum_clSep, cumsum_max_mean_perm_clSep) %>%
    tidyr::pivot_longer(cols = c(cumsum_clSep, cumsum_max_mean_perm_clSep),
                        names_to = "cumsum_clSep_type", values_to = "cumsum_clSep")
  ## Rejoin after pivoting longer
  df_long_clSep_real_n_perm <-
    cbind(.df_long1_clSep_real_n_perm, .df_long2_clSep_real_n_perm)
  
  axis_labs <- c("Variable", "Cluster seperation")
  lgnd_labs <- c("Variable cluster seperation", "Cummulative cluster seperation")
  list(
    ggplot2::geom_bar(ggplot2::aes(x = var, y = clSep, fill = clSep_type),
                      df_long_clSep_real_n_perm,
                      position = "dodge", stat = "identity"),
    ggplot2::geom_line(ggplot2::aes(x = var+.33, y = cumsum_clSep, color = lgnd_labs[2], 
                                    group = 1),
                       df_long_clSep_real_n_perm, lwd = 1.2, position = "dodge"),
    ggplot2::geom_point(ggplot2::aes(x = var, y = cumsum_clSep, color = lgnd_labs[2]),
                        df_long_clSep_real_n_perm, shape = 18, size = 4),
    ## Titles and colors
    ggplot2::labs(x = axis_labs[1], y = axis_labs[2], colour = "", fill = ""),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30),
                   legend.position = "bottom"),
    ggplot2::scale_fill_manual(values = palette()[1:2]),
    ggplot2::scale_colour_manual(values = palette()[3])
  )
}
