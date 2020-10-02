## CAREFUL WITH SOURCING HERE BEACAUSE RELATIVE PATHS FOR KNITTING .RMD
source(here::here("R/ggproto_screeplot_clSep.r"))

#' Creates a data frame of a single variable permutation of the data
#' (specified variable has it's values shuffled, resampled without replacment).
#' 
#' @examples
#' dat <- tourr::flea[, 1:6]
#' permute_var(dat)
#' 
#' permute_var(data = dat, permute_var_num = 4)
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
#' palette(RColorBrewer::brewer.pal(12, "Dark2"))
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
      ggplot2::geom_jitter(ggplot2::aes(x = !!rlang::enquo(i), y = var_clSep), tgt_df, 
                           width = .3, height = 0, 
                           color = .cols[i], alpha = .alp, shape = 3)
    ## Add boxplots for permuted pts
    proto_perm_jitter[[p + i]] <-
      ggplot2::geom_boxplot(ggplot2::aes(x = !!rlang::enquo(i), var_clSep), tgt_df, 
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



