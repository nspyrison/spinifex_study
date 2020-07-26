#' Creates a data frame with the single selected variable permuted (resampled w/o replacment).
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
  p <- ncol(data)
  
  p_ord <- sample(1:n, size = n)
  data[, permute_var_num] <- data[, permute_var_num][p_ord]
  ## Note don't rename; cause other issues; levels of the factor, etc
  ## colnames(data)[permute_var_num] <- paste0("permuted_", colnames(data)[permute_var_num])
  
  data
}



#' Produces a dataset with the single selected variable permuted (resampled w/o replacment)
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
  real_df_scree_clSep <- df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b)
  ## Decode rank_num to the varible number of the original data
  ord <- real_df_scree_clSep$data_colnum
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
  
  ## return long df of df_permuted_scree_clSep without original data df_scree_clSep
  df_permuted_scree_clSep
}



#' Produces a dataset with the single selected variable permuted (resampled w/o replacment)
#' 
#' @examples 
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' palette(RColorBrewer::brewer.pal(3, "Dark2"))
#' 
#' ggplot() + rep_permute_var_clSep(data = dat, class = clas, permute_rank_num = 2)
ggproto_rep_permute_var_clSep <- function(data,
                                          class,
                                          permute_rank_num = 1,
                                          num_class_lvl_a = 1,
                                          num_class_lvl_b = 2,
                                          n_reps = 500) {
  require("ggplot2")
  data <- as.data.frame(data)
  p <- ncol(data)
  ## clSep on original dataset
  real_df_scree_clSep <- df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b)
  ## clSep on all permuted datasets
  df_permuted_clSep <- 
    df_rep_permute_var_clSep(data, class, permute_rank_num,
                             num_class_lvl_a, num_class_lvl_b, n_reps)

  ## List of the geom_jitters for the repitions on the permuted data
  proto_perm_jitter <- list()
  .alp <- .5
  .cols <- rep("lightgrey", p)
  .cols[permute_rank_num] <- "black"
  .boxplot_cols <- rep("darkgrey", p)
  .boxplot_cols[permute_rank_num] <- "black"
  for (i in 1:p){
    .tgt_var <- real_df_scree_clSep$var[i]
    .tgt_df <- df_permuted_clSep[df_permuted_clSep$var == .tgt_var, ]
    .tgt_df$perm_clSep[i] <- mean(.tgt_df$var_clSep) - real_df_scree_clSep$var_clSep[i]
    
    ## Add jitter'd points
    proto_perm_jitter[[i]] <-
      geom_jitter(aes(x = !!enquo(i), y = var_clSep), .tgt_df, width = .3,
                  height = 0, color = .cols[i], alpha = .alp, shape = 3)
    ## Add boxplots for permuted pts
    proto_perm_jitter[[p + i]] <-
      geom_boxplot(aes(x = !!enquo(i)), .tgt_stats, 
                   width = .8, size = 1, fatten = 1, color = .boxplot_cols[i])
  }
  
  
  ## Palette, labels, and screeplot of clSep on unpermuted (real) data
  .tgt_lvls <- levels(as.factor(class))[c(num_class_lvl_a, num_class_lvl_b)]
  .var_nm <- colnames(data)[permute_var_num]
  .title <- paste0(.var_nm, "-permuted cluster seperation clSep (n_reps=", n_reps, ")")
  .subtitle <- paste0("Against original clSep sreeplot between the clusters ", 
                      .tgt_lvls[1], " and ", .tgt_lvls[2])
  palette(RColorBrewer::brewer.pal(3, "Dark2"))
  
  ## Return ggproto list
  ggplot() +
    ggproto_screeplot_clSep(data, class, num_class_lvl_a, num_class_lvl_b) +
    proto_perm_jitter +
    labs(title = .title, subtitle = .subtitle)
}
