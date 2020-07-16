#' Creates a data frame with the single selected variable permuted (resampled w/o replacment).
#' 
#' @examples 
#' dat <- tourr::flea[, 1:6]
#' permute_var(dat, 1)
#' 
#' permute_var(data = dat, permute_var_num = 4)
permute_var <- function(data,
                        permute_var_num) {
  data <- as.data.frame(data)
  n <- nrow(data)
  p <- ncol(data)
  
  p_ord <- sample(1:n, size = n)
  data[, permute_var_num] <- data[, permute_var_num][p_ord]
  colnames(data)[permute_var_num] <- paste0("permuted_", colnames(data)[permute_var_num])
  
  data
}


source("./R/ggproto_screeplot_ClSep.r")
#' Produces a dataset with the single selected variable permuted (resampled w/o replacment)
#' 
#' @examples 
#' dat <- tourr::flea[, 1:6]
#' clas <- tourr::flea$species
#' 
#' rep_permute_var_ClSep(data = dat, permute_var_num = 4)
if(F)
  data=dat;class=clas;num_class_lvl_A=1;num_class_lvl_B=2;permute_var_num=1;n_reps=100;
rep_permute_var_ClSep <- function(data = NULL,
                                  class = NULL,
                                  num_class_lvl_A = 1,
                                  num_class_lvl_B = 2,
                                  permute_var_num = NULL,
                                  n_reps = 500) {
  data <- as.data.frame(data)
  n <- nrow(data)
  p <- ncol(data)
  
  ## long df  for the data frames of single variable permuted data.
  df_permuted_data <- NULL
  ## long df for the df_scree_ClSep: var, var_ClSep, cumsum_ClSep of the pemputed data.
  df_permuted_df_scree_ClSep <- NULL
  for(i in 1:n_reps) {
    ## Permuted data, i-th iteration
    .df <- data.frame(
      permute_var(data = data, permute_var_num = permute_var_num),
      rep = i
    )
    df_permuted_data <- rbind(df_permuted_data, .df)
    
    ## scree of ClSep for the permuted data, i-th iteration
    .df_scree <- data.frame(
      df_scree_ClSep(data = .df, class = clas,
                     num_class_lvl_A = num_class_lvl_A, 
                     num_class_lvl_B = num_class_lvl_B),
      rep = i
    )
    df_permuted_df_scree_ClSep <- rbind(df_permuted_df_scree_ClSep, .df_scree)
  }
  
  real_df_scree_ClSep <- df_scree_ClSep(data, clas, num_class_lvl_A, num_class_lvl_B)
  real_var_ord <- real_df_scree_ClSep$var
    
  ## List of the geom_jitters for the repitions on the permuted data
  proto_perm_jitter <- list()
  .alp <- .5
  .cols <- rep("grey", p)
  .cols[permute_var_num] <- "black"
  for (i in 1:p){
    .tgt_var <- real_var_ord[i]
    .tgt_df <- df_permuted_df_scree_ClSep[df_permuted_df_scree_ClSep$var %in% .tgt_var, ]
    
    proto_perm_jitter[[i]] <-
      ggplot2::geom_jitter(.tgt_df$var_clSep, color = .cols[i], 
                           alpha = .alp, shape = 3,
                           mapping = ggplot2::aes(x = var, y = var_clSep)
    )
  }
  
  palette(RColorBrewer::brewer.pal(3, "Dark2")) 
  real_ggproto_screeplot_ClSep <- 
    ggproto_screeplot_ClSep(data = data, class = class,
                            num_class_lvl_A = num_class_lvl_A, 
                            num_class_lvl_B = num_class_lvl_B)
  
  ggplot2::ggplot() + real_ggproto_screeplot_ClSep + 
    ggplot2::geom_jitter(ls_permuted_df_scree_ClSep[[i]], color = "black",
                         mapping = ggplot2::aes(x = i, y = i), ) +
    ggplot2::geom_jitter(z, mapping = ggplot2::aes(x = 1, y = x), color = "grey")
}

library("ggplot2")  
z <- data.frame(x = rnorm(500))
ggplot2::ggplot() + ggplot2::geom_jitter(z, mapping = ggplot2::aes(x = 1, y = x), color = "black")