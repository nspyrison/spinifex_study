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
#' rep_permute_var_clSep(data = dat, class = clas, permute_rank_num = 2)
rep_permute_var_clSep <- function(data,
                                  class,
                                  permute_rank_num = 1,
                                  num_class_lvl_a = 1,
                                  num_class_lvl_b = 2,
                                  n_reps = 500,
                                  confidence = .99) {
  require("ggplot2")
  data <- as.data.frame(data)
  n <- nrow(data)
  p <- ncol(data)
  
  ## Init original data clSep
  real_df_scree_clSep <- df_scree_clSep(data, class, num_class_lvl_a, num_class_lvl_b)
  ## Decode rank_num to var_num
  .ord <- real_df_scree_clSep$data_colnum
  permute_var_num <- .ord[permute_rank_num]
  
  ## Long df for the data frames of single variable permuted data
  df_permuted_data <- NULL
  ## Long df for the df_scree_clSep: var, var_clSep, cumsum_clSep of the pemputed data
  df_permuted_scree_clSep <- NULL
  for(i in 1:n_reps) {
    ## Permuted data, i-th iteration
    .df <- permute_var(data, permute_var_num)
    df_permuted_data <- rbind(df_permuted_data, .df)
    
    ## Scree values of clSep for the permuted data, i-th iteration
    .df_scree <- data.frame(
      df_scree_clSep(.df, class, num_class_lvl_a, num_class_lvl_b),
      rep = i
    )
    df_permuted_scree_clSep <- rbind(df_permuted_scree_clSep, .df_scree)
  }
  
  ## List of the geom_jitters for the repitions on the permuted data
  proto_perm_jitter <- list()
  .alp <- .5
  .cols <- rep("grey", p)
  .cols[permute_rank_num] <- "black"
  .errbar_cols <- rep("darkgrey", p)
  .errbar_cols[permute_rank_num] <- "red"
  for (i in 1:p){
    .tgt_var <- real_df_scree_clSep$var[i]
    .tgt_df <- df_permuted_scree_clSep[df_permuted_scree_clSep$var == .tgt_var, ]
    .n <- nrow(.tgt_df)
    
    ## Create condfidence interval on the mean for the permuted data
    .single_tail_p <- (1 - confidence)/2
    .z_val <- qnorm(.single_tail_p, lower.tail = FALSE)
    .tgt_stats <- with(.tgt_df, data.frame(
      var = .tgt_var,
      mean = mean(var_clSep),
      ci_min = mean(var_clSep) - .z_val * sd(var_clSep) / sqrt(.n),
      ci_max = mean(var_clSep) + .z_val * sd(var_clSep) / sqrt(.n)
    ))
    
    ## Add jitter'd points
    proto_perm_jitter[[i]] <-
      geom_jitter(aes(x = !!enquo(i), y = var_clSep), .tgt_df, width = .3, height = 0,
                  color = .cols[i], alpha = .alp, shape = 3)
    ## Add cross bar for 95% CI (picture box of a boxplot)
    proto_perm_jitter[[p + i]] <-
      geom_crossbar(aes(x = !!enquo(i), y = mean, ymin = ci_min, ymax = ci_max), 
                    .tgt_stats, width = .8, size = 1, fatten = 1, color = .errbar_cols[i])
  }
  
  
  ## Palette, labels, and screeplot of clSep on unpermuted (real) data
  .tgt_lvls <- levels(as.factor(class))[c(num_class_lvl_a, num_class_lvl_b)]
  .var_nm <- colnames(data)[permute_var_num]
  .title <- paste0(100 * confidence, "% CI of the mean for ", 
                   .var_nm, "-permuted clSep (n_reps=", n_reps, ")")
  .subtitle <- paste0("Against original clSep sreeplot between the clusters ", .tgt_lvls[1], " and ", .tgt_lvls[2])
  palette(RColorBrewer::brewer.pal(3, "Dark2"))
  
  proto_scree_clSep <- ggproto_screeplot_clSep(data, class, num_class_lvl_a, num_class_lvl_b)
  
  ## Return ggproto list
  ggplot() +
    proto_scree_clSep +
    proto_perm_jitter +
    labs(title = .title, subtitle = .subtitle)
}
