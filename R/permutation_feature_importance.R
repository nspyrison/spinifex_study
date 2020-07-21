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
if(F) ## 10 sec to run then another 12 sec to render in Plots window 
  require("tictoc");tic("Run rep_permute_var_clSep()");rep_permute_var_clSep(dat,clas,permute_rank_num=4);toc()
if(F)
  data=tourr::flea[, 1:6];class=tourr::flea$species;permute_rank_num=1;n_reps=100;
rep_permute_var_clSep <- function(data,
                                  class,
                                  permute_rank_num = 1,
                                  num_class_lvl_a = 1,
                                  num_class_lvl_b = 2,
                                  n_reps = 500) {
  require("ggplot2")
  data <- as.data.frame(data)
  n <- nrow(data)
  p <- ncol(data)
  
  ## Init original data clSep, 
  real_df_scree_clSep <- df_scree_clSep(data, clas, num_class_lvl_a, num_class_lvl_b)
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
      df_scree_clSep(.df, clas, num_class_lvl_a, num_class_lvl_b),
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
    
    ## Create 95% condfidence interval on the mean of the permuted data.
    .tgt_stats <- with(.tgt_df, data.frame(
      var = .tgt_var,
      mean = mean(var_clSep),
      ci95_min = mean(var_clSep) - 1.96 * sd(var_clSep) / sqrt(.n),
      ci95_max = mean(var_clSep) + 1.96 * sd(var_clSep) / sqrt(.n)
    ))
    
    ## Add jitter'd points
    proto_perm_jitter[[i]] <-
      geom_jitter(aes(x = !!enquo(i), y = var_clSep), .tgt_df, width = .3, height = 0,
                  color = .cols[i], alpha = .alp, shape = 3)
    ## Add cross bar for 95% CI (picture box of a boxplot)
    proto_perm_jitter[[p + i]] <-
      geom_crossbar(aes(x = !!enquo(i), y = mean, ymin = ci95_min, ymax = ci95_max), 
                    .tgt_stats, width = .8, size = 1, fatten = 1, color = .errbar_cols[i])
  }
  
  ## Palette, labels, and screeplot of clSep on unpermuted (real) data
  tgt_lvls <- levels(as.factor(class))[num_class_lvl_a:num_class_lvl_b]
  palette(RColorBrewer::brewer.pal(3, "Dark2")) 
  real_ggproto_screeplot_clSep <- 
    ggproto_screeplot_clSep(data, class, num_class_lvl_a, num_class_lvl_b)
  
  ## Return
  list(
    real_ggproto_screeplot_clSep,
    proto_perm_jitter,
    labs(title = "95% CI of the mean* of single-variable permuted clSep", 
         subtitle = paste0(
           "Against real clSep between ", tgt_lvls[1], " and ", tgt_lvls[2]))
  )
}

## example Errorbars/crossbars
# library("ggplot2")
# x <- rnorm(500)
# df <- data.frame(x,
#                  mean = mean(x),
#                  Z95sd.sqrtn = 1.96*sd(x)/sqrt(500),
#                  ymin = mean(x) - 1.96*sd(x)/sqrt(500),
#                  ymax = mean(x) + 1.96*sd(x)/sqrt(500)
# )
# ggplot(df, mapping = aes(x = 1, y = x)) + geom_jitter(alpha = .5) +
#   theme_minimal() +
#   geom_point(aes(x = 1, y = mean), size = 4, shape = 8) +
#   geom_errorbar(aes(ymin = ymin, ymax = ymax),
#                 width = .8, size=1.1, color = "red", alpha=.3)