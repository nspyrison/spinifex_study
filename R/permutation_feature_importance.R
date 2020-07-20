#' Creates a data frame with the single selected variable permuted (resampled w/o replacment).
#' 
#' @examples 
#' dat <- tourr::flea[, 1:6]
#' permute_var(dat, 1)
#' 
#' permute_var(data = dat, permute_var_num = 4)
source("./R/ggproto_screeplot_ClSep.r")
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
#' rep_permute_var_ClSep(data = dat, class = clas, permute_var_num = 4)
if(F) ## 10 sec to run then another 12 sec to render in Plots window 
  require("tictoc");tic("Run rep_permute_var_ClSep()");rep_permute_var_ClSep(dat,clas,permute_var_num=4);toc()
if(F)
  data=tourr::flea[, 1:6];class=tourr::flea$species;permute_var_num=1;n_reps=100;
rep_permute_var_ClSep <- function(data,
                                  class,
                                  num_class_lvl_A = 1,
                                  num_class_lvl_B = 2,
                                  permute_var_num,
                                  n_reps = 500) {
  require("ggplot2")
  data <- as.data.frame(data)
  n <- nrow(data)
  p <- ncol(data)
  
  ## Long df for the data frames of single variable permuted data
  df_permuted_data <- NULL
  ## Long df for the df_scree_ClSep: var, var_ClSep, cumsum_ClSep of the pemputed data
  df_permuted_scree_ClSep <- NULL
  for(i in 1:n_reps) {
    ## Permuted data, i-th iteration
    .df <- permute_var(data, permute_var_num)
    df_permuted_data <- rbind(df_permuted_data, .df)
    
    ## Scree values of ClSep for the permuted data, i-th iteration
    .df_scree <- data.frame(
      df_scree_ClSep(.df, clas, num_class_lvl_A, num_class_lvl_B),
      rep = i
    )
    df_permuted_scree_ClSep <- rbind(df_permuted_scree_ClSep, .df_scree)
  }
  
  real_df_scree_ClSep <- df_scree_ClSep(data, clas, num_class_lvl_A, num_class_lvl_B)
  .ord <- real_df_scree_ClSep$data_colnum
  
  ## List of the geom_jitters for the repitions on the permuted data
  proto_perm_jitter <- list()
  .alp <- .5
  .cols <- rep("grey", p)
  .cols[permute_var_num] <- "black"
  .cols <- .cols[.ord] ## Apply the ordering of ClSep rank
  .errbar_cols <- rep("darkgrey", p)
  .errbar_cols[permute_var_num] <- "red"
  .errbar_cols <- .errbar_cols[.ord] ## Apply the ordering of ClSep rank

  for (i in 1:p){
    .tgt_var <- real_df_scree_ClSep$var[i]
    .tgt_df <- df_permuted_scree_ClSep[df_permuted_scree_ClSep$var == .tgt_var, ]
    .n <- nrow(.tgt_df)
    
    .tgt_stats <- with(.tgt_df, data.frame(
      var = .tgt_var,
      mean = mean(var_ClSep),
      ci95_min = mean(var_ClSep) - 1.96 * sd(var_ClSep) / sqrt(.n),
      ci95_max = mean(var_ClSep) + 1.96 * sd(var_ClSep) / sqrt(.n)
    ))
    
    ## Add jitter'd points
    proto_perm_jitter[[i]] <-
      geom_jitter(aes(x = !!enquo(i), y = var_ClSep), .tgt_df, width = .3, height = 0,
                  color = .cols[i], alpha = .alp, shape = 3)
    ## Add cross bar for 95% CI (picture box of a boxplot)
    proto_perm_jitter[[p + i]] <-
      geom_crossbar(aes(x = !!enquo(i), y = mean, ymin = ci95_min, ymax = ci95_max), 
                    .tgt_stats, width = .8, size = 1, fatten = 1, color = .errbar_cols[i])
  }
  
  ## Palette, labels, and screeplot of ClSep on unpermuted (real) data
  tgt_lvls <- levels(as.factor(class))[num_class_lvl_A:num_class_lvl_B]
  palette(RColorBrewer::brewer.pal(3, "Dark2")) 
  real_ggproto_screeplot_ClSep <- 
    ggproto_screeplot_ClSep(data, class, num_class_lvl_A, num_class_lvl_B)
  
  ## Render
  ggplot() + 
    real_ggproto_screeplot_ClSep + 
    proto_perm_jitter + 
    labs(title = "95% CI of the mean* of single-variable permuted ClSep", 
         subtitle = paste0(
           "Against real ClSep between ", tgt_lvls[1], " and ", tgt_lvls[2]))
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