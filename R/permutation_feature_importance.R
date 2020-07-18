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
  require("ggplot2")
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
  .errbar_cols <- rep("darkgrey", p)
  .errbar_cols[permute_var_num] <- "red"
  for (i in 1:p){
    .tgt_var <- real_var_ord[i]
    .tgt_df <- df_permuted_df_scree_ClSep[df_permuted_df_scree_ClSep$var %in% .tgt_var, ]
    .n <- nrow(.tgt_df)
    .df <- with(.tgt_df, data.frame(
      .tgt_df,
      mean = mean(var_clSep),
      ci95_min = mean(var_clSep) - 1.96 * sd(var_clSep) / sqrt(.n),
      ci95_max = mean(var_clSep) + 1.96 * sd(var_clSep) / sqrt(.n)
    ))
    
    ## Add jitter'd points
    proto_perm_jitter[[i]] <-
      geom_jitter(.tgt_df$var_clSep, color = .cols[i],
                  alpha = .alp, shape = 3,
                  mapping = aes(x = var, y = var_clSep[i])
    ) 
    ## Add cross bar for 95% CI (picture box of a boxplot)
    proto_perm_jitter[[p + i]] <-
      geom_crossbar(aes(ymin = ci95_min, ymax = ci95_max), 
                    width = .8, size = 1.1, fatten = 3, color = .errbar_cols[i])
  }
  
  
  tgt_lvls <- levels(as.factor(class))[num_class_lvl_A:num_class_lvl_B]
  palette(RColorBrewer::brewer.pal(3, "Dark2")) 
  real_ggproto_screeplot_ClSep <- 
    ggproto_screeplot_ClSep(data = data, class = class,
                            num_class_lvl_A = num_class_lvl_A,
                            num_class_lvl_B = num_class_lvl_B)
  
  ggplot() + real_ggproto_screeplot_ClSep + 
    proto_perm_jitter + 
    labs(title = "95% CI of the mean of single-variable permuted ClSep", 
         subtitle = paste0(
           "Against real ClSep between ", tgt_lvls[1], " and ", tgt_lvls[2]))
}

library("ggplot2")  
x <- rnorm(500)
df <- data.frame(x,
                 mean = mean(x),
                 Z95sd.sqrtn = 1.96*sd(x)/sqrt(500),
                 ymin = mean(x) - 1.96*sd(x)/sqrt(500),
                 ymax = mean(x) + 1.96*sd(x)/sqrt(500)
)
ggplot(df, mapping = aes(x = 1, y = x)) + geom_jitter(alpha = .5) +
  theme_minimal() + 
  geom_point(aes(x = 1, y = mean), size = 4, shape = 8) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), 
                width = .8, size=1.1, color = "red", alpha=.3)