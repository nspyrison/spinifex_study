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
  require("magrittr") ## For %>% namespace.
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
    suppressMessages( ## Suppress ungroup output message.
      df_perm_mean_clSep <- dplyr::group_by(df_permuted_clSep, var) %>%
        dplyr::summarise(mean_perm_clSep = mean(var_clSep)) %>%
        dplyr::ungroup() %>%
        data.frame(permute_rank_num = i)
    )
    df_exhaus_mean <- rbind(df_exhaus_mean, df_perm_mean_clSep)
  }
  
  df_exhaus_max_mean <- dplyr::group_by(df_exhaus_mean, var) %>%
    dplyr::filter(mean_perm_clSep == max(mean_perm_clSep)) %>%
    dplyr::mutate(.keep = "unused", max_mean_perm_clSep = mean_perm_clSep) %>%
    dplyr::ungroup()
  
  df_real_clSep <- cbind(df_real_clSep, var_rank_num = 1:p)
  ## Left join aggregated permuted data
  df_clSep_real_n_perm <-
    dplyr::left_join(df_real_clSep, df_exhaus_max_mean, by = "var") %>% 
    dplyr::mutate(diff = max_mean_perm_clSep - var_clSep,
                  adj_clSep = var_clSep + .5 * diff,
                  cumsum_adj_clSep = cumsum(adj_clSep))
  
  ## Pivot longer for side-by-side barcharts
  df_long_clSep_real_n_perm <- df_clSep_real_n_perm %>% 
    tidyr::pivot_longer(cols = c(var_clSep, max_mean_perm_clSep),
                        names_to = "clSep_type", values_to = "clSep")
  df_long_clSep_real_n_perm$clSep_type <- 
    factor(df_long_clSep_real_n_perm$clSep_type, 
           levels = c("var_clSep", "max_mean_perm_clSep"))
  
  list(
    ggplot2::geom_bar(ggplot2::aes(x = var, y = clSep, fill = clSep_type),
                      df_long_clSep_real_n_perm,
                      position = "dodge", stat = "identity"),
    ## Cumsum clSep
    ggplot2::geom_line(ggplot2::aes(x = as.integer(var) - .0, y = cumsum_clSep, ## -.1 for dodge
                                    color = "z col1 -Cumsum clSep", group = 1),
                       df_long_clSep_real_n_perm, lwd = 1.2),
    ggplot2::geom_point(ggplot2::aes(x = as.integer(var) - .0, y = cumsum_clSep, ## -.1 for dodge
                                     color = "z col1 -Cumsum clSep"),
                        df_long_clSep_real_n_perm, shape = 18, size = 4),
    ## Cumsum adj. clSep
    ggplot2::geom_line(ggplot2::aes(x = var, y = cumsum_adj_clSep, 
                                    color = "z col2 -Cumsum adj. clSep", group = 1),
                       df_long_clSep_real_n_perm, lwd = 1.2),
    ggplot2::geom_point(ggplot2::aes(x = var, y = cumsum_adj_clSep, 
                                     color = "z col2 -Cumsum adj. clSep"),
                        df_long_clSep_real_n_perm, shape = 17, size = 4),
    ## Adj cluster sep mid pts
    ggplot2::geom_bar(ggplot2::aes(x = var, y = .5 * adj_clSep, ## Halfed b/c it gets doubled in the pivot_longer.
                                   fill = "z fill col3"),
                      df_long_clSep_real_n_perm, 
                      width = .2, alpha = .5, stat = "identity"),
    ## Titles and colors
    ggplot2::labs(x = "Variable", y = "Cluster seperation"),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30),
                   legend.position = "bottom",
                   legend.direction = "vertical"),
    ggplot2::scale_fill_manual(
      values = palette()[1:3],
      name = "Variable cluster seperation",
      labels = c("Full sample", "Largest permuted mean", "Adj. largest permuted mean")),
    ggplot2::scale_colour_manual(
      values = palette()[4:5],
      name = "Cummulative cluster seperation",
      labels = c("Cum. full sample", "Adj. largest permuted mean", "Adj. largest permuted mean"))
  )
}
