source(here::here("R/sim_tidyverse.r")) ## For banana_tform() and rotate()

##TODO rotate_mtvnorm() CAUSING LIST
## handles p=4 or p=6, but only rotates var 1, into var 4 (var 1 always has signal, var 4 never does.)
rotate_mtvnorm <- function(x, ang){
  orig_attr <- attributes(x)
  orig_colnames <- colnames(x)
  x <- as.data.frame(x)
  colnames(x) <- paste0("V", 1:ncol(x))
  ## Apply transform
  c <- cos(ang)
  s <- sin(ang)
  ret <- mutate(x,
                V1 = c * V1 + -s * V4,
                V4 = s * V1 +  c * V4)
  
  ## Return
  colnames(ret) <- orig_colnames
  attr(ret, "cluster") <- orig_attr$cluster ## Cluster levels
  attr(ret, "args")    <- orig_attr$args  ## List of args
  attr(ret, "call")    <- orig_attr$call    ## the call   
  return(ret)
}


## Creates a 'V' in the shape of ">", by shifting 1/5 of the obs to be offset.
banana_tform_mtvnorm <- function(x){
  ## Initialize
  f <- floor(nrow(x) / 5) ## length of 1/5 of the data
  f1 <- 1:f
  f2 <- f1 + f
  f3 <- f2 + f
  f4 <- f3 + f
  ## f5, last fifth of the data stays as is. 
  
  ## Apply transform
  x[f1, 1] <- x[f1, 1] - 0.5
  x[f1, 2] <- x[f1, 2] + 0.5
  x[f2, 1] <- x[f2, 1] - 0.5
  x[f2, 2] <- x[f2, 2] - 0.5
  x[f3, 1] <- x[f3, 1] - 1
  x[f3, 2] <- x[f3, 2] + 1
  x[f4, 1] <- x[f4, 1] - 1
  x[f4, 2] <- x[f4, 2] - 1
  
  x
}

#' Creates a data frame of multivariate data with clusters via mvnorm::rmvnorm().
#'
#' @param means List, each element is a p-length vectors, the variable means 
#' of this cluster.
#' @param sigmas List, each element is a square (p, p) matrix, the 
#' variance-covariance matrix for this cluster. If any matrix is not 
#' positive definite, it will be coerced with `lqmm::make.positive.definite()`
#' @param cl_obs List, of number of observations within each cluster.
#' matrix root of `sigmas`. Expects, "eigen", the default, "svd", or "chol". 
#' Also see `?mvtnorm::rmvnorm()`.
#' @param do_shuffle Boolean specifying if order resampling should be applied 
#' to the rows and columns, Such that cluster rows are not all together and 
#' signal columns are not in the same order.
sim_mvtnorm_cl <- function(means,  ## Required
                           sigmas, ## Required
                           cl_obs = cl_obs,
                           do_shuffle = FALSE){
  if(length(cl_obs) == 1 & is.numeric(cl_obs) == TRUE)
    cl_obs <- rep(list(cl_obs), length(means))
  means  <- as.list(means)
  sigmas <- as.list(sigmas)
  cl_obs <- as.list(cl_obs)
  p <- length(means[[1]])
  k <- length(means)
  ## Means and covariances are both of length k, clusters
  stopifnot(all(k == c(length(means), length(sigmas)))) 
  ## Elements of means and elements covariances have length, rows/cols p, number of numeric variables.
  stopifnot(all(p == c(length(means[[1]]), nrow(sigmas[[1]]), ncol(sigmas[[1]]))))
  
  
  ## Simulate with checks
  df_sim <- NULL
  for(i in 1:k){
    cov <- as.matrix(sigmas[[i]])
    ## Check if this sigma is positive semi-definite, and a symmetric matrix .
    if(lqmm::is.positive.definite(cov) == FALSE){
      warning(paste0("sigmas[[", i, "]] wasn't a positive definite matrix. Applied lqmm::make.positive.definite()."))
      cov <- lqmm::make.positive.definite(cov)
    }
    if(base::isSymmetric.matrix(cov) == FALSE)
      stop(paste0("sigma[[", i, "]] is not a symetric matrix, all covariance metrices must be symetric (and positive semi-definate)."))
    
    ## Sample
    this_cl <- mvtnorm::rmvnorm(n = cl_obs[[i]],
                                mean = means[[i]],
                                sigma = cov)
    df_sim <- rbind(df_sim, this_cl)
  }
  df_sim <- apply(df_sim, 2, function(c)(c - mean(c)) / sd(c)) ## Standardize by column mean, sd.
  
  ## Init class
  cluster <- factor(paste0("cl ", rep(letters[1:k], unlist(cl_obs))))
  ## Reorder rows and columns if needed
  if(do_shuffle == TRUE){
    row_ord <- sample(1:nrow(df_sim))
    col_ord <- sample(1:p)
    
    ## Apply the shuffle reordering
    df_sim <- df_sim[row_ord, col_ord]
    cluster  <- cluster[row_ord]
    for (i in 1:k){
      means[[i]]  <- means[[i]][col_ord]
      sigmas[[i]] <- sigmas[[i]][col_ord, col_ord]
    }
  }
  
  ## Row/col names, after shuffle if required
  rownames(df_sim) <- 1:nrow(df_sim)
  colnames(df_sim) <- paste0("V", 1:ncol(df_sim))
  df_sim <- as.data.frame(df_sim)
  
  ## Capture attributes
  args <- list(means = means, sigmas = sigmas, cl_obs = cl_obs, 
                      do_shuffle = do_shuffle)
  this_call <- call("sim_pDim_kCl", args)
  ## Record attributes
  attr(df_sim, "cluster") <- cluster ## Cluster levels
  attr(df_sim, "args")    <- args  ## List of args
  attr(df_sim, "call")    <- this_call    ## Stored call, use eval(attr(sim, "call")) to reproduce
  
  return(df_sim)
}



#' Hard-coded wrapper function that uses mvnorm::rmvnorm() to simulate 3 
#' simulation locations by 3 (mclust 5 paper like) VC models. These 9 simulations
#' are assigned to global variables and saved as .rda files for use in the 
#' {shiny} app for the user study.
#'
#' @param cl_obs List, of number of observations within each cluster.
#' @examples
#' sim_user_study(cl_obs = 140, do_save = TRUE)
sim_user_study <- function(cl_obs = 140,
                           do_save = FALSE){
  ## HARD CODE SEED!!!
  set.seed(123)
  ## Initialize
  location <- c("0_1", "33_66", "50_50")
  VC <- c("EEE", "EEV", "banana")
  p_s <- c(4, 6)
  p_nms <- paste0("p", p_s)
  in_nms  <- c("EEE_p4_0_1", "EEE_p4_33_66", "EEE_p4_50_50",
               "EEV_p4_0_1", "EEV_p4_33_66", "EEV_p4_50_50",
               "banana_p4_0_1", "banana_p4_33_66", "banana_p4_50_50",
               "EEE_p6_0_1", "EEE_p6_33_66", "EEE_p6_50_50",
               "EEV_p6_0_1", "EEV_p6_33_66", "EEV_p6_50_50",
               "banana_p6_0_1", "banana_p6_33_66", "banana_p6_50_50")
  cov_nms <- paste0("covs_", in_nms)
  root <- paste0(here::here("apps/data/"), "/")

  ## MEANS ------
  mns_p4 <- ##___2 signal dim  | 2 noise dim
    list("cl a" = c(-1, -1,    0, 0),
         "cl b" = c( 1, -1,    0, 0),
         "cl c" = c(-1,  1,    0, 0))
  mns_p6 <- ##___3 signal dim     | 3 noise dim
    list("cl a" = c(-1, -1, -1,   0, 0, 0),
         "cl b" = c( 1, -1, -1,   0, 0, 0),
         "cl c" = c(-1,  1,  1,   0, 0, 0),
         "cl d" = c(1,   1,  1,   0, 0, 0))
  
  ## COVARIANCES -----
  sd <- 1 
  ### DEFINE COMMON COVARIANCES
  #### FOR p = 4 
  cov_circ_p4 <- matrix(c(sd, 0,  0,  0,
                          0,  sd, 0,  0,
                          0,  0,  sd, 0,
                          0,  0,  0,  sd),
                        ncol = 4, byrow = TRUE)
  cov_elipse_pos_p4 <- matrix(c(sd, .9, 0,  0,
                                .9, sd, 0,  0,
                                0,  0,  sd, 0,
                                0,  0,  0,  sd),
                              ncol = 4, byrow = TRUE)
  cov_elipse_neg_p4 <- matrix(c(sd, -.9, 0,  0,
                                -.9, sd, 0,  0,
                                0,   0,  sd, 0,
                                0,   0,  0,  sd), 
                              ncol = 4, byrow = TRUE)
  #### FOR p = 6
  cov_circ_p6 <- matrix(c(sd, 0,  0,  0,  0,  0, 
                          0,  sd, 0,  0,  0,  0, 
                          0,  0,  sd, 0,  0,  0,
                          0,  0,  0,  sd, 0,  0,
                          0,  0,  0,  0,  sd, 0,
                          0,  0,  0,  0,  0,  sd),
                        ncol = 6, byrow = TRUE)
  cov_elipse_pos_p6 <- matrix(c(sd, .9, .9, 0,  0,  0,
                                .9, sd, .9, 0,  0,  0,
                                .9, .9, sd, 0,  0,  0,
                                0,  0,  0,  sd, 0,  0,
                                0,  0,  0,  0,  sd, 0,
                                0,  0,  0,  0,  0,  sd),
                              ncol = 6, byrow = TRUE)
  .a <- -.4
  .b <- -.4
  .c <- -.4
  cov_elipse_neg_p6 <- matrix(c(sd, .a, .b, 0,  0,  0,
                                .a, sd, .c, 0,  0,  0,
                                .b, .c, sd, 0,  0,  0,
                                0,  0,  0,  sd, 0,  0,
                                0,  0,  0,  0,  sd, 0,
                                0,  0,  0,  0,  0,  sd),
                              ncol = 6, byrow = TRUE)
  
  #### EEE covariances
  ##### p = 4
  covs_EEE_p4 <- list("cl a" = cov_circ_p4,
                      "cl b" = cov_circ_p4,
                      "cl c" = cov_circ_p4)
  covs_EEV_p4 <- list("cl a" = cov_elipse_pos_p4,
                      "cl b" = cov_elipse_neg_p4,
                      "cl c" = cov_elipse_pos_p4)
  covs_banana_p4 <- list("cl a" = .2 * cov_circ_p4,
                         "cl b" = .05 * cov_circ_p4, ## Apply banana_tform_mtvnorm() to cl_b after simulation
                         "cl c" = cov_elipse_pos_p4)
  ##### p = 6
  covs_EEE_p6 <- list("cl a" = cov_circ_p6,
                      "cl b" = cov_circ_p6,
                      "cl c" = cov_circ_p6,
                      "cl d" = cov_circ_p6)
  covs_EEV_p6 <- list("cl a" = cov_elipse_pos_p6,
                      "cl b" = cov_elipse_neg_p6,
                      "cl c" = cov_elipse_pos_p6,
                      "cl d" = cov_elipse_neg_p6)
  covs_banana_p6 <- list("cl a" = .2 * cov_circ_p6,
                         "cl b" = .05 * cov_circ_p6, ## Apply banana_tform_mtvnorm() to cl_b after simulation
                         "cl c" = cov_elipse_pos_p6,
                         "cl d" = cov_elipse_neg_p6)
  
  
  ## SIMULATIONS ------
  ### all simulations must be different; cannot use EEE_p4 <-, each EEE_p4 needs to be different
  #### p = 4
  this_sim <- function(means, sigmas){
    sim_mvtnorm_cl(means = means, sigmas = sigmas,
                   cl_obs = cl_obs, do_shuffle = TRUE)
  }
  EEE_p4_0_1      <<- this_sim(means = mns_p4, sigmas = covs_EEE_p4)
  EEE_p4_33_66    <<- this_sim(means = mns_p4, sigmas = covs_EEE_p4)
  EEE_p4_50_50    <<- this_sim(means = mns_p4, sigmas = covs_EEE_p4)
  EEV_p4_0_1      <<- this_sim(means = mns_p4, sigmas = covs_EEV_p4)
  EEV_p4_33_66    <<- this_sim(means = mns_p4, sigmas = covs_EEV_p4)
  EEV_p4_50_50    <<- this_sim(means = mns_p4, sigmas = covs_EEV_p4)
  banana_p4_0_1   <<- this_sim(means = mns_p4, sigmas = covs_banana_p4)
  banana_p4_33_66 <<- this_sim(means = mns_p4, sigmas = covs_banana_p4)
  banana_p4_50_50 <<- this_sim(means = mns_p4, sigmas = covs_banana_p4)
  #### p = 6
  EEE_p6_0_1      <<- this_sim(means = mns_p6, sigmas = covs_EEE_p6)
  EEE_p6_33_66    <<- this_sim(means = mns_p6, sigmas = covs_EEE_p6)
  EEE_p6_50_50    <<- this_sim(means = mns_p6, sigmas = covs_EEE_p6)
  EEV_p6_0_1      <<- this_sim(means = mns_p6, sigmas = covs_EEV_p6)
  EEV_p6_33_66    <<- this_sim(means = mns_p6, sigmas = covs_EEV_p6)
  EEV_p6_50_50    <<- this_sim(means = mns_p6, sigmas = covs_EEV_p6)
  banana_p6_0_1   <<- this_sim(means = mns_p6, sigmas = covs_banana_p6)
  banana_p6_33_66 <<- this_sim(means = mns_p6, sigmas = covs_banana_p6)
  banana_p6_50_50 <<- this_sim(means = mns_p6, sigmas = covs_banana_p6)
  
  ## _Apply banana_tform_mtvnorm() ----
  ind <- attr(banana_p4_0_1, "cluster") == "cl b"
  banana_p4_0_1[ind, ]   <<- banana_tform_mtvnorm(banana_p4_0_1[ind, ])
  ind <- attr(banana_p4_33_66, "cluster") == "cl b"
  banana_p4_0_1[ind, ]   <<- banana_tform_mtvnorm(banana_p4_33_66[ind, ])
  ind <- attr(banana_p4_50_50, "cluster") == "cl b"
  banana_p4_50_50[ind, ] <<- banana_tform_mtvnorm(banana_p4_50_50[ind, ])
  ind <- attr(banana_p6_0_1, "cluster") == "cl b"
  banana_p6_0_1[ind, ]   <<- banana_tform_mtvnorm(banana_p6_0_1[ind, ])
  ind <- attr(banana_p6_33_66, "cluster") == "cl b"
  banana_p6_33_66[ind, ] <<- banana_tform_mtvnorm(banana_p6_33_66[ind, ])
  ind <- attr(banana_p6_50_50, "cluster") == "cl b"
  banana_p6_50_50[ind, ] <<- banana_tform_mtvnorm(banana_p6_50_50[ind, ])
  
  ## _Apply rotate_mtvnorm() -----
  ### 0_10 has no rotation,
  ### 50_50 is sin(pi/4) = .707,
  ### 33_67 is sin(pi/6) and sin(pi/3), .5 and 0.866 respectively
  #### p = 4
  EEE_p4_33_66    <<- rotate_mtvnorm(EEE_p4_33_66   , ang = pi / 6)
  EEE_p4_50_50    <<- rotate_mtvnorm(EEE_p4_50_50   , ang = pi / 4)
  EEV_p4_33_66    <<- rotate_mtvnorm(EEV_p4_33_66   , ang = pi / 6)
  EEV_p4_50_50    <<- rotate_mtvnorm(EEV_p4_50_50   , ang = pi / 4)
  banana_p4_33_66 <<- rotate_mtvnorm(banana_p4_33_66, ang = pi / 6)
  banana_p4_50_50 <<- rotate_mtvnorm(banana_p4_50_50, ang = pi / 4)
  #### p = 6
  EEE_p6_33_66    <<- rotate_mtvnorm(EEE_p6_33_66   , ang = pi / 6)
  EEE_p6_50_50    <<- rotate_mtvnorm(EEE_p6_50_50   , ang = pi / 4)
  EEV_p6_33_66    <<- rotate_mtvnorm(EEV_p6_33_66   , ang = pi / 6)
  EEV_p6_50_50    <<- rotate_mtvnorm(EEV_p6_50_50   , ang = pi / 4)
  banana_p6_33_66 <<- rotate_mtvnorm(banana_p6_33_66, ang = pi / 6)
  banana_p6_50_50 <<- rotate_mtvnorm(banana_p6_50_50, ang = pi / 4)
  ##
  message("Assigned all simulations as a global variables, as '<model_dim_location>'. \n")
  
  ## _Save if needed ----
  if(do_save == TRUE){
    ## Using .rda. .rds not working b/c of long path? issue may be in the loading more than the saving.
    ## p = 4
    save(EEE_p4_0_1      , file = paste0(root, quote(EEE_p4_0_1     ), ".rda"))
    save(EEE_p4_33_66    , file = paste0(root, quote(EEE_p4_33_66   ), ".rda"))
    save(EEE_p4_50_50    , file = paste0(root, quote(EEE_p4_50_50   ), ".rda"))
    save(EEV_p4_0_1      , file = paste0(root, quote(EEV_p4_0_1     ), ".rda"))
    save(EEV_p4_33_66    , file = paste0(root, quote(EEV_p4_33_66   ), ".rda"))
    save(EEV_p4_50_50    , file = paste0(root, quote(EEV_p4_50_50   ), ".rda"))
    save(banana_p4_0_1   , file = paste0(root, quote(banana_p4_0_1  ), ".rda"))
    save(banana_p4_33_66 , file = paste0(root, quote(banana_p4_33_66), ".rda"))
    save(banana_p4_50_50 , file = paste0(root, quote(banana_p4_50_50), ".rda"))
    ## p = 6
    save(EEE_p6_0_1      , file = paste0(root, quote(EEE_p6_0_1     ), ".rda"))
    save(EEE_p6_33_66    , file = paste0(root, quote(EEE_p6_33_66   ), ".rda"))
    save(EEE_p6_50_50    , file = paste0(root, quote(EEE_p6_50_50   ), ".rda"))
    save(EEV_p6_0_1      , file = paste0(root, quote(EEV_p6_0_1     ), ".rda"))
    save(EEV_p6_33_66    , file = paste0(root, quote(EEV_p6_33_66   ), ".rda"))
    save(EEV_p6_50_50    , file = paste0(root, quote(EEV_p6_50_50   ), ".rda"))
    save(banana_p6_0_1   , file = paste0(root, quote(banana_p6_0_1  ), ".rda"))
    save(banana_p6_33_66 , file = paste0(root, quote(banana_p6_33_66), ".rda"))
    save(banana_p6_50_50 , file = paste0(root, quote(banana_p6_50_50), ".rda"))
    message(paste0("Saved all simulations to ", root, " as '<model_dim_location>.rda'. Use load('my.rda') bring obj into env. \n"))
  }
}

#' Saves off grand tour paths for the evaluation simulations
#' @examples 
#' tpath_user_study(do_save = TRUE)
tpath_user_study <- function(do_save = FALSE){
  ## Initialize
  require("tourr")
  root    <- paste0(here::here("apps/data"), "/")
  in_nms  <- c("EEE_p4_0_1", "EEE_p4_33_66", "EEE_p4_50_50",
               "EEV_p4_0_1", "EEV_p4_33_66", "EEV_p4_50_50",
               "banana_p4_0_1", "banana_p4_33_66", "banana_p4_50_50",
               "EEE_p6_0_1", "EEE_p6_33_66", "EEE_p6_50_50",
               "EEV_p6_0_1", "EEV_p6_33_66", "EEV_p6_50_50",
               "banana_p6_0_1", "banana_p6_33_66", "banana_p6_50_50")
  in_fps  <- paste0(root, in_nms, ".rda")
  out_nms <- paste0("tpath_", in_nms)
  out_fps <- paste0(root, out_nms, ".rda")
  
  ## Load simulations, create tour paths
  for (i in 1:length(in_fps)) {
    load(in_fps[i], envir = globalenv())
    dat <- as.matrix(get(in_nms[i])) ## Numeric data only, as matrix
    tpath <- save_history(data = dat, tour_path = grand_tour(), max_bases = 8)
    
    assign(out_nms[i], tpath, envir = globalenv())
  }
  message("Assigned all grand tour paths as a global variables, as 'tpath_<factor_model>'. \n")
  browser()
  
  ## Save if needed
  if(do_save == TRUE){
    ## p = 4
    save(tpath_EEE_p4_0_1   , file = paste0(root, quote(tpath_EEE_p4_0_1  ), ".rda"))
    save(tpath_EEE_p4_33_66 , file = paste0(root, quote(tpath_EEE_p4_33_66), ".rda"))
    save(tpath_EEE_p4_50_50 , file = paste0(root, quote(tpath_EEE_p4_50_50), ".rda"))
    save(tpath_EEV_p4_0_1   , file = paste0(root, quote(tpath_EEV_p4_0_1  ), ".rda"))
    save(tpath_EEV_p4_33_66 , file = paste0(root, quote(tpath_EEV_p4_33_66), ".rda"))
    save(tpath_EEV_p4_50_50 , file = paste0(root, quote(tpath_EEV_p4_50_50), ".rda"))
    save(tpath_banana_p4_0_1   , file = paste0(root, quote(tpath_banana_p4_0_1  ), ".rda"))
    save(tpath_banana_p4_33_66 , file = paste0(root, quote(tpath_banana_p4_33_66), ".rda"))
    save(tpath_banana_p4_50_50 , file = paste0(root, quote(tpath_banana_p4_50_50), ".rda"))
    ## p = 6
    save(tpath_EEE_p6_0_1   , file = paste0(root, quote(tpath_EEE_p6_0_1  ), ".rda"))
    save(tpath_EEE_p6_33_66 , file = paste0(root, quote(tpath_EEE_p6_33_66), ".rda"))
    save(tpath_EEE_p6_50_50 , file = paste0(root, quote(tpath_EEE_p6_50_50), ".rda"))
    save(tpath_EEV_p6_0_1   , file = paste0(root, quote(tpath_EEV_p6_0_1  ), ".rda"))
    save(tpath_EEV_p6_33_66 , file = paste0(root, quote(tpath_EEV_p6_33_66), ".rda"))
    save(tpath_EEV_p6_50_50 , file = paste0(root, quote(tpath_EEV_p6_50_50), ".rda"))
    save(tpath_banana_p6_0_1   , file = paste0(root, quote(tpath_banana_p6_0_1  ), ".rda"))
    save(tpath_banana_p6_33_66 , file = paste0(root, quote(tpath_banana_p6_33_66), ".rda"))
    save(tpath_banana_p6_50_50 , file = paste0(root, quote(tpath_banana_p6_50_50), ".rda"))
    message(paste0("Saved all grand tour paths to ", root, " as 'tpath_<factor_model>.rda'. Use load('my.rda') bring obj into env. \n"))
  }
}

