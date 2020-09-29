source(here::here("R/sim_tidyverse.r")) ## For banana_tform() and rotate()


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
  df_sim <- as.data.frame(df_sim)
  df_sim <- apply(df_sim, 2, function(c)(c - mean(c)) / sd(c)) ## Standardize by column mean, sd.
  
  ## Init class
  class <- factor(paste0("cl ", rep(letters[1:k], unlist(cl_obs))))
  ## Reorder rows and columns if needed
  if(do_shuffle == TRUE){
    row_ord <- sample(1:nrow(df_sim))
    col_ord <- sample(1:p)
    
    ## Apply the shuffle reordering
    df_sim <- df_sim[row_ord, col_ord]
    class  <- class[row_ord]
    for (i in 1:k){
      means[[i]]  <- means[[i]][col_ord]
      sigmas[[i]] <- sigmas[[i]][col_ord, col_ord]
    }
  }
  
  ## Row/col names, after shuffle if required
  rownames(df_sim) <- 1:nrow(df_sim)
  colnames(df_sim) <- paste0("V", 1:ncol(df_sim))
  
  ## Capture attibutes
  args <- list(means = means, sigmas = sigmas, cl_obs = cl_obs, 
                      do_shuffle = do_shuffle)
  cl <- call("sim_pDim_kCl", args)
  ## Record attributes
  attr(df_sim, "cluster") <- class ## Cluster levels
  attr(df_sim, "args")    <- args  ## List of args
  attr(df_sim, "call")    <- cl    ## Stored call, use eval(attr(sim, "call")) to reproduce
  
  return(df_sim)
}



#' Hard-coded wrapper function that uses mvnorm::rmvnorm() to simulate 3 
#' simulation factors by 3 (mclust 5 paper like) models. These 9 simulations
#' are assigned to global variables and saved as .rda files for use in the 
#' {shiny} app for the user study.
#'
#' @param cl_obs List, of number of observations within each cluster.
#' @examples
#' sim_user_study(cl_obs = 140, do_save = TRUE)
sim_user_study <- function(
  ## fixed at p = 4, k_cl = 3, mn_sz = 2, var_sz =1, cor_sz = .9, p_sig 2, (p_noise = 2)
  cl_obs = 200,
  do_save = FLASE 
){
  ## HARD CODE SEED!!!
  set.seed(123)
  ## Initializae
  p      <- 4
  k_cl   <- 3
  factors <- c("baseLn", "corNoise", "mnComb")
  models <- c("EEE", "EEV", "banana")
  obj_nms <- c(t(outer(factors, paste0("_", models), FUN = paste0)))
  cov_nms <- paste0("covs_", obj_nms)

  ## Assign cluster means 
  mns <- ##___2 signal dim  | 2 noise dim
    list("cl a" = c(-1, -1,   0, 0),
         "cl b" = c( 1, -1,   0, 0),
         "cl c" = c(-1,  1,   0, 0))
  
  ## COVARIANCES
  sd <- 1 ## .07 IS ROUGHLY ~ MCLUST5 paper with unit means.
  #### DEFINE COMMON COVARIANCES
  cov_circ <- matrix(c(sd,  0,   0,   0,
                       0,   sd,  0,   0,
                       0,   0,   sd,  0,
                       0,   0,   0,   sd),
                     ncol = 4, byrow = TRUE)
  cov_elipse_pos <- matrix(c(sd,  .9,  0,   0,
                             .9,  sd,  0,   0,
                             0,   0,   sd,  0,
                             0,   0,   0,   sd),
                           ncol = 4, byrow = TRUE)
  cov_elipse_neg <- matrix(c(sd,  -.9, 0,   0,
                             -.9, sd,  0,   0,
                             0,   0,  sd,   0,
                             0,   0,   0,   sd), 
                           ncol = 4, byrow = TRUE)
  cov_circ_corNoise <- matrix(c(sd,  0,   0,   0,
                                0,   sd,  0,   0,
                                0,   0,   sd,  .8,
                                0,   0,   .8,   sd),
                              ncol = 4, byrow = TRUE)
  cov_elipse_pos_corNoise <- matrix(c(sd,  .9,  0,   0,
                                      .9,  sd,  0,   0,
                                      0,   0,   sd,  .8,
                                      0,   0,   .8,  sd),
                                    ncol = 4, byrow = TRUE)
  cov_elipse_neg_corNoise <- matrix(c(sd,  -.9, 0,   0,
                                      -.9, sd,  0,   0,
                                      0,   0,   sd,  .8,
                                      0,   0,   .8,  sd),
                                    ncol = 4, byrow = TRUE)
  #### BaseLn covariances
  covs_baseLn_EEE    <- covs_mnComb_EEE    <- list("cl a" = cov_elipse_pos,
                                                   "cl b" = cov_elipse_pos,
                                                   "cl c" = cov_elipse_pos)
  covs_baseLn_EEV    <- covs_mnComb_EEV    <- list("cl a" = cov_elipse_pos,
                                                   "cl b" = cov_elipse_neg,
                                                   "cl c" = cov_elipse_pos)
  covs_baseLn_banana <- covs_mnComb_banana <- list("cl a" = .2 * cov_circ,
                                                   "cl b" = .05 * cov_circ, ## Apply banana_tform() to cl_b after simulation
                                                   "cl c" = cov_elipse_neg)
  #### corNoise covariances
  covs_corNoise_EEE    <- list("cl a" = cov_elipse_pos_corNoise,
                               "cl b" = cov_elipse_pos_corNoise,
                               "cl c" = cov_elipse_pos_corNoise)
  covs_corNoise_EEV    <- list("cl a" = cov_elipse_pos_corNoise,
                               "cl b" = cov_elipse_neg_corNoise,
                               "cl c" = cov_elipse_pos_corNoise)
  covs_corNoise_banana <- list("cl a" = .2 * cov_circ_corNoise,
                               "cl b" = .05 * cov_circ_corNoise,  ## Apply banana_tform() to cl_b after simulation
                               "cl c" = cov_elipse_neg)
  #### mnComb covariances assigned with BaseLn covariances
  
  
  ## Create simulations, assigning to global_env
  for(i in 1:length(obj_nms)){
    this_sim <- sim_mvtnorm_cl(means = mns, sigmas = get(cov_nms[i]),
                               cl_obs = cl_obs, do_shuffle = TRUE)
    cl <- attr(this_sim, "cluster")
    ret <- data.frame(cl, this_sim)
    assign(x = obj_nms[i], value = ret, envir = globalenv())
  }
  print("Assigned all simulations as a global variables, as '<factor_model>'.")
  
  ## Apply banana_tform()
  cl_b_rows <- baseLn_EEE$cl == "cl b"
  baseLn_banana[cl_b_rows, ]   <- banana_tform(baseLn_banana[cl_b_rows, ])
  corNoise_banana[cl_b_rows, ] <- banana_tform(corNoise_banana[cl_b_rows, ])
  mnComb_banana[cl_b_rows, ]   <- banana_tform(mnComb_banana[cl_b_rows, ])
  ## Apply rotate()
  mnComb_EEE    <- rotate(mnComb_EEE)
  mnComb_EEV    <- rotate(mnComb_EEV)
  mnComb_banana <- rotate(mnComb_banana)
  
  
  ## Save within function
  if(do_save == TRUE){
    root <- here::here("apps/data/")
    save(baseLn_EEE     , file = paste0(root, "/", obj_nms[1], ".rda"))
    save(baseLn_EEV     , file = paste0(root, "/", obj_nms[2], ".rda"))
    save(baseLn_banana  , file = paste0(root, "/", obj_nms[3], ".rda"))
    save(corNoise_EEE   , file = paste0(root, "/", obj_nms[4], ".rda"))
    save(corNoise_EEV   , file = paste0(root, "/", obj_nms[5], ".rda"))
    save(corNoise_banana, file = paste0(root, "/", obj_nms[6], ".rda"))
    save(mnComb_EEE     , file = paste0(root, "/", obj_nms[7], ".rda"))
    save(mnComb_EEV     , file = paste0(root, "/", obj_nms[8], ".rda"))
    save(mnComb_banana  , file = paste0(root, "/", obj_nms[9], ".rda"))
  }
  print(paste0("Save all simulations to ", root, " as '<factor_model>.rda'."))
}
