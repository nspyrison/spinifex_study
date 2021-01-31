# OLD source("./R/sim_tidyverse.r")) ## For banana_tform() and rotate()

## NOTE:  rotate_mtvnorm() & banana_tform_mtvnorm()
#### have been dissolved into sim_mvtnorm_cl(), below.

#' Creates a data frame of multivariate data with clusters via mvnorm::rmvnorm().
#'
#' @param means List, each element is a p-length vectors, the variable means 
#' of this cluster.
#' @param sigmas List, each element is a square (p, p) matrix, the 
#' variance-covariance matrix for this cluster. If any matrix is not 
#' positive definite, it will be coerced with `lqmm::make.positive.definite()`
#' @param cl_obs List, of number of observations within each cluster.
#' @param do_bananatize Boolean specifying if cluster b should be bananatized 
#' before rotation. Bananatizing moves 4/5th of the samples away in a 
#' (loosely ~quin-nomial) 2D 'V' shape.
#' @param ang The angle (in radians) to rotate the V1 and V4. Defaults to 0, 
#' no rotation. (hardcoded to V1/V4 for 3cl in 4-dim, and 4cl in 6-dim).

#' @param do_shuffle Boolean specifying if order sampling/shuffling should be 
#' applied to the rows and columns, such that cluster rows are not all together
#' and signal columns are not in the same order.
sim_mvtnorm_cl <- function(means,  ## Required
                           sigmas, ## Required
                           cl_obs = cl_obs,
                           do_bananatize = FALSE,
                           ang = 0,
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
  ## Initialize cluster
  cluster <- factor(paste0("cl ", rep(letters[1:k], unlist(cl_obs))))
  
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
  
  ## Bananatize cluster b if needed, before rotation and shuffling
  if(do_bananatize == TRUE){
    b_df <- df_sim[cluster == "cl b", ]
    fif <- floor(nrow(b_df) / 5) ## length of 1/5 of the data
    fif1 <- 1:fif
    fif2 <- fif1 + fif
    fif3 <- fif2 + fif
    fif4 <- fif3 + fif
    ## f5, last fifth of the data stays as is. 
    
    ## Apply transform
    b_df[fif1, 1] <- b_df[fif1, 1] - 0.5
    b_df[fif1, 2] <- b_df[fif1, 2] + 0.5
    b_df[fif2, 1] <- b_df[fif2, 1] - 0.5
    b_df[fif2, 2] <- b_df[fif2, 2] - 0.5
    b_df[fif3, 1] <- b_df[fif3, 1] - 1
    b_df[fif3, 2] <- b_df[fif3, 2] + 1
    b_df[fif4, 1] <- b_df[fif4, 1] - 1
    b_df[fif4, 2] <- b_df[fif4, 2] - 1
    
    df_sim[cluster == "cl b", ] <- b_df
  }
  
  ## Rotate V1/V4 if needed, before shuffling
  if(length(ang) == 1 & ang != 0){
    colnames(df_sim) <- paste0("V", 1:p) ## temporary colnames for rotating V1/V4
    c <- cos(ang)
    s <- sin(ang)
    df_sim <- dplyr::mutate(as.data.frame(df_sim),
                            V1 = c * V1 + -s * V4,
                            V4 = s * V1 +  c * V4)
  }
  
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
  
  ## Find answer for ab mean differences after shuffle, if needed.
  sampled_means_a <- colMeans(df_sim[cluster == "cl a", ])
  sampled_means_b <- colMeans(df_sim[cluster == "cl b", ])
  var_mean_diff_ab <- abs(sampled_means_b - sampled_means_a)
  
  ## Row/col names, after shuffle if needed
  rownames(df_sim) <- 1:nrow(df_sim)
  colnames(df_sim) <- paste0("V", 1:ncol(df_sim))
  df_sim <- as.data.frame(df_sim)
  
  ## Capture original arguments and this call, for reproducibility.
  args <- list(means = means, sigmas = sigmas, cl_obs = cl_obs, 
                      do_shuffle = do_shuffle)
  this_call <- call("sim_pDim_kCl", args)
  ## Record attributes
  attr(df_sim, "cluster") <- cluster   ## Cluster levels
  attr(df_sim, "var_mean_diff_ab") <- var_mean_diff_ab ## The answer for the study, before rotation
  attr(df_sim, "args")    <- args      ## List of args
  attr(df_sim, "call")    <- this_call ## Stored call, use eval(attr(sim, "call")) to reproduce
  
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
  root <- paste0("./apps_supplementary/data/")

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
  .a <- .9
  .b <- -.9
  .c <- -.9
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

  for(i in 1:3){
    ## Training t1:3, for study
    assign(paste0("EEE_p4_0_1_t", i),
           sim_mvtnorm_cl(means = mns_p4, sigmas = covs_EEE_p4,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = 0),
           envir = globalenv())
    #### p = 4
    assign(paste0("EEE_p4_0_1_rep", i),
           sim_mvtnorm_cl(means = mns_p4, sigmas = covs_EEE_p4,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = 0),
           envir = globalenv())
    assign(paste0("EEE_p4_33_66_rep", i),
           sim_mvtnorm_cl(means = mns_p4, sigmas = covs_EEE_p4,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 6),
           envir = globalenv())
    assign(paste0("EEE_p4_50_50_rep", i),
           sim_mvtnorm_cl(means = mns_p4, sigmas = covs_EEE_p4,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 4),
           envir = globalenv())
    assign(paste0("EEV_p4_0_1_rep", i),
           sim_mvtnorm_cl(means = mns_p4, sigmas = covs_EEV_p4,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = 0),
           envir = globalenv())
    assign(paste0("EEV_p4_33_66_rep", i),
           sim_mvtnorm_cl(means = mns_p4, sigmas = covs_EEV_p4,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 6),
           envir = globalenv())
    assign(paste0("EEV_p4_50_50_rep", i),
           sim_mvtnorm_cl(means = mns_p4, sigmas = covs_EEV_p4,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 4),
           envir = globalenv())
    assign(paste0("banana_p4_0_1_rep", i),
           sim_mvtnorm_cl(means = mns_p4, sigmas = covs_banana_p4,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = 0, do_bananatize = TRUE),
           envir = globalenv())
    assign(paste0("banana_p4_33_66_rep", i),
           sim_mvtnorm_cl(means = mns_p4, sigmas = covs_banana_p4,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 6, do_bananatize = TRUE),
           envir = globalenv())
    assign(paste0("banana_p4_50_50_rep", i),
           sim_mvtnorm_cl(means = mns_p4, sigmas = covs_banana_p4,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 4, do_bananatize = TRUE),
           envir = globalenv())
  ## CONTINUE TO ADD ASSIGN...
    #### p = 6
    assign(paste0("EEE_p6_0_1_rep", i),
           sim_mvtnorm_cl(means = mns_p6, sigmas = covs_EEE_p6,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = 0),
           envir = globalenv())
    assign(paste0("EEE_p6_33_66_rep", i),
           sim_mvtnorm_cl(means = mns_p6, sigmas = covs_EEE_p6,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 6),
           envir = globalenv())
    assign(paste0("EEE_p6_50_50_rep", i),
           sim_mvtnorm_cl(means = mns_p6, sigmas = covs_EEE_p6,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 4),
           envir = globalenv())
    assign(paste0("EEV_p6_0_1_rep", i),
           sim_mvtnorm_cl(means = mns_p6, sigmas = covs_EEV_p6,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = 0),
           envir = globalenv())
    assign(paste0("EEV_p6_33_66_rep", i),
           sim_mvtnorm_cl(means = mns_p6, sigmas = covs_EEV_p6,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 6),
           envir = globalenv())
    assign(paste0("EEV_p6_50_50_rep", i),
           sim_mvtnorm_cl(means = mns_p6, sigmas = covs_EEV_p6,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 4),
           envir = globalenv())
    assign(paste0("banana_p6_0_1_rep", i),
           sim_mvtnorm_cl(means = mns_p6, sigmas = covs_banana_p6,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = 0, do_bananatize = TRUE),
           envir = globalenv())
    assign(paste0("banana_p6_33_66_rep", i),
           sim_mvtnorm_cl(means = mns_p6, sigmas = covs_banana_p6,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 6, do_bananatize = TRUE),
           envir = globalenv())
    assign(paste0("banana_p6_50_50_rep", i),
           sim_mvtnorm_cl(means = mns_p6, sigmas = covs_banana_p6,
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = pi / 4, do_bananatize = TRUE),
           envir = globalenv())
  }
  ## Training t4:5, for video
  .nms <- c("EEE_p4_0_1_t4", "EEE_p6_50_50_t5")
  .num <- 4:5
  .mns <- list(mns_p4, mns_p6)
  .sig <- list(covs_EEE_p4, covs_EEE_p6)
  .ang <- c(0, pi / 4)
  for(i in 1:length(.num)){
    assign(.nms[i],
           sim_mvtnorm_cl(means = .mns[[i]], sigmas = .sig[[i]],
                          cl_obs = cl_obs, do_shuffle = TRUE,
                          ang = .ang[[i]]),
           envir = globalenv())
  }
  ##
  message("Assigned all simulations as a global variables, as '<model_dim_location>'. \n")
  
  ## _Save if needed ----
  if(do_save == TRUE){
    ## Using .rda. .rds not working b/c of long path? issue may be in the loading more than the saving.
    
    #### Training
    save(EEE_p4_0_1_t1, file = paste0(root, quote(EEE_p4_0_1_t1), ".rda"))
    save(EEE_p4_0_1_t2, file = paste0(root, quote(EEE_p4_0_1_t2), ".rda"))
    save(EEE_p4_0_1_t3, file = paste0(root, quote(EEE_p4_0_1_t3), ".rda"))
    save(EEE_p4_0_1_t4, file = paste0(root, quote(EEE_p4_0_1_t4), ".rda")) ## For video p=4
    save(EEE_p6_50_50_t5, file = paste0(root, quote(EEE_p6_50_50_t5), ".rda")) ## For video p=6
    
    #### rep 1
    ## p = 4
    save(EEE_p4_0_1_rep1     , file = paste0(root, quote(EEE_p4_0_1_rep1     ), ".rda"))
    save(EEE_p4_33_66_rep1   , file = paste0(root, quote(EEE_p4_33_66_rep1   ), ".rda"))
    save(EEE_p4_50_50_rep1   , file = paste0(root, quote(EEE_p4_50_50_rep1   ), ".rda"))
    save(EEV_p4_0_1_rep1     , file = paste0(root, quote(EEV_p4_0_1_rep1     ), ".rda"))
    save(EEV_p4_33_66_rep1   , file = paste0(root, quote(EEV_p4_33_66_rep1   ), ".rda"))
    save(EEV_p4_50_50_rep1   , file = paste0(root, quote(EEV_p4_50_50_rep1   ), ".rda"))
    save(banana_p4_0_1_rep1  , file = paste0(root, quote(banana_p4_0_1_rep1  ), ".rda"))
    save(banana_p4_33_66_rep1, file = paste0(root, quote(banana_p4_33_66_rep1), ".rda"))
    save(banana_p4_50_50_rep1, file = paste0(root, quote(banana_p4_50_50_rep1), ".rda"))
    ## p = 6
    save(EEE_p6_0_1_rep1     , file = paste0(root, quote(EEE_p6_0_1_rep1     ), ".rda"))
    save(EEE_p6_33_66_rep1   , file = paste0(root, quote(EEE_p6_33_66_rep1   ), ".rda"))
    save(EEE_p6_50_50_rep1   , file = paste0(root, quote(EEE_p6_50_50_rep1   ), ".rda"))
    save(EEV_p6_0_1_rep1     , file = paste0(root, quote(EEV_p6_0_1_rep1     ), ".rda"))
    save(EEV_p6_33_66_rep1   , file = paste0(root, quote(EEV_p6_33_66_rep1   ), ".rda"))
    save(EEV_p6_50_50_rep1   , file = paste0(root, quote(EEV_p6_50_50_rep1   ), ".rda"))
    save(banana_p6_0_1_rep1  , file = paste0(root, quote(banana_p6_0_1_rep1  ), ".rda"))
    save(banana_p6_33_66_rep1, file = paste0(root, quote(banana_p6_33_66_rep1), ".rda"))
    save(banana_p6_50_50_rep1, file = paste0(root, quote(banana_p6_50_50_rep1), ".rda"))
    #### rep 2
    ## p = 4
    save(EEE_p4_0_1_rep2     , file = paste0(root, quote(EEE_p4_0_1_rep2     ), ".rda"))
    save(EEE_p4_33_66_rep2   , file = paste0(root, quote(EEE_p4_33_66_rep2   ), ".rda"))
    save(EEE_p4_50_50_rep2   , file = paste0(root, quote(EEE_p4_50_50_rep2   ), ".rda"))
    save(EEV_p4_0_1_rep2     , file = paste0(root, quote(EEV_p4_0_1_rep2     ), ".rda"))
    save(EEV_p4_33_66_rep2   , file = paste0(root, quote(EEV_p4_33_66_rep2   ), ".rda"))
    save(EEV_p4_50_50_rep2   , file = paste0(root, quote(EEV_p4_50_50_rep2   ), ".rda"))
    save(banana_p4_0_1_rep2  , file = paste0(root, quote(banana_p4_0_1_rep2  ), ".rda"))
    save(banana_p4_33_66_rep2, file = paste0(root, quote(banana_p4_33_66_rep2), ".rda"))
    save(banana_p4_50_50_rep2, file = paste0(root, quote(banana_p4_50_50_rep2), ".rda"))
    ## p = 6
    save(EEE_p6_0_1_rep2     , file = paste0(root, quote(EEE_p6_0_1_rep2     ), ".rda"))
    save(EEE_p6_33_66_rep2   , file = paste0(root, quote(EEE_p6_33_66_rep2   ), ".rda"))
    save(EEE_p6_50_50_rep2   , file = paste0(root, quote(EEE_p6_50_50_rep2   ), ".rda"))
    save(EEV_p6_0_1_rep2     , file = paste0(root, quote(EEV_p6_0_1_rep2     ), ".rda"))
    save(EEV_p6_33_66_rep2   , file = paste0(root, quote(EEV_p6_33_66_rep2   ), ".rda"))
    save(EEV_p6_50_50_rep2   , file = paste0(root, quote(EEV_p6_50_50_rep2   ), ".rda"))
    save(banana_p6_0_1_rep2  , file = paste0(root, quote(banana_p6_0_1_rep2  ), ".rda"))
    save(banana_p6_33_66_rep2, file = paste0(root, quote(banana_p6_33_66_rep2), ".rda"))
    save(banana_p6_50_50_rep2, file = paste0(root, quote(banana_p6_50_50_rep2), ".rda"))
    #### rep 3
    ## p = 4
    save(EEE_p4_0_1_rep3     , file = paste0(root, quote(EEE_p4_0_1_rep3     ), ".rda"))
    save(EEE_p4_33_66_rep3   , file = paste0(root, quote(EEE_p4_33_66_rep3   ), ".rda"))
    save(EEE_p4_50_50_rep3   , file = paste0(root, quote(EEE_p4_50_50_rep3   ), ".rda"))
    save(EEV_p4_0_1_rep3     , file = paste0(root, quote(EEV_p4_0_1_rep3     ), ".rda"))
    save(EEV_p4_33_66_rep3   , file = paste0(root, quote(EEV_p4_33_66_rep3   ), ".rda"))
    save(EEV_p4_50_50_rep3   , file = paste0(root, quote(EEV_p4_50_50_rep3   ), ".rda"))
    save(banana_p4_0_1_rep3  , file = paste0(root, quote(banana_p4_0_1_rep3  ), ".rda"))
    save(banana_p4_33_66_rep3, file = paste0(root, quote(banana_p4_33_66_rep3), ".rda"))
    save(banana_p4_50_50_rep3, file = paste0(root, quote(banana_p4_50_50_rep3), ".rda"))
    ## p = 6
    save(EEE_p6_0_1_rep3     , file = paste0(root, quote(EEE_p6_0_1_rep3     ), ".rda"))
    save(EEE_p6_33_66_rep3   , file = paste0(root, quote(EEE_p6_33_66_rep3   ), ".rda"))
    save(EEE_p6_50_50_rep3   , file = paste0(root, quote(EEE_p6_50_50_rep3   ), ".rda"))
    save(EEV_p6_0_1_rep3     , file = paste0(root, quote(EEV_p6_0_1_rep3     ), ".rda"))
    save(EEV_p6_33_66_rep3   , file = paste0(root, quote(EEV_p6_33_66_rep3   ), ".rda"))
    save(EEV_p6_50_50_rep3   , file = paste0(root, quote(EEV_p6_50_50_rep3   ), ".rda"))
    save(banana_p6_0_1_rep3  , file = paste0(root, quote(banana_p6_0_1_rep3  ), ".rda"))
    save(banana_p6_33_66_rep3, file = paste0(root, quote(banana_p6_33_66_rep3), ".rda"))
    save(banana_p6_50_50_rep3, file = paste0(root, quote(banana_p6_50_50_rep3), ".rda"))
    message(paste0("Saved all simulations to ", root, " as '<model_dim_location>.rda'. Use load('my.rda') bring obj into env. \n"))
  }
}

#' Saves off grand tour paths for the evaluation simulations
#' @examples 
#' tpath_user_study(do_save = TRUE)
tpath_user_study <- function(do_save = FALSE){
  ## Initialize
  require("tourr")
  root    <- paste0("./apps_supplementary/data/")
  in_nms  <- c("EEE_p4_0_1_t1", "EEE_p4_0_1_rep1", "EEE_p6_0_1_rep1")
  in_fps  <- paste0(root, in_nms, ".rda")
  out_nms <- paste0("tpath_", c("p4_t", "p4", "p6"))
  out_fps <- paste0(root, out_nms, ".rda")
  
  ## Load simulations, create tour paths
  for (i in 1:length(in_fps)) {
    load(in_fps[i], envir = globalenv())
    dat <- as.matrix(get(in_nms[i])) ## Numeric data only, as matrix
    tpath <- save_history(data = dat, tour_path = grand_tour(), max_bases = 8)
    
    assign(out_nms[i], tpath, envir = globalenv())
  }
  message("Assigned all grand tour paths as a global variables, as 'tpath_<factor_model>'. \n")
  
  ## Save if needed
  if(do_save == TRUE){
    save(tpath_p4_t, file = paste0(root, quote(tpath_p4_t  ), ".rda"))
    save(tpath_p4,   file = paste0(root, quote(tpath_p4),     ".rda"))
    save(tpath_p6,   file = paste0(root, quote(tpath_p6),     ".rda"))
    message(paste0("Saved all grand tour paths to ", root, " as 'tpath_<factor_model>.rda'. Use load('my.rda') bring obj into env. \n"))
  }
}

