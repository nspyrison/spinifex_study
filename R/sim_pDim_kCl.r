#' Creates a data frame containing clusters of multivariate data 
#'
#' @param means List, each element is a p-length vectors, the variable means 
#' of this cluster.
#' @param sigmas List, each element is a square (p, p) matrix, the 
#' variance-covariance matrix for this cluster. If any matrix is not 
#' positive definite, it will be coerced with `lqmm::make.positive.definite()`
#' @param n_points List, each element is a single number of the points to sample 
#' from this cluster.
#' @param method String specifying the matrix decomposition used find the 
#' matrix root of `sigmas`. Expects, "eigen", the default, "svd", or "chol". 
#' Also see `?mvtnorm::rmvnorm()`.
#' @param do_shuffle Boolean specifying if order resampling should be applied 
#' to the rows and columns, Such that cluster rows are not all together and 
#' signal columns are not in the same order.
#' @examples 
#' mns <- list(c(10, 3, rep(0, 3)), c(2, 1, rep(0, 3)))
#' covs <- list(diag(5), diag(5))
#' sim_pDim_kCl(means = mns, sigmas = covs)
#' 
#' sim_pDim_kCl(means = mns, sigmas = covs, n_points = list(200, 50),
#'              method = "svd", do_shuffle = FALSE)

sim_pDim_kCl <- function(means, 
                         sigmas,
                         n_points = rep(list(100), length(means)),
                         method = c("eigen", "svd", "chol"),
                         do_shuffle = TRUE
) {
  means <- as.list(means)
  sigmas <- as.list(sigmas)
  method <- match.arg(method)
  n_points <- as.list(n_points)
  p <- length(means[[1]])
  k <- length(means)
  ## means and covariances are both of length k, clusters
  stopifnot(all(k == c(length(means), length(sigmas)))) 
  ## elements of means and elements covariances have length, rows/cols p, number of numeric variables.
  stopifnot(all(p == c(length(means[[1]]), nrow(sigmas[[1]]), ncol(sigmas[[1]]))))
  require("mvtnorm")
  require("lqmm")
  
  ## Create each cluster
  df_sim <- NULL
  sim_means <- list()
  sim_sigmas <- list()
  for (i in 1:k) {
    ## Set sample size and partician sample if complex shape
    .n <- n_points[[i]]
    .mn <- means[[i]]
    .cov <- as.matrix(sigmas[[i]])
    
    ## Check if this sigma is positive definite.
    if(lqmm::is.positive.definite(.cov) == FALSE){
      warning(paste0("sigmas[[", i, "]] wasn't a positive definite matrix. Applied lqmm::make.positive.definite()."))
      .cov <- lqmm::make.positive.definite(.cov)
    }
    if (isSymmetric.matrix(.cov) == FALSE) stop(paste0("sigma[[", i, "]] is not a symetric matrix, all covariance metrices must be symetric and positive definate."))
    
    ## Sample and store outputs
    .k <- mvtnorm::rmvnorm(n = .n, mean = .mn, sigma = .cov, method = method)
    df_sim <- rbind(df_sim, .k)
    sim_means[[i]] <- as.vector(colMeans(.k))
    sim_sigmas[[i]] <- cov(.k)
  }
  df_sim <- as.data.frame(df_sim)
  
  ## Capture input args for attributed before anything could be reshuffled.
  input_args <- list(means = means, sigmas = sigmas, n_points = n_points, 
                     method = method, do_shuffle = do_shuffle)
  cl_lvl <- paste0("cl ", rep(letters[1:k], unlist(n_points)))
  
  ## Reorder rows and columns if needed
  if(do_shuffle == TRUE) {
    row_ord <- sample(1:nrow(df_sim))
    col_ord <- sample(1:p)
    
    ## Apply the shuffle reordering
    df_sim <- df_sim[row_ord, col_ord]
    cl_lvl <- cl_lvl[row_ord]
    for (i in 1:k){
      sim_means[[i]] <- sim_means[[i]][col_ord]
      sim_sigmas[[i]] <- sim_sigmas[[i]][col_ord, col_ord]
    }
  }
  
  ## Row/col names, after shuffle if required
  rownames(df_sim) <- 1:nrow(df_sim)
  colnames(df_sim) <- paste0("V", 1:ncol(df_sim))
  
  ## Record attributes
  attr(df_sim, "cl_lvl")     <- cl_lvl     ## Cluster levels
  attr(df_sim, "input_args") <- input_args ## List of parameters
  attr(df_sim, "sim_means")  <- sim_means  ## List of the simulations means, after shuffle applied if needed
  attr(df_sim, "sim_sigmas") <- sim_sigmas ## List of the simulations covariance matrices, after shuffle applied if needed
  
  return(df_sim)
}


# ### Saving off simulations for the PoC app.
# mns <- list(c(4, 6, sample(1:3, size = 3)),
#             c(5, 4, sample(1:3, size = 3)))
# .p <- length(mns[[1]])
# covar <- diag(.p) 
# uppertri_ind <- upper.tri(covar)
# covar[uppertri_ind] <- sample(seq(-.2, .2, by = .1), sum(uppertri_ind), replace = T)
# covar[uppertri_ind] <- t(covar)[uppertri_ind]
# lqmm::make.positive.definite(covar)
# 
# covs <- list(covar, 
#              covar)
# mySim <- sim_pDim_kCl(means = mns, sigmas = covs)
# 
# ex3 <- mySim
# save(ex3, file = "../PoC_WebGL_shiny/_NicholasSpyrison_rgl/data/ex3_5var2Cl_noise.r")

