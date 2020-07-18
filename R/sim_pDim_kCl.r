#' Creates a data frame containing clusters of multivariate data 
#'
#' @examples 
#' ## Goal ClSep: ~c(.8, 2, 0,0,0)
#' mns <- list(c(10, 3, rep(0,3)), c(2, 1, rep(0,3)))
#' covs <- list(diag(5), diag(5))
#' sim_pDim_kCl(means = mns, covars = covs)
#' 
#' sim_pDim_kCl(means = mns, covars = covs, n_points = list(200, 50))

sim_pDim_kCl <- function(means, 
                         sigmas,
                         n_points = rep(list(100), length(means)),
                         method = c("eigen", "svd", "chol"),
                         do_permute = FALSE
) {
  means <- as.list(means)
  sigmas <- as.list(sigmas)
  n_points <- as.list(n_points)
  p <- length(means[[1]])
  k <- length(means)
  ## means and covariances are both of length k, clusters
  stopifnot(all(k == c(length(means), length(sigmas))) ) 
  ## elements of means and elements covariances have length, rows/cols p, number of numeric variables.
  stopifnot(all(p == c(length(means[[1]]), nrow(sigmas[[1]]), ncol(sigmas[[1]]))))
  
  require("mvtnorm")
  require("lqmm")
  set.seed(20200717)
  

  ## Create each cluster
  df_sim <- NULL
  for (i in 1:k) {
    ## Set sample size and partician sample if complex shape
    .n <- n_points[[i]]
    .mn <- means[[i]]
    .cov <- as.matrix(sigmas[[i]])
    
    if(lqmm::is.positive.definite(.cov) == FALSE){
      warning(paste0("sigmas[[", i, "]] wasn't a positive definite matrix. Applied lqmm::make.positive.definite()."))
      .cov <- lqmm::make.positive.definite(.cov)
    }
    
    .k <- mvtnorm::rmvnorm(n = .n, mean = .mn, sigma = .cov, method = method)
    df_sim <- rbind(df_sim)
  }
  df_sim <- as.data.frame(df_sim)
  
  if(do_permute == TRUE) {
    ## Reorder rows and columns
    row_ord <- sample(1:nrow(df_sim))
    col_ord <- sample(1:p)
    df_sim <- df_sim[row_ord, col_ord]
    
    vc_reord <- vc[y.indx, y.indx]
     <- rep(letters[1:2], unlist(l))
    cl_lvl_reord <- factor(rep(letters[1:cl], cl_n))
    cl_lvl_reord <- cl_lvl_reord[x.indx]
  }
  
  ## Mask output after reorder
  rownames(x) <- 1:nrow(x)
  colnames(x) <- paste0("V", 1:ncol(x))
  cl_mn_reord <- cl_mn[, y.indx]
  rownames(cl_mn_reord) <- paste0("cl ", letters[1:nrow(cl_mn)])
  colnames(cl_mn_reord) <- paste0("V", 1:ncol(cl_mn))
  
  params <- list(means = means, sigmas = sigmas, n_points = n_points, 
                 method = method, scramble = scramble)
  
  ## Record attributes
  attr(x, "params") <- params       ## List of parameters
  attr(x, "cl_lvl") <- cl_lvl_reord ## Cluster levels
  attr(x, "cl_mn")  <- cl_mn_reord  ## Mean of each cluster*variable
  attr(x, "vc")     <- vc_reord     ## Variance-covariance matrix
  # attr(x, "col_reorder") <- y.indx ## Order variables were scrambled in
  # attr(x, "row_reorder") <- x.indx ## Order rows were scrambled in
  
  return(x)
  
}