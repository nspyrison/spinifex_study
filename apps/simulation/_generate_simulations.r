library("mvtnorm")
library("lqmm")
set.seed(20200228)

#easy defaults: p = sample(5:7, 1); pnoise = p - 1; cl = sample(3:4, 1); vc_vect = seq(-.1, 0.6, by = 0.1); mn_vect = seq(2, 3, .1);
###### Simulate clusters
simulate_clusters <- function(p = sample(5:7, 1), 
                              pnoise = p - 1, 
                              cl = sample(3:4, 1),
                              vc_vect = seq(-.1, 0.6, 0.1),
                              mn_vect = seq(2, 3, .1) 
){
  x <- NULL
  cl_n <- NULL
  cl_mn <- NULL
  vc <- NULL
  for (i in 1:cl) {
    n <- sample(30:150, 1)
    vc <- matrix(sample(vc_vect, p * p, replace = T), nrow = p) 
    ind <- lower.tri(vc) 
    vc[ind] <- t(vc)[ind] 
    vc <- lqmm::make.positive.definite(vc)
    diag(vc) <- 1
    mn <- c(sample(mn_vect * sample(c(-1, 1), 1), p - pnoise, replace = T), rep(0, pnoise))
    x <- rbind(x, mvtnorm::rmvnorm(n = n, mean = mn, vc))
    cl_n <- c(cl_n, n)
    cl_mn <- rbind(cl_mn, mn)
  }
  x <- scale(x)
  x <- as.data.frame(x)
  
  # Reorder rows and columns
  x.indx <- sample(1:nrow(x))
  y.indx <- sample(1:ncol(x))
  x <- x[x.indx, y.indx]
  
  # Mask output after reorder
  rownames(x) <- 1:nrow(x)
  colnames(x) <- paste0("V", 1:ncol(x))
  cl_mn_reord <- cl_mn[, y.indx]
  rownames(cl_mn_reord) <- paste0("cl ", letters[1:nrow(cl_mn)])
  colnames(cl_mn_reord) <- paste0("V", 1:ncol(cl_mn))
  vc_reord <- vc[y.indx, y.indx]
  cl_lvl_reord <- factor(rep(letters[1:cl], cl_n))
  cl_lvl_reord <- cl_lvl_reord[x.indx]
  params <- list(p = p, pnoise = pnoise, cl = cl, 
                       vc_vect = vc_vect, mn_vect = mn_vect)
  
  # Record attributes; AFTER REORDER
  attr(x, "params") <- params       # List of parameters
  attr(x, "cl_lvl") <- cl_lvl_reord # Cluster levels
  attr(x, "cl_mn")  <- cl_mn_reord  # Mean of each cluster*variable
  attr(x, "vc")     <- vc_reord     # Variance-covariance matrix

  # attr(x, "col_reorder") <- y.indx # order variables were scrambled in
  # attr(x, "row_reorder") <- x.indx # order rows were scrambled in 
  return(x)
}

simulate_clusters()


