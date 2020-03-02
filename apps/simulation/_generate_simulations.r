library("mvtnorm")
library("lqmm")
set.seed(20200228)

easy <- function(){
  p <<- sample(5:7, 1); p_signal <<- 1; cl <<- sample(3:4, 1); 
  vc_vect <<- seq(-.1, 0.6, by = 0.1); mn_vect <<- seq(2, 3, .1); 
  n_cl_complexshape <<- 0
}
cat("HARD STILL NEEDS TO CHANGE SHAPE OF SIMULATIONS")
hard <- function(){
  p <<- sample(7:9, 1); p_signal <<- sample(3:5, 1); cl <<- sample(3:4, 1); 
  vc_vect <<- seq(-.1, 0.8, by = 0.1); mn_vect <<- seq(2, 2.5, .1);
  n_cl_complexshape <<- sample(1:2, 1)
}
###### Simulate clusters
simulate_clusters <- function(p = sample(5:7, 1),      ## Numbbr of columns
                              p_signal = 1,            ## Number of signal columns (difference in mean)
                              cl = sample(3:4, 1),     ## Number of clusters
                              vc_vect = seq(-.1, 0.6, 0.1), ## Posible values of var-covar (cormat) elemets
                              mn_vect = seq(2, 3, .1), ## Posible values of for column mean (strength of signal)
                              n_cl_complexshape = 0    ## Sample(1:2, 1) ## Number of clusters to make into 3d cresent shape.
){
  x <- NULL
  cl_n <- NULL
  cl_mn <- NULL
  vc <- NULL
  for (i in 1:cl) {
    ## Set sample size and partician sample if complex shape
    n <- full_samp_n <- sample(30:150, 1)
    if (i <= n_cl_complexshape){n <- round(full_samp_n / 3)}
    
    ## Make a cluster 
    vc <- matrix(sample(vc_vect, p * p, replace = T), nrow = p) 
    ind <- lower.tri(vc) 
    vc[ind] <- t(vc)[ind] 
    vc <- lqmm::make.positive.definite(vc)
    diag(vc) <- 1
    mn <- c(sample(mn_vect * sample(c(-1, 1), 1), p_signal, replace = T), rep(0, p - p_signal))
    this_x <- mvtnorm::rmvnorm(n = n, mean = mn, vc)
    
    ## Make complex shape if specified
    if (i <= n_cl_complexshape){
      for (i in 1:n_cl_complexshape) {
        reord <- sample(1:p, p)
        cl_mnshift <- c(sample(seq(1, 2, .1), 2), rep(0, p - 2))[reord]
        new_shape <- mvtnorm::rmvnorm(n = n, mean = mn + cl_mnshift, vc)
        this_x <- rbind(this_x, new_shape)
      }
    }
    
    ## Retrun
    x <- rbind(x, this_x)
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
  params <- list(p = p, p_signal = p_signal, cl = cl, 
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

### Test run
p <-  pnoise <- cl <- vc_vect <- mn_vect <- n_cl_complexshape <- NULL
easy()
simulate_clusters()

hard()
simulate_clusters()

library("spinifex")
dat <- tourr::rescale(tourr::flea[, 1:6])
pca1_loading <- prcomp(dat)$rotation
pca2_loading <- princomp(dat)$loadings

spinifex::view_basis(pca1_loading[,1:2])
spinifex::


