library("mvtnorm")
library("lqmm")
set.seed(20200228)

### trivial funcs; set global evn variables.
train <- function(){
  p <<- sample(4:5, 1); p_signal <<- 1; cl <<- sample(3:4, 1); 
  vc_vect <<- seq(-.1, 0.4, by = 0.1); mn_vect <<- seq(3, 4, .1); 
  n_cl_complexshape <<- 0
}
easy <- function(){
  p <<- sample(5:7, 1); p_signal <<- 1; cl <<- sample(3:4, 1); 
  vc_vect <<- seq(-.1, 0.6, by = 0.1); mn_vect <<- seq(2, 3, .1); 
  n_cl_complexshape <<- 0
}
hard <- function(){
  p <<- sample(7:9, 1); p_signal <<- sample(3:5, 1); cl <<- sample(3:4, 1); 
  vc_vect <<- seq(-.1, 0.8, by = 0.1); mn_vect <<- seq(2, 2.5, .1);
  n_cl_complexshape <<- sample(1:2, 1)
}
###### Simulate clusters
simulate_clusters <- function(p = sample(5:7, 1),      ## Number of columns
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
  ## Create each cluster
  for (i in 1:cl) {
    ## Set sample size and partician sample if complex shape
    n <- full_samp_n <- sample(30:150, 1)
    if (i <= n_cl_complexshape){n <- round(full_samp_n / 3)}
    
    ## Make a cluster 
    vc <- matrix(sample(vc_vect, p * p, replace = T), nrow = p)
    ind <- upper.tri(vc)
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
  
  ## Format data
  x <- scale(x)
  x <- as.data.frame(x)
  
  ## Reorder rows and columns
  x.indx <- sample(1:nrow(x))
  y.indx <- sample(1:ncol(x))
  x <- x[x.indx, y.indx]
  
  ## Mask output after reorder
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
  
  ## Record attributes; AFTER REORDER
  attr(x, "params") <- params       ## List of parameters
  attr(x, "cl_lvl") <- cl_lvl_reord ## Cluster levels
  attr(x, "cl_mn")  <- cl_mn_reord  ## Mean of each cluster*variable
  attr(x, "vc")     <- vc_reord     ## Variance-covariance matrix
  
  return(x)
}

##### EXPORT SIMULATIONS ------ 
### Simulate and export evaluation section simulations
if(F){ ## Don't run
  sim_series <- 300
  for (i in 1:12){
    if((i %% 2) == 1) {easy()} ## Odd sims are easy
    if((i %% 2) == 0) {hard()} ## Even sims are hard
    sim_nm <- paste0("simulation_data", (sim_series + i))
    assign(sim_nm, simulate_clusters())
    filepath_nm = paste0("./apps/data/", sim_nm,".rds")
    saveRDS(object = get(sim_nm), file = filepath_nm)
    
    if (file.exists(filepath_nm)) {
      cat(paste0("Saved ", sim_nm, ". \n"))
    } else {cat(paste0("warning: file not found for ", sim_nm, ". \n"))}
  }
}

### Simulate and export training section simulations
if(F){ ## Don't run
  sim_series <- "_t"
  for (i in 1:4){
    train() ## Using training parameters
    sim_nm <- paste0("simulation_data", sim_series, i)
    assign(sim_nm, simulate_clusters())
    filepath_nm = paste0("./apps/data/", sim_nm,".rds")
    saveRDS(object = get(sim_nm), file = filepath_nm)
    
    if (file.exists(filepath_nm)) {
      cat(paste0("Saved ", sim_nm, ". \n"))
    } else {cat(paste0("warning: file not found for ", sim_nm, ". \n"))}
  }
}


##### Other -----
### Sanity check
library("spinifex")
dat <- tourr::rescale(tourr::flea[, 1:6])
pca1_loading <- prcomp(dat)$rotation
pca2_loading <- princomp(dat)$loadings

spinifex::view_basis(pca1_loading[,1:2])



