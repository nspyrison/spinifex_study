source("./R/sim_pDim_kCl.r")
mns  <- list(c(0, 0))
covs <- list( matrix(c(3, 0, 0, 3), 2) )
cl_a <- sim_pDim_kCl(means = mns, sigmas = covs,
                     cl_points = list(200), do_shuffle = FALSE)
cl_rows <- cl_a
boomerang <- function(cl_rows){
  pca_obj <- prcomp(cl_rows)
  var_sp <- cl_rows
  pca_sp <- pca_sp2 <- pca_obj$x
  rot <- pca_obj$rotation
  z_scores <- seq(-3, 3, by = .1)
  
  # Let's print the vector
  z_scores
  pca_sp2[, 2] <- pca_sp[, 2] + dnorm(x =  pca_sp[, 2])
  
  
}