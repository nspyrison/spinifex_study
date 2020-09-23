#' Transforms the variable space of an ellipse into a boomerang
#' 
#' Take the rows of a cluster in the shape of an elipse, goes to principal 
#' component space, transforms PC2, based on dnorm of PC1, shifts mean back 
#' to 0, then transforms back into the original variable space. 
#' @param cl_rows A dataframe of the rows of 1 cluster.
#' @examples 
#' source(here::here("R/sim_pDim_kCl.r"))
#' mns  <- list(c(0, 0))
#' covs <- list( matrix(c(3, .9, 
#'                        .9, 1), 
#'                      ncol = 2, byrow = TRUE) )
#' input <- sim_pDim_kCl(means = mns, sigmas = covs,
#'                       cl_points = list(200), do_shuffle = FALSE)
#' output <- boomerang(cl_a)
#' plot(input)
#' plot(output)
#' plot(prcomp(input)$x)
#' plot(prcomp(output)$x)
boomerang <- function(cl_rows){
  ## Initialize
  pca_obj <- prcomp(cl_rows)
  var_sp <- tform_var_sp <- cl_rows
  pca_sp <- pca_sp_tfrom <- pca_obj$x
  rot <- pca_obj$rotation
  
  ## Transform PC2, based on PC1
  height_coef <- diff(range(pca_sp[, 1]))
  tform_pc2 <- height_coef * dnorm(x = pca_sp[, 1], mean = mean(pca_sp[, 1]), sd = sd(pca_sp[, 1]))
  ## Shift the mean back to 0
  tform_pc2 <- tform_pc2 - mean(tform_pc2)
  
  ## Apply the tranform back to variable space
  for(i in 1:ncol(cl_rows)){
    tform_var_sp[,i] <- var_sp[,i] + rot[i, 2] * tform_pc2
  }
  
  tform_var_sp
}

