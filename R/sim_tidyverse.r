#### NS: Di's example mock up. See sim_mvnorm.r for latest simulation functions.
#### This is an old variant. See _sim_user_study.r for the latest simulations.
#' @example
#' file.edit("./apps_supplementary/data_simulation/_sim_user_study.r")
####

## PACKAGES
require(tidyverse)
require(mvtnorm)


## LOCAL UTILITY FUNCTIONS
rmvnorm_tsb <- function(n=100, mean, sigma) {
  d <- rmvnorm(n, mean, sigma)
  colnames(d) <- paste0("x", 1:ncol(d))
  as_tibble(d)
}
stdd <- function(x) {
  (x - mean(x)) / sd(x)
}


## WORK HORSE FUNCTIONS
sim_tidyverse <- function(mns_ls, cov_ls, obs_per_cl){
  df <- map2(mns_ls, cov_ls,
             ~rmvnorm_tsb(n = obs_per_cl, as.matrix(.x), as.matrix(.y)))
  df %>% enframe(name = "cl") %>% unnest(value) %>%
    mutate_if(is.numeric, stdd)
}

rotate <- function(x){
  cl <- x$cl
  orig_colnames <- colnames(x)
  rot <- x[, -1]
  colnames(rot) <- paste0("V", 1:ncol(rot))
  ## Apply transform
  rot <- mutate(rot, 
                V1 = 0.707  * V1 + -0.707 * V3,
                V3 = 0.707 * V1 + 0.707 * V3,
                V2 = 0.707  * V2 + -0.707 * V4,
                V4 = 0.707 * V2 + 0.707 * V4)
  
  ## Return
  ret <- data.frame(cl = cl, rot)
  colnames(ret) <- orig_colnames
  return(ret)
}


## Creates a 'V' in the shape of ">", by shifting 1/5 of the obs to be offset.
banana_tform <- function(d){
  ## Initialize
  f <- floor(nrow(d) / 5) ## length of 1/5 of the data
  f1 <- 1:f
  f2 <- f1 + f
  f3 <- f2 + f
  f4 <- f3 + f
  ## f5, last fifth of the data stays as is. 
  
  ## Apply transform
  d[f1, 2] <- d[f1, 2] - 0.5
  d[f1, 3] <- d[f1, 3] + 0.5
  d[f2, 2] <- d[f2, 2] - 0.5
  d[f2, 3] <- d[f2, 3] - 0.5
  d[f3, 2] <- d[f3, 2] - 1
  d[f3, 3] <- d[f3, 3] + 1
  d[f4, 2] <- d[f4, 2] - 1
  d[f4, 3] <- d[f4, 3] - 1
  
  d
}
