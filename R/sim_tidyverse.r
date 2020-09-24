## PACKAGES
require(tidyverse)
require(mvtnorm)
require(GGally)
require(tourr)
require(spinifex)

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
  d <- x[, 2:ncol(x)]
  colnames(d) <- paste0("x", 1:ncol(d))
  rot <- mutate(d, 
                x1 = 0.707  * x1 + 0.707 * x3,
                x3 = -0.707 * x1 + 0.707 * x3,
                x2 = 0.707  * x2 + 0.707 * x4,
                x4 = -0.707 * x2 + 0.707 * x4)
  as_tibble(data.frame(cl, rot))
}

## Creates a 'V' in the shape of ">", by shifting 1/5 of the obs to be offset.
banana_tform <- function(df){
  n <- length(df)
  d <- df
  d[(n+1):(n+1+n/5), 2] <- d[(n+1):(n+1+n/5), 2] - 0.5
  d[(n+1):(n+1+n/5), 3] <- d[(n+1):(n+1+n/5), 3] + 0.5
  d[(n+1+n/5+1):(n+1+2*n/5), 2] <- d[(n+1+n/5+1):(n+1+2*n/5), 2] - 0.5
  d[(n+1+n/5+1):(n+1+2*n/5), 3] <- d[(n+1+n/5+1):(n+1+2*n/5), 3] - 0.5
  d[(n+1+2*n/5+1):(n+1+3*n/5), 2] <- d[(n+1+2*n/5+1):(n+1+3*n/5), 2] - 1
  d[(n+1+2*n/5+1):(n+1+3*n/5), 3] <- d[(n+1+2*n/5+1):(n+1+3*n/5), 3] + 1
  d[(n+1+3*n/5+1):(n+1+4*n/5), 2] <- d[(n+1+3*n/5+1):(n+1+4*n/5), 2] - 1
  d[(n+1+3*n/5+1):(n+1+4*n/5), 3] <- d[(n+1+3*n/5+1):(n+1+4*n/5), 3] - 1
  return(d)
}