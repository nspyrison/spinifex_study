##### NOTES -----
## d near 10 
## G near 3 going to 4, 5
## models; VII, EEE, VVV, respectively: diff scale, diff shape, diff scale & shape
#### same means used 3(diag(p) chopped off), var differing by model
## Var is [dxd], may start at d=5.

#### For tomorrow;
## 3 different model sets
## as PCA, Manual tour, and ClSep_MMP

## Setup
source("./R/sim_pDim_kCl.r")
args(sim_pDim_kCl)
DO_SAVE <- FALSE

## Initializae
p <- 10    ## Number of dimensions
k_cl <- 3  ## Number of clusters
z <- rep(0, p)

## Obs, and means 
n_obs_per_cl <- rep(list(200), k_cl)
mns <- list(c(5, rep(0, p - 1)), 
            c(0, 5, rep(0, p - 2)),
            c(0, 0, 5, rep(0, p - 3))
)

## Covariances
diag_p <- diag(p)
ind <- upper.tri(diag_p)
cov1 <- cov2 <- cov3 <- .6 * diag_p
cov2[2, 1] <- cov2[3, 1] <- cov2[3, 2] <- .5 
cov2[ind] <- t(cov2)[ind]
cov3[2, 1] <- cov3[3, 1] <- cov3[3, 2] <- -.5 
cov3[ind] <- t(cov3)[ind]

covs_VII <- list(cov1, 2 * cov1, .5 * cov1)
covs_EEE <- list(cov2, cov2, cov2)
covs_VVV <- list(cov1, cov2, cov3)

## Create simulations
sim_VII <- sim_pDim_kCl(means = mns, sigmas = covs_VII, 
                        cl_points = n_obs_per_cl, do_shuffle = F)
sim_EEE <- sim_pDim_kCl(means = mns, sigmas = covs_EEE, 
                        cl_points = n_obs_per_cl, do_shuffle = F)
sim_VVV <- sim_pDim_kCl(means = mns, sigmas = covs_VVV, 
                        cl_points = n_obs_per_cl, do_shuffle = F)

## Save
if(DO_SAVE == TRUE){
  root <- "./catalogue/simulations/"
  save(sim_VII, file = paste0(root, quote(sim_VII), ".rda"))
  save(sim_EEE, file = paste0(root, quote(sim_EEE), ".rda"))
  save(sim_VVV, file = paste0(root, quote(sim_VVV), ".rda"))
}

## Inital vis
clas <- attr(sim_VII, "cl_lvl") ## all the same number and order of obs

GGally::ggpairs(sim_VII[, 1:3], ggplot2::aes(color = as.factor(clas)))
GGally::ggpairs(sim_EEE[, 1:3], ggplot2::aes(color = as.factor(clas)))
GGally::ggpairs(sim_VVV[, 1:3], ggplot2::aes(color = as.factor(clas)))

source("./R/ggpcapairs.r")
ggpcapairs(sim_VII, class = clas, top_n = 3)
ggpcapairs(sim_EEE, class = clas, top_n = 3)
ggpcapairs(sim_VVV, class = clas, top_n = 3)
