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
DO_SAVE = FALSE 

sims_kpsigp <- function(k_cl = 3, ## Number of clusters
                        p_sig = 3, ## Intrisic data dimensionality, previously p_sig (nal)
                        p = 10, ## Number of dimensions
                        n_obs_per_cl = rep(list(200), k_cl)
                        , DO_SAVE = FALSE 
){
  ## Initializae
  idd <- p_sig - 1
  models <- c("VII", "EEE", "EVV", "VVV")
  suffix <- paste0(as.character(c(k_cl, p_sig, p)), collapse = "")
  cl_lvls <- paste0("cl ", letters[1:k_cl])
  names(n_obs_per_cl) <- cl_lvls
  ## Hard-coded parameters
  signal <- 10
  var_sz <- 1
  cor_sz <- .5
  
  ## Variable means by cluster
  diag_p <- mn_mat <- diag(p)
  diag(mn_mat) <- signal
  mns <- list()
  for(i in 1:k_cl) mns[[i]] <- mn_mat[i,]
  names(mns) <- cl_lvls
  
  ## Covariances by cluster
  cov1 <- cov2 <- cov3 <<- var_sz * diag_p ## cov1 set, init cov2, cov3
  cov_psig <- cov2[1:p_sig, 1:p_sig]
  ind <- upper.tri(cov_psig) | lower.tri(cov_psig)
  cov_psig[ind] <- cor_sz
  cov2[1:p_sig, 1:p_sig] <- cov_psig ## cov2 set
  cov_psig[ind] <- -cor_sz
  cov3[1:p_sig, 1:p_sig] <- cov_psig ## cov3 set
  ## Ensure cov3 is positive definite 
  if(lqmm::is.positive.definite(cov3) == FALSE){
    cov3_pd <- lqmm::make.positive.definite(cov3)
    if(all.equal(cov3, cov3_pd) != TRUE){
      warning("Had to make cov3 positive definite. From, to, respectively")
      print(cov3)
      print(cov3_pd)
    }
    cov3 <- cov3_pd
  }
  # ## EVALUATING POSITIVE DEFINATE.
  # browser()
  # lqmm::is.positive.definite(cov3)
  # z <- lqmm::make.positive.definite(cov3)
  # lqmm::is.positive.definite(z)
  # all.equal(z, cov3)
  # i <- 0
  # val <- -999
  # while(lqmm::is.positive.definite(cov3) == F){
  #   val <- -(cor_sz - (.1 * i))
  #   cov_psig[ind] <- val
  #   cov3[1:p_sig, 1:p_sig] <- cov_psig ## cov3 set
  #   print(paste0(val, " doesn't lead to cov3 being positive definate."))
  #   cov3
  #   i <- i + 1
  # }
  # print(paste0(val, " DOES lead to cov3 being positive definate."))
  
  covs_VII <- list(cov1 / 4, cov1, cov1 * 4)
  covs_EEE <- list(cov2, cov2, cov2)
  covs_EVV <- list(cov1, cov2, cov3)
  ##
  diag(cov1) <- diag(cov1) / 4
  diag(cov2) <- diag(cov3) * 4
  covs_VVV <- list(cov1, cov2, cov3)
  names(covs_VII) <- cl_lvls
  names(covs_EEE) <- cl_lvls
  names(covs_EVV) <- cl_lvls
  names(covs_VVV) <- cl_lvls
  
  ## Create simulations
  obj_nms <- paste0(models, "_", suffix)
  cov_nms <- paste0("covs_", models)
  
  for(i in 1:length(obj_nms)){
    this_sim <- sim_pDim_kCl(means = mns, sigmas = get(cov_nms[i]),
                             cl_points = n_obs_per_cl, do_shuffle = F)
    assign(x = obj_nms[i], value = this_sim, envir = globalenv())
    print(paste0("Assigned ", obj_nms[i], " as a global variable."))
  }
  
  # ## Save
  # if(DO_SAVE == TRUE){
  #   root <- "./catalogue/simulations/"
  #   save(VII_3310, file = paste0(root, quote(VII_3310), ".rda"))
  #   save(EEE_3310, file = paste0(root, quote(EEE_3310), ".rda"))
  #   save(EVV_3310, file = paste0(root, quote(EVV_3310), ".rda"))
  #   save(VVV_3310, file = paste0(root, quote(VVV_3310), ".rda"))
  # }
}
sims_kpsigp(k_cl = 3, p_sig =  3,  p =  10)
sims_kpsigp(k_cl = 3, p_sig =  5,  p =  10)
sims_kpsigp(k_cl = 3, p_sig =  7,  p =  10)
sims_kpsigp(k_cl = 3, p_sig =  10, p =  10)

if(DO_SAVE == TRUE){
  root <- "./catalogue/simulations/"
  ## 3310
  save(VII_3310, file = paste0(root, quote(VII_3310), ".rda"))
  save(EEE_3310, file = paste0(root, quote(EEE_3310), ".rda"))
  save(EVV_3310, file = paste0(root, quote(EVV_3310), ".rda"))
  save(VVV_3310, file = paste0(root, quote(VVV_3310), ".rda"))
  ## 3510
  save(VII_3510, file = paste0(root, quote(VII_3510), ".rda"))
  save(EEE_3510, file = paste0(root, quote(EEE_3510), ".rda"))
  save(EVV_3510, file = paste0(root, quote(EVV_3510), ".rda"))
  save(VVV_3510, file = paste0(root, quote(VVV_3510), ".rda"))
  ## 3710
  save(VII_3710, file = paste0(root, quote(VII_3710), ".rda"))
  save(EEE_3710, file = paste0(root, quote(EEE_3710), ".rda"))
  save(EVV_3710, file = paste0(root, quote(EVV_3710), ".rda"))
  save(VVV_3710, file = paste0(root, quote(VVV_3710), ".rda"))
  ## 31010
  save(VII_31010, file = paste0(root, quote(VII_31010), ".rda"))
  save(EEE_31010, file = paste0(root, quote(EEE_31010), ".rda"))
  save(EVV_31010, file = paste0(root, quote(EVV_31010), ".rda"))
  save(VVV_31010, file = paste0(root, quote(VVV_31010), ".rda"))
}


## Inital vis
clas <- attr(VII_3310, "cl_lvl") ## all the same number and order of obs

require("GGally"); require("ggplot2");
GGally::ggpairs(VII_3310[, 1:3], aes(color = as.factor(clas))) + ggtitle("Model VII ggpairs (original variables)")
GGally::ggpairs(EEE_3310[, 1:3], aes(color = as.factor(clas))) + ggtitle("Model EEE ggpairs (original variables)")
GGally::ggpairs(EVV_3310[, 1:3], aes(color = as.factor(clas))) + ggtitle("Model EVV ggpairs (original variables)")
GGally::ggpairs(VVV_3310[, 1:3], aes(color = as.factor(clas))) + ggtitle("Model VVV ggpairs (original variables)")

source("./R/ggpcapairs.r")
ggpcapairs(VII_3310, class = clas, top_n = 3)
ggpcapairs(EEE_3310, class = clas, top_n = 3)
ggpcapairs(VVV_3310, class = clas, top_n = 3)
