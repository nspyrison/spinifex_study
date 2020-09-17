##### NOTES -----
## d near 10 
## G near 3 going to 4, 5
## models; VII, EEE, VVV, respectively: diff scale, diff shape, diff scale & shape
#### same means used 3(diag(p) chopped off), var differing by model
## Var is [dxd], may start at d=5.

# ## Setup
# source("./R/sim_pDim_kCl.r")
# DO_SAVE = FALSE 

##
## SIMULATION FUNCTION ----
## using mvtnorm::rmvnorm, lqmm to make the models as described in the mclust5 paper
## Creates and assigns models c("VII", "EEE", "EVV", "VVV") to the global envir.
#### wtih the specified params.
##
sims_mvnorm_mclust <- function(
  k_cl = 3, ## Number of clusters
  #sigMns = 3, ## Intrisic data dimensionality
  sigCors = 3,
  p = 10, ## Number of dimensions
  n_obs_per_cl = rep(list(200), k_cl),
  mn_sz  = 5,
  var_sz = 1,
  cor_sz = .5,
  cor_var_sz = 3
  #, DO_SAVE = FALSE 
){
  source("./R/sim_pDim_kCl.r")
  ## Initializae
  models <- c("EII", "VII", "EEE", "EVV", "VVV")
  suffix <- paste0(as.character(c(k_cl, sigCors, p)), collapse = "") ##!! sigCors, or sigMns?
  cl_lvls <- paste0("cl ", letters[1:k_cl])
  names(n_obs_per_cl) <- cl_lvls
  
  ## Variable means by cluster
  diag_p <- mn_mat <- diag(p)
  diag(mn_mat) <- mn_sz
  mns <- list()
  for(i in 1:k_cl) mns[[i]] <- mn_mat[i,]
  names(mns) <- cl_lvls
  
  ## Covariances by cluster
  cov1 <- cov2 <- cov3 <- var_sz * diag_p ## cov1 set, init cov2, cov3
  cor_mat <- cor_var_sz * cov2[1:sigCors, 1:sigCors]
  ind <- upper.tri(cor_mat) | lower.tri(cor_mat)
  cor_mat[ind] <- cor_sz
  cov2[1:sigCors, 1:sigCors] <- cor_mat ## cov2 set
  cor_mat[ind] <- -cor_sz
  cov3[1:sigCors, 1:sigCors] <- cor_mat ## cov3 set
  ## Ensure cov3 is positive definite 
  if(lqmm::is.positive.definite(cov3) == FALSE){
    cov3_pd <- lqmm::make.positive.definite(cov3)
    if(all.equal(cov3, cov3_pd) != TRUE){
      warning("Had to make cov3 positive definite. From and to, respectively in the above prints.")
      print(cov3)
      print(cov3_pd)
    }
    cov3 <- cov3_pd
  }
  
  # ## EVALUATING POSITIVE DEFINATE ------
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
  #   cov3[1:sigMns, 1:sigMns] <- cov_psig ## cov3 set
  #   print(paste0(val, " doesn't lead to cov3 being positive definate."))
  #   cov3
  #   i <- i + 1
  # }
  # print(paste0(val, " DOES lead to cov3 being positive definate."))
  
  #### Wave 1 covariances
  covs_EII <- list(cov1, cov1, cov1)
  covs_VII <- list(cov1 / 4, cov1, cov1 * 4)
  covs_EEE <- list(cov2, cov2, cov2)
  covs_EVV <- list(cov1, cov2, cov3)
  diag(cov1) <- diag(cov1) / 4
  diag(cov2) <- diag(cov3) * 4
  covs_VVV <- list(cov1, cov2, cov3)
  ##
  names(covs_EII) <- cl_lvls
  names(covs_VII) <- cl_lvls
  names(covs_EEE) <- cl_lvls
  names(covs_EVV) <- cl_lvls
  names(covs_VVV) <- cl_lvls
  
  ##FORGET ABOUT WAVE 2 & 3 FOR THE MOMENT.
  # #### Wave 2
  # covs_EII <- list(cov1, cov1, cov1)
  # covs_EEI <- list(cov1oval, cov1oval, cov1oval)
  # covs_VEI <- list(cov1oval, cov1oval, cov1oval)
  # covs_EVI <- list(cov1, cov1oval, cov1oval)
  # covs_VVI <- list(cov1, cov1oval / 4, cov1oval * 4) 
  # covs_EVE <- list(cov1, cov2, cov2)
  # covs_EEV <- list(cov1oval, cov2, cov3)
  # ### Wave 3
  # covs_VEE <- list(cov2 / 4, cov2, cov2 * 4) ## Need to */ only diag elements
  # covs_VEV <- list(cov1oval / 4, cov2, cov3 * 4) ## Need to */ only diag elements
  # covs_VVE <- list(cov1, cov3 cov3 ## Need to */ only diag elements
  
  ## Create simulations
  obj_nms <- paste0(models, "_", suffix)
  cov_nms <- paste0("covs_", models)
  for(i in 1:length(obj_nms)){
    this_sim <- sim_pDim_kCl(means = mns, sigmas = get(cov_nms[i]),
                             cl_points = n_obs_per_cl, do_shuffle = FALSE)
    assign(x = obj_nms[i], value = this_sim, envir = globalenv())
    print(paste0("Assigned ", obj_nms[i], " as a global variable."))
  }
  
  # ## Save within function
  # if(DO_SAVE == TRUE){
  #   root <- "./catalogue/simulations/"
  #   save(EII_3310, file = paste0(root, quote(EII_3310), ".rda"))
  #   save(VII_3310, file = paste0(root, quote(VII_3310), ".rda"))
  #   save(EEE_3310, file = paste0(root, quote(EEE_3310), ".rda"))
  #   save(EVV_3310, file = paste0(root, quote(EVV_3310), ".rda"))
  #   save(VVV_3310, file = paste0(root, quote(VVV_3310), ".rda"))
  # }
}

##
## CREATE SIMS TO GLOBAL ENV. -----
##
sims_mvnorm_mclust(k_cl = 3, sigCors = 3, p =  10,
                   n_obs_per_cl = rep(list(200), 3),
                   mn_sz = 5, var_sz = 1, cor_sz = .5, cor_var_sz = 3.3)

##
## ARE OUTPUT COVARIACNES AS EXPECTED? -----
##
#str(EEE_3310)
print0("EXPECTED:")
(exp <- tibble::as_tibble(attr(EEE_3310, "input_args")$sigmas[[1]]))
print0("ACTUAL:")
(act <- tibble::as_tibble(round(attr(EEE_3310, "cl_sigmas")[[1]], 1)))
print0("DIFF:")
tibble::as_tibble(round(act - exp, 1))

##
## TUNING mn_sz and var_sz -----
## Goal want to find mn_sz, and var_sz
#### s.t. variance between clusters = 2 * variance within clusters
tune_mn_var <- function(k_cl = 3 , p = 10, mn_sz = 5, cor_var_sz = 3, 
                        cor_sz = .5, var_sz = 1){
  sims_mvnorm_mclust(k_cl = k_cl, sigCors = 3, p = p,
                     n_obs_per_cl = rep(list(200), 3),
                     mn_sz = mn_sz, var_sz = var_sz, cor_sz = cor_sz,
                     cor_var_sz = cor_var_sz)
  x <- EEE_3310
  
  input_cov <- attr(x, "input_args")$sigmas
  output_cov <- list(cov(x[  1:200, ]),
                     cov(x[201:400, ]),
                     cov(x[401:600, ]))
  var_bt <- round(sum(apply(x, 2, var)), 1)
  var_wi <- round(sum(c(apply(x[  1:200, ], 2, var),
                        apply(x[201:400, ], 2, var),
                        apply(x[401:600, ], 2, var))) / 3, 1)
  var_delta <- var_bt - var_wi
  diff_wi_delta <- var_wi - var_delta
  # var_mat <- var(x)
  # sum_col_var <- round(sum(diag(var_mat)), 1) ## basicaly same with var_bt
  source("./R/ggpcapairs.r"); require("ggplot2");
  clas <- attr(x, "cl_lvl") ## all the same number and order of obs
  titles <-
    paste0(c("VARIABLE SPACE", "PC SPACE"),
           " -- EEE_3310, var_bt=", round(var_bt, 1),
           ", var_wi:", round(var_wi, 1))
  print(GGally::ggpairs(x[, 1:3], aes(color = as.factor(clas))) + ggtitle(titles[1]))
  print(ggpcapairs(x, class = clas, top_n = 3) + ggtitle(titles[2]))
  print(k_cl)
  print(p)
  print(mn_sz)
  print(cor_var_sz)
  print(var_bt)
  print(var_wi)
  print(var_delta)
  print(diff_wi_delta)
}
tune_mn_var(k_cl = 3, p = 10, mn_sz = 5, cor_var_sz = 3.3)
### LOOKING AT PCA SIGNAL FROM MEAN DIFFS AND VARIANCE DIFFS
tibble::tibble(
  k_cl          = c(3,    3,    3     ), ## COEFFIENCTS
  p             = c(10,   10,   10    ), ## COEFFIENCTS 
  mn_sz         = c(5,    5,    5     ), ## SIGNAL FROM MEANS
  cor_var_sz    = c(5,    3,    3.3   ), ## NOISE FROM VARIANCE
  var_bt        = c(39.6, 33.9, 33.5  ), ## ~ k_cl * (cor_var_sz + mn_sz) + (p - k_cl) * var_sz
  var_wi        = c(22.1, 16.3, 16.9  ), ## ~ k_cl * cor_var_sz + (p - k_cl) * var_sz
  var_delta     = c(17.5, 17.6, 16.6  ), ## ~ k_cl * (mn_sz) !?
  diff_wi_delta = c(4.6,  -1.3, 0.3   )
  #cor_sz     = c(X), ## WON'T CHANGE MARGINAL VARIANCE
  #var_sz     = c(X), ## THIS iS JUST NOISE DIM VARIANCE
)


##
## MANUAL SAVING ------
##
if(DO_SAVE == TRUE){
  root <- "./catalogue/simulations/"
  ## 3310
  save(EII_3310, file = paste0(root, quote(EII_3310), ".rda"))
  save(VII_3310, file = paste0(root, quote(VII_3310), ".rda"))
  save(EEE_3310, file = paste0(root, quote(EEE_3310), ".rda"))
  save(EVV_3310, file = paste0(root, quote(EVV_3310), ".rda"))
  save(VVV_3310, file = paste0(root, quote(VVV_3310), ".rda"))
  # ## 3510
  # save(EII_3510, file = paste0(root, quote(EII_3510), ".rda"))
  # save(VII_3510, file = paste0(root, quote(VII_3510), ".rda"))
  # save(EEE_3510, file = paste0(root, quote(EEE_3510), ".rda"))
  # save(EVV_3510, file = paste0(root, quote(EVV_3510), ".rda"))
  # save(VVV_3510, file = paste0(root, quote(VVV_3510), ".rda"))
  # ## 3710
  # save(EII_3710, file = paste0(root, quote(EII_3710), ".rda"))
  # save(VII_3710, file = paste0(root, quote(VII_3710), ".rda"))
  # save(EEE_3710, file = paste0(root, quote(EEE_3710), ".rda"))
  # save(EVV_3710, file = paste0(root, quote(EVV_3710), ".rda"))
  # save(VVV_3710, file = paste0(root, quote(VVV_3710), ".rda"))
  # ## 31010
  # save(EII_31010, file = paste0(root, quote(EII_31010), ".rda"))
  # save(VII_31010, file = paste0(root, quote(VII_31010), ".rda"))
  # save(EEE_31010, file = paste0(root, quote(EEE_31010), ".rda"))
  # save(EVV_31010, file = paste0(root, quote(EVV_31010), ".rda"))
  # save(VVV_31010, file = paste0(root, quote(VVV_31010), ".rda"))
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
ggpcapairs(EVV_3310, class = clas, top_n = 3)
ggpcapairs(VVV_3310, class = clas, top_n = 3)
