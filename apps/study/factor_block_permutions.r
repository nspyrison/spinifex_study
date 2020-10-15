participant_num  <- sample(1:999, 1)
full_perm_num <- 1 + participant_num %% 6
##
this_factor_perm   <- 1 + (full_perm_num - 1) %% 3 ## %% is mod
this_location_perm <- 1 + floor((full_perm_num - 1) / 3) %% 3
this_vc_perm       <- 1 + floor((full_perm_num - 1) / 9) %% 6

factor_perms   <- rbind(c(1, 2, 3), ## The 3 permutations of the 3 factor orders
                        c(2, 3, 1),
                        c(3, 1, 2))
location_perms <- 1
vc_perms       <- 1 # rbind(c(1, 2), ## The 6 permutations of the 3 location orders
                    #       c(1, 3),
                    #       c(2, 3),
                    #       c(2, 1),
                    #       c(3, 1),
                    #       c(3, 2))
## set factor and block names
factor_nms   <- c("pca", "grand", "radial")
location_nms <- c("0_1", "33_66", "50_50")
vc_nms       <- c("EEE", "EEV", "banana")
p_dim_nms <- this_p_dim_nm_ord <- c("p4", "p6")
## The decoded names
this_factor_nm_ord <-
  factor_nms[factor_perms[this_factor_perm, ]]
this_location <-
  location_nms[location_perms[this_location_perm, ]]
this_vc_nm_ord <-
  vc_nms[vc_perms[this_vc_perm, ]]

##
this_sim_nms <- paste(rep(this_vc_nm_ord, 3), rep(p_dim_nms, 3), rep(this_location, 3), sep = "_")
this_sim_nms
