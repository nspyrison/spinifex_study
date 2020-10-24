rand_participant <- function(){
  #### Initialize
  ## Given
  factor_perms   <- matrix(c(1, 1,  2, 2,  3, 3, ## The 3 permutations of the 3 factor orders
                             1, 1,  3, 3,  2, 2,
                             2, 2,  3, 3,  1, 1,
                             2, 2,  1, 1,  3, 3,
                             3, 3,  1, 1,  2, 2,
                             3, 3,  2, 2,  1, 1),
                           nrow = 6, byrow = TRUE)
  location_perms <- matrix(c(1, 2,  3, 1,  2, 3), nrow = 1)
  vc_perms       <- matrix(c(1, 1,  2, 2,  3, 3), nrow = 1)
  ##
  n_facotor_perms  <- nrow(factor_perms)
  n_location_perms <- nrow(location_perms)
  n_vc_perms       <- nrow(vc_perms)
  n_perms_prr <- ## Number of permutations per round robin
    n_facotor_perms * n_location_perms * n_vc_perms
  
  ## Selection
  participant_num <- sample(1:999, 1)
  rr_num          <- participant_num %/% n_perms_prr
  perm_num_NEXT   <- 1 + (participant_num - 1) %% n_perms_prr
  
  
  
  
  #### Permutation count table
  perm_cnt_tbl_init <-
    matrix(rep(rr_num, n_perms_prr), nrow = 1, ncol = n_perms_prr,)
  colnames(perm_cnt_tbl_init) <- paste0("Perm", 1:n_perms_prr)
  ## Assigning next
  perm_cnt_tbl_NEXT <- perm_cnt_tbl_RAND <- perm_cnt_tbl_init
  perm_cnt_tbl_NEXT[, 1:perm_num_NEXT] <- perm_cnt_tbl_NEXT[, 1:perm_num_NEXT] + 1
  ## Assigned randomly within the available.
  inc <- sample(1:n_perms_prr, perm_num_NEXT, replace = FALSE)
  perm_cnt_tbl_RAND[, inc] <- perm_cnt_tbl_RAND[, inc] + 1
  perm_num_RAND <- inc[length(inc)]
  
  
  decode_perm <- function(perm_num, perm_cnt_tbl){
    ## Level names
    factor_nms   <- c("pca", "grand", "radial")
    location_nms <- c("0_1", "33_66", "50_50")
    vc_nms       <- c("EEE", "EEV", "banana")
    p_dim_nms    <- this_p_dim_nm_ord <- c("p4", "p6")
    ##  
    this_factor_perm   <-
      1 + (perm_num - 1) %% n_facotor_perms
    this_location_perm <-
      1 + ((perm_num - 1) %/% n_facotor_perms) %% n_location_perms
    this_vc_perm       <-
      1 + ((perm_num - 1) %/% (n_facotor_perms * n_location_perms)) %% n_vc_perms
    
    ## Return in order
    this_factor_nm_ord <- factor_nms[factor_perms[this_factor_perm, ]]
    this_location_ord  <- location_nms[location_perms[this_location_perm, ]]
    this_vc_nm_ord     <- vc_nms[vc_perms[this_vc_perm, ]]
    ##
    this_sim_nms <- paste(rep(this_vc_nm_ord, 3),
                          rep(p_dim_nms, 3), 
                          rep(this_location_ord, 3),
                          sep = "_")
    
    ## Messaging
    print(paste0("Participant number: ", participant_num))
    print(paste0("Permutation number: ", perm_num))
    print(paste0("Permutations per round-robin: ", n_perms_prr))
    print("Perm count table: ")
    print(perm_cnt_tbl)
    print("sim names: ")
    print(this_sim_nms)
    
    return(this_sim_nms)
  }
  sim_nms_NEXT <- decode_perm(perm_num_NEXT, perm_cnt_tbl_NEXT)
  sim_nms_RAND <- decode_perm(perm_num_RAND, perm_cnt_tbl_RAND)
}

for(i in 1:3){
  tictoc::tic()
  rand_participant()
  tictoc::toc()
}
