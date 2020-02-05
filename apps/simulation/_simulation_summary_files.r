## scratchpad for making simulation csv files.

### zSim example -----
ex <- readRDS("./apps/simulation/simulation_data201.rds") 
str(ex)


### LOAD ALL SIM -----
rm(list = ls(pattern = "^simulation_data"))
load_num  <- start_num <- 201
load_name <- sprintf("simulation_data%03d", load_num)
load_file <- paste0("./apps/simulation/", load_name, ".rds")
while (file.exists(load_file)){
  assign(load_name, readRDS(load_file))
  load_num <- load_num + 1
  load_name <- sprintf("simulation_data%03d", load_num)
  load_file <- paste0("./apps/simulation/", load_name, ".rds")
}
loaded_sim_names <- ls()[grepl("simulation_data", ls())]
id_nums <- start_num:(load_num - 1)


### SIM_PARAMETERS.CSV (& task1_ans) -----
# grain: simulation
sim_parameters <- NULL
for (i in 1:length(loaded_sim_names)) {
  this_sim <- get(loaded_sim_names[i])
  this_covar <- attr(this_sim, "vc")
  # parameters
  p <- ncol(this_sim)
  n_cl <- length(attr(this_sim, "ncl"))
  pnoise <- sum(colSums(attr(this_sim, "mncl") == 0) == n_cl)
  # colate
  this_parameters <- data.frame(id = id_nums[i], p, n_cl, pnoise)
  sim_parameters <- rbind(sim_parameters, this_parameters)
}
rownames(sim_parameters) <- NULL
sim_parameters
#write.csv(sim_parameters, "./apps/simulation/sim_parameters_200s.csv", row.names = FALSE)


### SIM_TASK2_ANS_SCORE.CSV -----
sim_task2_ans <- NULL
for (i in 1:length(loaded_sim_names)) {
  # init
  this_sim <- get(loaded_sim_names[i])
  p <- ncol(this_sim)
  colnames(this_sim) <- paste0("V", 1:p)
  
  this_supervied_sim <- data.frame(this_sim, cluster = attr(this_sim, "cluster"))
  lda_means <- MASS::lda(cluster~., data = this_supervied_sim)$means
  
  abs_lda_means <- matrix(NA, nrow = 2, ncol = p)
  abs_lda_means[1, ] <- abs(lda_means[1, ] - lda_means[2, ])
  abs_lda_means[2, ] <- abs(lda_means[2, ] - lda_means[3, ])
  
  ans <- matrix(NA, nrow = 2, ncol = 12) # 12 is max columns
  for (i in 1:2){
    this_row_ptile <- abs_lda_means[i, ] / max(abs_lda_means[i, ])
    this_row_ans   <- dplyr::case_when(this_row_ptile >= .75 ~ 2, # very
                                       this_row_ptile >= .25 ~ 1, # some
                                       this_row_ptile >=   0 ~ 0)
    ans[i, ] <- c(this_row_ans, rep(NA, 12 - length(this_row_ans))) # coerse to 12, max columns 
  }
  colnames(ans) <- paste0("V", 1:12)
  
  # colate
  this_task2_ans <- data.frame(id = id_nums[i], clusters = c("ab", "bc"), ans)
  sim_task2_ans <- rbind(sim_task2_ans, this_task2_ans)
}
rownames(sim_task2_ans) <- NULL
sim_task2_ans
#write.csv(sim_task2_ans_score, "./apps/simulation/sim_task2_ans.csv", row.names = F)

### SAVING GRAND TPATHS -----
# tour paths for the 18 simulations 
for (i in 1:length(loaded_sim_names)) {
  .sim    <- as.matrix(get(loaded_sim_names[i]))
  .bas    <- prcomp(.sim)$rotation[, 1:2]
  .tpath  <- save_history(data = .sim, tour_path = grand_tour(), max_bases = 8, start = .bas)
  .obj_nm <- paste0("grand_tpath", id_nums[i])
  assign(.obj_nm, .tpath)
  saveRDS(object = get(.obj_nm), paste0("./apps/simulation/", .obj_nm, ".rds"))
}
# Add training
simulation_data_train1 <- readRDS("./apps/simulation/simulation_data_train1.rds")
simulation_data_train2 <- readRDS("./apps/simulation/simulation_data_train2.rds")
loaded_train_names <- ls()[grepl("simulation_data_train", ls())]
for (i in 1:length(loaded_train_names)) {
  .sim    <- as.matrix(get(loaded_train_names[i]))
  .bas    <- prcomp(.sim)$rotation[, 1:2]
  
  .tpath  <- save_history(data = .sim, tour_path = grand_tour(), max_bases = 8, start = .bas)
  .obj_nm <- paste0("grand_tpath_train", i)
  assign(.obj_nm, .tpath)
  saveRDS(object = get(.obj_nm), paste0("./apps/simulation/", .obj_nm, ".rds"))
}
