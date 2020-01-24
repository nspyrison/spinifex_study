## scratchpad for making simulation csv files.

### zSim example -----
ex <- readRDS("./apps/simulation/simulation_data101.rds") 
str(ex)


### LOAD ALL SIM -----
load_num <- 201
load_name <- sprintf("simulation_data%03d", load_num)
load_file <- paste0("./apps/simulation/", load_name, ".rds")
while (file.exists(load_file)){
  assign(load_name, readRDS(load_file))
  load_num <- load_num + 1
  load_name <- sprintf("simulation_data%03d", load_num)
  load_file <- paste0("./apps/simulation/", load_name, ".rds")
}
loaded_sim_names <- ls()[grepl("simulation_data", ls())]

id_nums <- (load_num - length(loaded_sim_names)):load_num
### CREATE SIM_CL_MEANS.CSV -----
# grain: sim*cluster, cl
sim_cl_means <- NULL
for (i in 1:length(loaded_sim_names)) {
  this_sim <- get(loaded_sim_names[i])
  this_mncl <- attr(this_sim, "mncl")
  while (ncol(this_mncl) < 12) { # force columns to 12, the max(p)
    this_mncl <- cbind(this_mncl, NA)
  }
  colnames(this_mncl) <- paste0("V", 1:12)
  this_id <- as.integer(substr(loaded_sim_names[i], 16, 18))
  this_mncl <- data.frame(id = id_nums[i], cluster = letters[1:nrow(this_mncl)], this_mncl)
  sim_cl_means <- rbind(sim_cl_means, this_mncl)
}
rownames(sim_cl_means) <- NULL
sim_cl_means
#write.csv(sim_cl_means, "./apps/simulation/sim_cl_means.csv", row.names = FALSE)


### CREATE SIM_COVAR_LDA.CSV -----
# grain: sim*variable, p
sim_covar_lda <- NULL
for (i in 1:length(loaded_sim_names)) {
  # init
  this_sim <- get(loaded_sim_names[i])
  colnames(this_sim) <- paste0("V", 1:ncol(this_sim))
  # covar
  this_covar <- attr(this_sim, "vc")
  while (ncol(this_covar) < 12) { # force columns to 12, the max(p)
    this_covar <- cbind(this_covar, NA)
  }
  colnames(this_covar) <- paste0("V", 1:12)
  this_covar <- data.frame(id = i, var = paste0("V", 1:nrow(this_covar)), this_covar)
  # LDA
  this_supervied_sim <- data.frame(this_sim, cluster = attr(this_sim, "cluster"))
  this_lda <- MASS::lda(cluster~., data = this_supervied_sim)
  this_lda_df <- this_lda$scaling
  while (ncol(this_lda_df) < 3) { # force columns to 3, the max(lda$scaling)
    this_lda_df <- cbind(this_lda_df, NA)
  }
  colnames(this_lda_df) <- paste0("LD", 1:3)
  
  this_covar_lda <- data.frame(this_covar, this_lda_df)
  sim_covar_lda <- rbind(sim_covar_lda, this_covar_lda)
}
rownames(sim_covar_lda) <- NULL
sim_covar_lda
#write.csv(sim_covar_lda, "./apps/simulation/sim_covar_lda.csv", row.names = F)


### CREATE SIM_PARAMETERS.CSV -----
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
  this_parameters <- data.frame(id = i, p, n_cl, pnoise)
  sim_parameters <- rbind(sim_parameters, this_parameters)
}
rownames(sim_parameters) <- NULL
sim_parameters
#write.csv(sim_parameters, "./apps/simulation/sim_parameters.csv", row.names = FALSE)


### CREATE SIM_TASK2_ANS_SCORE.CSV -----
sim_task2_ans <- NULL
for (i in 1:length(loaded_sim_names)) {
  # init
  this_sim <- get(loaded_sim_names[i])
  colnames(this_sim) <- paste0("V", 1:ncol(this_sim))
  
  this_supervied_sim <- data.frame(this_sim, cluster = attr(this_sim, "cluster"))
  lda_means <- MASS::lda(cluster~., data = this_supervied_sim)$means
  
  abs_lda_means <- matrix(NA, nrow = 2, ncol = p())
  abs_lda_means[1, ] <- abs(lda_means[1, ] - lda_means[2, ])
  abs_lda_means[2, ] <- abs(lda_means[2, ] - lda_means[3, ])
  
  ans <- matrix(NA, nrow = 2, ncol = p)
  for (i in 1:2){
    this_row_ptile <- abs_lda_means[i, ] / max(abs_lda_means[i, ])
    this_row_ans   <- dplyr::case_when(this_row_ptile >= .75 ~ 2, # very
                                       this_row_ptile >= .25 ~ 1, # some
                                       this_row_ptile >=   0 ~ 0)
    ans[i, ] <- this_row_ans
  }

}
rownames(sim_task2_ans) <- NULL
sim_task2_ans
#write.csv(sim_task2_ans_score, "./apps/simulation/sim_task2_ans_score.csv", row.names = F)


# # LDA Components not useful for seperating bewteen 2 clusters.
# ### SIMULATION RATING -----
# # grain: sim*vairable
# df_sim_ratings <- NULL
# for (i in 1:length(loaded_sim_names)) {
#   # init
#   this_sim <- get(loaded_sim_names[i])
#   colnames(this_sim) <- paste0("V", 1:ncol(this_sim))
#   # parameters
#   p <- ncol(this_sim)
#   n_cl <- length(attr(this_sim, "ncl"))
#   pnoise <- sum(colSums(attr(this_sim, "mncl") == 0) == n_cl)
#   # LDA
#   this_supervied_sim <- data.frame(this_sim, cluster = attr(this_sim, "cluster"))
#   this_lda <- MASS::lda(cluster~., data = this_supervied_sim)
#   ## LDA - linear combinations
#   this_lda_scaling <- this_lda$scaling
#   while (ncol(this_lda_scaling) < 3) { # force columns to 3, max(#LD) = max(n_cl) - 1
#     this_lda_scaling <- data.frame(this_lda_scaling, LD3 = NA)
#   }
#   ## LDA - % of variance 
#   this_prop_var <- this_lda$svd^2 / sum(this_lda$svd^2) # geting to "Proportion of trace:", really propotion of variances
#   while (length(this_prop_var) < 3) { # force columns to 3, max(#LD) = max(n_cl) - 1
#     this_prop_var <- c(this_prop_var, NA)
#   }
#   this_prop_var <- data.frame(t(matrix(this_prop_var)))
#   colnames(this_prop_var) <- paste0("LD", 1:3, "_prop_var")
#   # colate
#   this_sim_ratings <- data.frame(id = i, var = paste0("V", 1:ncol(this_sim)), 
#                                   p, n_cl, pnoise, this_prop_var, this_lda_scaling,
#                                   var_rating = NA, var_corr = NA)
#   print(dim(this_sim_ratings))
#   df_sim_ratings <- rbind(df_sim_ratings, this_sim_ratings)
# }
# rownames(df_sim_ratings) <- NULL
# sim_ratings <- df_sim_ratings
# sim_ratings
# #write.csv(sim_ratings, "./apps/simulation/sim_ratings.csv", row.names = FALSE)


# ### _scratch GROUND TRUTH ----
# ### task 1:
# (blk1 <- sim_parameters[c(21, 17, 4, 20), c(1, 3)])
# 
### __task 2: -----
ex <- readRDS("./apps/simulation/simulation_data101.rds")

this_sim <- ex #get(loaded_sim_names[i])
colnames(this_sim) <- paste0("V", 1:ncol(this_sim))
# covar
this_covar <- attr(this_sim, "vc")
while (ncol(this_covar) < 14) { # force columns to 14, max(p)
  this_covar <- cbind(this_covar, NA)
}
colnames(this_covar) <- paste0("V", 1:14)
this_covar <- data.frame(id = "ex", var = paste0("V", 1:nrow(this_covar)), this_covar)
# LDA
this_supervied_sim <- data.frame(this_sim, cluster = attr(this_sim, "cluster"))
this_lda <- MASS::lda(cluster~., data = this_supervied_sim)
this_lda$scaling
this_lda$means
this_cl_means <- attr(this_sim, "mncl")[, attr(this_sim, "col_reorder")]
#print("scaling (var grain) is on a different grainularity from means (cluster grain), cannot combine.")
print("manually looking at sim 021: cluster a -- v important: V3, somewhat important: 2, 4, 6, 1")
response <- c(1, 1, 2, 1, 0, 1)
(this_row <- this_lda$means[1,])
this_row_abs <- abs(this_lda$means[1,])
(this_row_ptile <- this_row_abs / max(this_row_abs))
print("this makes sense, but seems like the lda means are a fair bit off from the parameter.")
dplyr::case_when(
  this_row_ptile >= .75 ~ "very",
  this_row_ptile >= .25 ~ "somewhat",
  this_row_ptile >= 0 ~ "not"
)
(ans <- dplyr::case_when(
  this_row_ptile >= .75 ~ 2,
  this_row_ptile >= .25 ~ 1,
  this_row_ptile >= 0 ~ 0
))
(score <- -1 * sum((response - ans)^2))



