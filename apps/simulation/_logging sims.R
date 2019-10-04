## scratchpad for making simulation csv files.

### SCRATCH
#write.csv(mtcars, "mtcars.csv")
sim1 <- readRDS("./apps/simulation/simulation_data001.rds") 
str(sim1)
mncl<-attr(sim1, "mncl")
colnames(mncl) <- paste0("V", 1:10)
mncl

### LOAD ALL SIM
load_num <- 1
load_name <- sprintf("simulation_data%03d", load_num)
load_file <- paste0("./apps/simulation/", load_name, ".rds") # "./apps/simulation/"
#local_file <- paste0("./apps/simulation/", load_name, ".rds"); assign(load_name, readRDS(local_file))
while (file.exists(load_file)){
  assign(load_name, readRDS(load_file))
  load_num <- load_num + 1
  load_name <- sprintf("simulation_data%03d", load_num)
  load_file <- paste0("./apps/simulation/", load_name, ".rds")
}
loaded_sim_names <- ls()[grepl("simulation_data", ls())]

### CREATE SIM_CL_MEANS.CSV
df_mncl <- NULL
for (i in 1:length(loaded_sim_names)) {
  this_sim <- get(loaded_sim_names[i])
  this_reorder <- attr(this_sim, "col_reorder")
  this_mncl <- attr(this_sim, "mncl")[, this_reorder]
  while (ncol(this_mncl) < 14) {
    this_mncl <- cbind(this_mncl, NA)
  }
  colnames(this_mncl) <- paste0("V", 1:14)
  this_mncl <- data.frame(id = i, cluster = letters[1:nrow(this_mncl)], this_mncl)
  df_mncl <- rbind(df_mncl, this_mncl)
}
df_mncl
rownames(df_mncl) <- NULL
sim_cl_means <- df_mncl
write.csv2(sim_cl_means, "sim_cl_means.csv")

### CREATE SIM_COVAR.CSV
df_covar <- NULL
for (i in 1:length(loaded_sim_names)) {
  this_sim <- get(loaded_sim_names[i])
  this_reorder <- attr(this_sim, "col_reorder")
  this_covar <- attr(this_sim, "vc")[this_reorder, this_reorder]
  while (ncol(this_covar) < 14) {
    this_covar <- cbind(this_covar, NA)
  }
  colnames(this_covar) <- paste0("V", 1:14)
  this_covar <- data.frame(id = i, var = paste0("V", 1:nrow(this_covar)), this_covar)
  df_covar <- rbind(df_covar, this_covar)
}
df_covar
rownames(df_covar) <- NULL
sim_covar <- df_covar
write.csv2(sim_covar, "sim_covar.csv")

