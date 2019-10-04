## scratchpad for making simulation csv files.

### SCRATCH -----
#write.csv(mtcars, "mtcars.csv")
sim1 <- readRDS("./apps/simulation/simulation_data001.rds") 
str(sim1)
mncl<-attr(sim1, "mncl")
colnames(mncl) <- paste0("V", 1:10)
mncl


### LOAD ALL SIM -----
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


### CREATE SIM_CL_MEANS.CSV -----
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


### CREATE SIM_PARAMETERS.CSV -----
df_parameters <- NULL
for (i in 1:length(loaded_sim_names)) {
  this_sim <- get(loaded_sim_names[i])
  this_reorder <- attr(this_sim, "col_reorder")
  this_covar <- attr(this_sim, "vc")[this_reorder, this_reorder]
  
  p <- ncol(this_sim)
  n_cl <- length(attr(this_sim, "ncl"))
  pnoise <- sum(colSums(attr(this_sim, "mncl") == 0) == n_cl)
  
  this_parameters <- data.frame(id = i, p, n_cl, pnoise)
  df_parameters <- rbind(df_parameters, this_parameters)
}
df_parameters
rownames(df_parameters) <- NULL
sim_parameters <- df_parameters
write.csv2(sim_parameters, "sim_parameters.csv")


### LDA scratchpad -----
# https://towardsdatascience.com/linear-discriminant-analysis-lda-101-using-r-6a97217a55a6
dat <- tourr::flea
# f <- paste(names(dat)[7], "~", paste(names(dat)[-7], collapse=" + "))
# MASS::lda(as.formula(f), data = dat)
lda <- MASS::lda(species~., data = dat)
# return what? the coeffiecients of the var that make LD1, LD2?
str(lda)
lda$scaling


###  MAHALONOBIS DIST -----
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/mahalanobis.html
?mahalanobis
colnames(sim1) <- paste0("V", 1:ncol(sim1))
rownames(sim1) <- NULL
mn <- apply(sim1, 2, mean)
cv <- var(sim1)
mahalanobis(sim1, mn, cv)

