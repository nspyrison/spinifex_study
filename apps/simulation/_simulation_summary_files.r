## scratchpad for making simulation csv files.

### SCRATCH -----
#write.csv(mtcars, "mtcars.csv")
ex<- readRDS("./apps/simulation/simulation_data001.rds") 
str(ex)


### LOAD ALL SIM -----
load_num <- 1
load_name <- sprintf("simulation_data%03d", load_num)
load_file <- paste0("./apps/simulation/", load_name, ".rds")
while (file.exists(load_file)){
  assign(load_name, readRDS(load_file))
  load_num <- load_num + 1
  load_name <- sprintf("simulation_data%03d", load_num)
  load_file <- paste0("./apps/simulation/", load_name, ".rds")
}
loaded_sim_names <- ls()[grepl("simulation_data", ls())]


### CREATE SIM_CL_MEANS.CSV -----
# grain: cluster, cl
df_mncl <- NULL
for (i in 1:length(loaded_sim_names)) {
  this_sim <- get(loaded_sim_names[i])
  this_reorder <- attr(this_sim, "col_reorder")
  this_mncl <- attr(this_sim, "mncl")[, this_reorder]
  while (ncol(this_mncl) < 14) { # force columns to 14, max(p)
    this_mncl <- cbind(this_mncl, NA)
  }
  colnames(this_mncl) <- paste0("V", 1:14)
  this_mncl <- data.frame(id = i, cluster = letters[1:nrow(this_mncl)], this_mncl)
  df_mncl <- rbind(df_mncl, this_mncl)
}
df_mncl
rownames(df_mncl) <- NULL
sim_cl_means <- df_mncl
write.csv2(sim_cl_means, "./apps/simulation/sim_cl_means.csv", row.names = FALSE)



### CREATE SIM_COVAR_LDA.CSV -----
# grain: variable, p
df_covar_lda <- NULL
for (i in 1:length(loaded_sim_names)) {
  # init
  this_sim <- get(loaded_sim_names[i])
  colnames(this_sim) <- paste0("V", 1:ncol(this_sim))
  this_reorder <- attr(this_sim, "col_reorder")
  # covar
  this_covar <- attr(this_sim, "vc")[this_reorder, this_reorder]
  while (ncol(this_covar) < 14) { # force columns to 14, max(p)
    this_covar <- cbind(this_covar, NA)
  }
  colnames(this_covar) <- paste0("V", 1:14)
  this_covar <- data.frame(id = i, var = paste0("V", 1:nrow(this_covar)), this_covar)
  # LDA
  this_supervied_sim <- data.frame(this_sim, cluster = attr(this_sim, "cluster"))
  this_lda <- MASS::lda(cluster~., data = this_supervied_sim)
  this_lda$scaling #TODO: do i need to force to 3 LD col? check .csv file.
  this_covar_lda <- data.frame(this_covar, this_lda$scaling)
  df_covar_lda <- rbind(df_covar_lda, this_covar_lda)
}
df_covar_lda
rownames(df_covar_lda) <- NULL
sim_covar_lda <- df_covar_lda
write.csv2(sim_covar_lda, "./apps/simulation/sim_covar_lda.csv", 
           row.names = FALSE) # not rounded.


### CREATE SIM_PARAMETERS.CSV -----
# grain: simulation
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
write.csv2(sim_parameters, "./apps/simulation/sim_parameters.csv", row.names = FALSE)


### LDA -----
# grain: variable, p ; join with var-covar
# https://towardsdatascience.com/linear-discriminant-analysis-lda-101-using-r-6a97217a55a6
?MASS::lda
dat <- tourr::flea
lda <- MASS::lda(species~., data = dat)
str(lda)
# return what? the coeffiecients of the var that make LD1, LD2?
lda$scaling
cat <- tourr::flea$species
spinifex::view_basis(basis = lda$scaling, data = dat[, 1:6], col = cat, pch = cat)

### MAHALANOBIS DIST -----
# grain: observations, could sum to cluster
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/mahalanobis.html
?mahalanobis
colnames(ex) <- paste0("V", 1:ncol(ex))
rownames(ex) <- NULL
dim(ex)
mn <- apply(ex, 2, mean)
cv <- var(ex)
ex_ma <- mahalanobis(ex, mn, cv)
length(ex_ma) # vector of length n.
# I don't think mahalanobis dist is the right approach to a variable rank,
# it's really a measure of the distance of an obs away from the 
# multivariate mean, rather than a varable wise ranking.

# from help example:
x <- matrix(rnorm(100*3), ncol = 3) # 100r x 3c
Sx <- cov(x)
D2 <- mahalanobis(x, colMeans(x), Sx) # 100 distances
length(D2) # obs; ~ std dev dist away from multivariate mean?

