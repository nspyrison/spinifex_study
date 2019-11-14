## scratchpad for making simulation csv files.

### zSim example -----
#write.csv(mtcars, "mtcars.csv")
ex<- readRDS("./apps/simulation/simulation_data001.rds") 
str(ex)


### zLDA example -----
# grain: sim*variable, p ; join with var-covar
# https://towardsdatascience.com/linear-discriminant-analysis-lda-101-using-r-6a97217a55a6
?MASS::lda
dat <- tourr::flea
lda <- MASS::lda(species~., data = dat)
str(lda)
# return what? the coeffiecients of the var that make LD1, LD2?
lda$scaling
cat <- tourr::flea$species
spinifex::view_basis(basis = lda$scaling, data = dat[, 1:6], col = cat, pch = cat)
# !! But the scaling is not a basis, it's p x (cl-1). compare with lda_pp?
dat <- get(loaded_sim_names[2])
colnames(dat) <- paste0("V", 1:ncol(dat))
dat_reorder <- attr(dat, "col_reorder")
supervied_dat <- data.frame(dat, cluster = attr(dat, "cluster"))
lda <- MASS::lda(cluster~., data = supervied_dat)
lda$scaling
cat <- supervied_dat$cluster
spinifex::view_basis(basis = lda$scaling, data = dat, col = cat, pch = cat)
# LD3 seems to be ignored. let's look at lda_pp
tpath    <- save_history(dat, tour_path = lda_pp(cl = attr(dat, "cluster")))
save_history(flea[, 1:6], lda_pp(flea$species))
animate_xy(flea[, 1:6], lda_pp(flea$species))


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
# grain: sim*cluster, cl
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
#write.csv(sim_cl_means, "./apps/simulation/sim_cl_means.csv", row.names = FALSE)


### CREATE SIM_COVAR_LDA.CSV -----
# grain: sim*variable, p
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
  this_lda
  this_lda$scaling #TODO: do i need to force to 3 LD col? check .csv file.
  this_covar_lda <- data.frame(this_covar, this_lda$scaling)
  df_covar_lda <- rbind(df_covar_lda, this_covar_lda)
}
df_covar_lda
rownames(df_covar_lda) <- NULL
sim_covar_lda <- df_covar_lda
#write.csv(sim_covar_lda, "./apps/simulation/sim_covar_lda.csv", row.names = F) # not rounded.


### CREATE SIM_PARAMETERS.CSV -----
# grain: simulation
df_parameters <- NULL
for (i in 1:length(loaded_sim_names)) {
  this_sim <- get(loaded_sim_names[i])
  this_reorder <- attr(this_sim, "col_reorder")
  this_covar <- attr(this_sim, "vc")[this_reorder, this_reorder]
  # parameters
  p <- ncol(this_sim)
  n_cl <- length(attr(this_sim, "ncl"))
  pnoise <- sum(colSums(attr(this_sim, "mncl") == 0) == n_cl)
  # colate
  this_parameters <- data.frame(id = i, p, n_cl, pnoise)
  df_parameters <- rbind(df_parameters, this_parameters)
}
df_parameters
rownames(df_parameters) <- NULL
sim_parameters <- df_parameters
#write.csv(sim_parameters, "./apps/simulation/sim_parameters.csv", row.names = FALSE)


# ### MAHALANOBIS DIST -----
# # grain: sim*observations, could sum to cluster
# # https://stat.ethz.ch/R-manual/R-devel/library/stats/html/mahalanobis.html
# ?mahalanobis
# colnames(ex) <- paste0("V", 1:ncol(ex))
# rownames(ex) <- NULL
# dim(ex)
# mn <- apply(ex, 2, mean)
# cv <- var(ex)
# ex_ma <- mahalanobis(ex, mn, cv)
# length(ex_ma) # vector of length n.
# # I don't think mahalanobis dist is the right approach to rating variable 
# # importance, it's really a measure of the distance of an obs away from the 
# # multivariate mean, rather than a varable wise rating
# # 
# # Looking at magnitude of LDA, gives us an idea of importance,
# # but again doesn't penalize for covariance. 
# 
# # from help example:
# x <- matrix(rnorm(100*3), ncol = 3) # 100r x 3c
# Sx <- cov(x)
# D2 <- mahalanobis(x, colMeans(x), Sx) # 100 distances
# length(D2) # obs; ~ std dev dist away from multivariate mean?


### SIMULATION RATING -----
# grain: sim*vairable
df_sim_ratings <- NULL
for (i in 1:length(loaded_sim_names)) {
  # init
  this_sim <- get(loaded_sim_names[i])
  colnames(this_sim) <- paste0("V", 1:ncol(this_sim))
  # parameters
  p <- ncol(this_sim)
  n_cl <- length(attr(this_sim, "ncl"))
  pnoise <- sum(colSums(attr(this_sim, "mncl") == 0) == n_cl)
  # LDA
  this_supervied_sim <- data.frame(this_sim, cluster = attr(this_sim, "cluster"))
  this_lda <- MASS::lda(cluster~., data = this_supervied_sim)
  ## LDA - linear combinations
  this_lda_scaling <- this_lda$scaling
  while (ncol(this_lda_scaling) < 3) { # force columns to 3, max(#LD) = max(n_cl) - 1
    this_lda_scaling <- data.frame(this_lda_scaling, LD3 = NA)
  }
  ## LDA - % of variance 
  this_prop_var <- this_lda$svd^2 / sum(this_lda$svd^2) # geting to "Proportion of trace:", really propotion of variances
  while (length(this_prop_var) < 3) { # force columns to 3, max(#LD) = max(n_cl) - 1
    this_prop_var <- c(this_prop_var, NA)
  }
  this_prop_var <- data.frame(t(matrix(this_prop_var)))
  colnames(this_prop_var) <- paste0("LD", 1:3, "_prop_var")
  # colate
  this_sim_ratings <- data.frame(id = i, var = paste0("V", 1:ncol(this_sim)), 
                                  p, n_cl, pnoise, this_prop_var, this_lda_scaling,
                                  var_rating = NA, var_corr = NA)
  print(dim(this_sim_ratings))
  df_sim_ratings <- rbind(df_sim_ratings, this_sim_ratings)
}
rownames(df_sim_ratings) <- NULL
sim_ratings <- df_sim_ratings
sim_ratings
#write.csv(sim_ratings, "./apps/simulation/sim_ratings.csv", row.names = FALSE)

### ANALYSIS/SELECTING SIMULATIONS -----
library("tidyverse")
sim_ratings <- read.csv("./apps/simulation/sim_ratings.csv")
str(sim_ratings)
table(sim_ratings$p)
table(sim_ratings$n_cl) # only 3, 4
table(sim_ratings$pnoise)
ggplot(sim_ratings, aes(x = LD1_prop_var)) + geom_histogram() # high end are out, too simple.
ggplot(sim_ratings, aes(x = LD1_prop_var, y = LD2_prop_var, col = factor(n_cl))) + 
  #geom_point() + # 3 sims at bottom right are out, too simple to see in 2d.
  #would they be better example than sim 21?
  geom_text(mapping = aes(label = id))
  # note that this may help pick simulations, but doesn't give ground truth for
  # blocks 2 and 3.
## manually looking at 9,11,12 i don't think they are really too simple.

# sim 17 is a canidate for the hardest 3 cluster data set 
# as it has the lowest LD1_prop_var.


# 17, might make a good first rep, check if sim 4, 20 are too hard. 
# thinking training: 21, reps: 17, 4, 20 respectively, 
# consider simulating from 5 and 6 clusters as well.

sim_parameters <- read.csv("./apps/simulation/sim_parameters.csv")
#sim_parameters
sim_parameters[c(21, 17, 4, 20), ]

### _GROUND TRUTH ----
# block 1:
(blk1 <- sim_parameters[c(21, 17, 4, 20), c(1, 3)])
# block 2:

ex <- readRDS("./apps/simulation/simulation_data021.rds") 

df_covar_lda <- NULL

this_sim <- ex #get(loaded_sim_names[i])
colnames(this_sim) <- paste0("V", 1:ncol(this_sim))
this_reorder <- attr(this_sim, "col_reorder")
# covar
this_covar <- attr(this_sim, "vc")[this_reorder, this_reorder]
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
print("scaling (var grain) is on a different grainularity from means (cluster grain), cannot combine.")
print("manually looking at sim 021, it looks like V4 is important for distinguishing 
      1 group(harder to see). and V2, V6, V3 distinguish the easier group to split from the other 2.")

m <- this_lda$means
