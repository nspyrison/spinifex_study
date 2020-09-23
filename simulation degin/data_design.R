# experimental design
library(tidyverse)
library(mvtnorm)
library(GGally)
library(tourr)
library(spinifex)

# Utility functions
rmvnorm_tsb <- function(n=100, mean, sigma) {
  d <- rmvnorm(n, mean, sigma)
  colnames(d) <- paste0("x", 1:ncol(d))
  d <- as_tibble(d)
  return(d)
}
stdd <- function(x) {
  (x - mean(x))/sd(x)
}

# Generate data: BE SURE TO GENERATE MEANS AND SIGMAS FIRST
set.seed <- 6446
nobs <- 120 # Easier if this is a multiple of 5
df <- map2(m_l, vc_l, ~rmvnorm_tsb(n=nobs, as.matrix(.x), 
                                   as.matrix(.y)))
d <- df %>% enframe(name="cl") %>% unnest(value) %>%
  mutate_if(is.numeric, stdd)

#ggplot(d, aes(x=x1, y=x2, colour=cl)) + geom_point()
ggscatmat(d, columns=2:5, col = "cl")

d_pca <- prcomp(d[,2:5])
d_pcs <- as_tibble(d_pca$x)
d_pcs$cl <- d$cl
ggscatmat(d_pcs, columns=1:4, col = "cl")

# Generate means
# Assuming focus is going to be on difference between 
# cluster 1 and 2
m <- tibble(cl = factor(c("a", "b", "c")), 
            x1 = 2*(c(0, 1, 0)-0.5), 
            x2 = 2*(c(0, 0, 1)-0.5),
            x3 = c(0, 0, 0),
            x4 = c(0, 0, 0))
m_l <- m %>% group_by(cl) %>% group_split(.keep=FALSE)
names(m_l) <- m$cl

# Generate variances
# Equal elliptical
vc1 <- matrix(c(1,0.9,0,0,
                0.9,1,0,0,
                0,0,1,0,
                0,0,0,1), ncol=4, byrow=TRUE)
vc_l <- lst()
vc_l[[1]] <- vc1
vc_l[[2]] <- vc1
vc_l[[3]] <- vc1
names(vc_l) <- m$cl

# Covariance in noise variables
vc1 <- matrix(c(1,0.9,0,0,
                0.9,1,0,0,
                0,0,1,0.8,
                0,0,0.8,1), ncol=4, byrow=TRUE)
vc_l <- lst()
vc_l[[1]] <- vc1
vc_l[[2]] <- vc1
vc_l[[3]] <- vc1
names(vc_l) <- m$cl

# Variable elliptical
vc1 <- matrix(c(1,0.9,0,0,
                0.9,1,0,0,
                0,0,1,0,
                0,0,0,1), ncol=4, byrow=TRUE)
vc3 <- matrix(c(1,-0.9,0,0,
                -0.9,1,0,0,
                0,0,1,0,
                0,0,0,1), ncol=4, byrow=TRUE)
vc_l <- lst()
vc_l[[1]] <- vc1
vc_l[[2]] <- vc1
vc_l[[3]] <- vc3
names(vc_l) <- m$cl

# covariance in noise variables
vc1 <- matrix(c(1,0.9,0,0,
                0.9,1,0,0,
                0,0,1,0.8,
                0,0,0.8,1), ncol=4, byrow=TRUE)
vc3 <- matrix(c(1,-0.9,0,0,
                -0.9,1,0,0,
                0,0,1,0.8,
                0,0,0.8,1), ncol=4, byrow=TRUE)
vc_l <- lst()
vc_l[[1]] <- vc1
vc_l[[2]] <- vc1
vc_l[[3]] <- vc3
names(vc_l) <- m$cl

# Banana shape
vc1 <- 0.2*matrix(c(1,0,0,0, # spherical
                    0,1,0,0,
                    0,0,1,0,
                    0,0,0,1), ncol=4, byrow=TRUE)
vc2 <- 0.05*matrix(c(1,0,0,0, # First make spherical and 
                     0,1,0,0, # then shift means
                     0,0,1,0, # to make banana
                     0,0,0,1), ncol=4, byrow=TRUE)
vc3 <- matrix(c(1,-0.9,0,0,
                -0.9,1,0,0,
                0,0,1,0,
                0,0,0,1), ncol=4, byrow=TRUE)
vc_l <- lst()
vc_l[[1]] <- vc1
vc_l[[2]] <- vc2
vc_l[[3]] <- vc3
names(vc_l) <- m$cl

# Now make banana
# Run the code on lines 19-23 first
d[(nobs+1):(nobs+1+nobs/5), 2] <- d[(nobs+1):(nobs+1+nobs/5), 2] - 0.5
d[(nobs+1):(nobs+1+nobs/5), 3] <- d[(nobs+1):(nobs+1+nobs/5), 3] + 0.5
d[(nobs+1+nobs/5+1):(nobs+1+2*nobs/5), 2] <- d[(nobs+1+nobs/5+1):(nobs+1+2*nobs/5), 2] - 0.5
d[(nobs+1+nobs/5+1):(nobs+1+2*nobs/5), 3] <- d[(nobs+1+nobs/5+1):(nobs+1+2*nobs/5), 3] - 0.5
d[(nobs+1+2*nobs/5+1):(nobs+1+3*nobs/5), 2] <- d[(nobs+1+2*nobs/5+1):(nobs+1+3*nobs/5), 2] - 1
d[(nobs+1+2*nobs/5+1):(nobs+1+3*nobs/5), 3] <- d[(nobs+1+2*nobs/5+1):(nobs+1+3*nobs/5), 3] + 1
d[(nobs+1+3*nobs/5+1):(nobs+1+4*nobs/5), 2] <- d[(nobs+1+3*nobs/5+1):(nobs+1+4*nobs/5), 2] - 1
d[(nobs+1+3*nobs/5+1):(nobs+1+4*nobs/5), 3] <- d[(nobs+1+3*nobs/5+1):(nobs+1+4*nobs/5), 3] - 1

# Make mean difference a combination of variables
# Focus on linear combination of V1 and V3
# Run code on lines 19-23 first
d_rot <- d %>%
  mutate(x1 = 0.707*x1 + 0.707*x3,
         x3 = -0.707*x1 + 0.707*x3,
         x2 = 0.707*x2 + 0.707*x4,
         x4 = -0.707*x2 + 0.707*x4)

ggscatmat(d_rot, columns=2:5, col = "cl")

d_pca <- prcomp(d_rot[,2:5]) # Should produce the same PCs 
d_pcs <- as_tibble(d_pca$x) # as unrotated except for loadings
d_pcs$cl <- d$cl
ggscatmat(d_pcs, columns=1:4, col = "cl")

# Add an outlier or two
# Goal is that the mean of the clusters is now the same
# NOTE: change idea, to force mean, need to have very low values
#       better to just add some outliers and see if distracts
d %>% group_by(cl) %>%
  summarise_if(is.numeric, mean)

d_out <- d %>% 
  bind_rows(tibble(cl=c("c","c","c"), 
                   x1=c(1.05,0.9,1.15), 
                   x2=c(-0.5, -0.45, -0.55), 
                   x3=c(0.01,0.03,-0.01), 
                   x4=c(-0.02,0,0.03)))

d_out %>% group_by(cl) %>%
  summarise_if(is.numeric, mean)

ggscatmat(d_out, columns=2:5, col = "cl")

d_pca <- prcomp(d_out[,2:5]) # Should produce the same PCs 
d_pcs <- as_tibble(d_pca$x) # as unrotated except for loadings
d_pcs$cl <- d_out$cl
ggscatmat(d_pcs, columns=1:4, col = "cl")

# Check manual tour: code not working - maybe need update to spinifex
dat <- d
bas <- basis_pca(dat)
mv <- manip_var_pca(dat)
clas <- attr(dat, "cl_lvl")
play_manual_tour(bas, dat, mv, aes_args = list(color = clas, shape = clas))
