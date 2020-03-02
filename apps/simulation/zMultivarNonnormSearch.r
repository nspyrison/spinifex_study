library("fungible")


## Generate dimensional data for 4 variables. 
## All correlations = .60; all variable
## skewness = 1.75; 
## all variable kurtosis = 3.75

cormat <- matrix(.60,4,4)
diag(cormat) <- 1

nontaxon.dat <- monte1(seed = 123, nsub = 100000, nvar = 4, skewvec = rep(1.75, 4),
                       kurtvec = rep(3.75, 4), cormat = cormat)

print(cor(nontaxon.dat$data), digits = 3)
print(apply(nontaxon.dat$data, 2, skew), digits = 3)
print(apply(nontaxon.dat$data, 2, kurt), digits = 3)         

dat <- nontaxon.dat$data

pca_dat <- prcomp(dat)
pca_rot_dat <- pca_dat$rotation

pca_dat$x
plot(pca_dat$x[,1], pca_dat$x[,2])


help("fungible")
