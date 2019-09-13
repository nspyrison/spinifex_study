# Load libraries
library(mvtnorm)

# Simulating clusters
# Clusters
p <- 10 #sample(5:8, 1)
pnoise <- 4
cl <- 4 #sample(5:9,1)
x <- NULL
ncl <- NULL
mncl <- NULL
for (i in 1:cl) {
  n <- sample(30:150, 1)
  vc <- matrix(sample(seq(-0.1, 0.7, 0.1), 1), p, p) # Add some association
  diag(vc) <- 1
  mn <- c(sample(seq(-3, 3, 1), p-pnoise, replace=T), rep(0, pnoise))
  x <- rbind(x, rmvnorm(n=n, mean=mn, vc))
  ncl <- c(ncl, n)
  mncl <- rbind(mncl, mn)
}
x <- scale(x)
x <- as.data.frame(x)
ncl # Sizes of the clusters, and clusters are sequential row numbers
mncl # cluster means

# Check data
library(tourr)
quartz()
animate_xy(x, axes="bottomleft")
animate_xy(x, axes="bottomleft", guided_tour(holes()), sphere = TRUE)

# Show colour on plots to check clustering
library(RColorBrewer)
classes <- factor(rep(c("a", "b", "c", "d"), ncl))
clrs <- brewer.pal(4, "Dark2")
col <- clrs[as.numeric(classes)]
animate_xy(x, axes="bottomleft", col=col)
animate_xy(x, axes="bottomleft", guided_tour(holes()), sphere = TRUE, col=col)

# Check PCA
x_pca <- prcomp(x, retx=TRUE)
screeplot(x_pca, type="l")
biplot(x_pca)

# Check clustering
x_cl <- hclust(dist(x), method="ward.D2")
plot(x_cl)

# Manual tour
library(spinifex)
# Get PP max
tour_hist <- save_history(x, guided_tour(holes()))
tour_len <- dim(tour_hist)[3]
proj <- matrix(as.numeric(tour_hist[,, tour_len]), ncol = 2)
play_manual_tour(basis = proj, data = x, manip_var = 1, 
                 axes = "bottomleft") #, 
                 #render_type = render_gganimate)


#---------
# Need to disguise the important variables, so its not just the first few
# similarly should not have the clusters all in order
# Scramble variable order - code needs ot be written


# Scramble order of cases
cl <- rep(1:10, c(ncl, 111))
x.indx <- sample(1:nrow(x))
x <- x[x.indx,]
cl <- cl[x.indx]

# Keep track of clusters through scrambling
mycl2 <- rep(1, nrow(x))
for (i in 2:cl) 
  mycl2[x.indx[(ncl[i-1]+1):ncl[i]]] <- i
#---------

# Don't think this extra code is necessary
# Add outliers
n<-sample(80:120, 1)
y<-matrix((runif(n*ncol(x))-0.5)*2, ncol=ncol(x))
colnames(y) <- paste("V", 1:ncol(y), sep="")
x<-rbind(x,y)

animate_xy(data.frame(x))

# AND make some variables need transformations
x<-data.frame(x)
x$V2 <- log(x$V2+3)
x$V1 <- exp(x$V1+2)
x<-scale(x)




