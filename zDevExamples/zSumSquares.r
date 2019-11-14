load_name <- "simulation_data021"
local_file <- paste0("./apps/simulation/", load_name, ".rds")
assign(load_name, readRDS(local_file))

d <- get(load_name)
colnames(d) <- paste0("V", 1:ncol(d))
p <- ncol(d)
n_cl <- length(attr(d, "ncl"))
reord <- attr(d, "col_reorder")
mncl <- attr(d, "mncl")[, reord]
rownames(mncl) <- paste0("cl ", letters[1:n_cl])
colnames(mncl) <- paste0("V", 1:p)

#mncl_nonnoise <- mncl[, (colSums(attr(d, "mncl") == 0) != n_cl)[reord]] # remove zeros
cl_mn <- rowMeans(mncl)
cl_mn
sum_sq <- apply(mncl, 2, function(x) {sum((x - cl_mn)^2)}) # sum of squares
ord <- order(as.numeric(sum_sq), decreasing = T)
sum_sq <- sum_sq[ord]

df <- rbind(sum_sq, rank = as.integer(1:length(sum_sq))) # double check rate wasn't correct at one point.
df
