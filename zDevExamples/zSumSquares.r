load_name <- "simulation_data001"
local_file <- paste0("./apps/simulation/", load_name, ".rds")
assign(load_name, readRDS(local_file))

d <- simulation_data001
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
rank <- order(sum_sq, decreasing = T)

df <- rbind(sum_sq, rank) # double check rank, wasn't correct at one point.
df
