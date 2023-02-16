####
# Visualize PR curves and inferred networks on synthetic data against true networks 
# Use the synthetic data generator from SpiecEasi.
# Infer with any suitable tool. As example, SpiecEasi is used here.
####


library(SpiecEasi)
library(PRROC)
require(igraph)

source("colored_union.R")



# Load and filter the data set provided in the SpiecEasi library
data(amgut1.filt)
depths <- rowSums(amgut1.filt)
amgut1.filt.n  <- t(apply(amgut1.filt, 1, norm_to_total))
amgut1.filt.cs <- round(amgut1.filt.n * min(depths))

d <- ncol(amgut1.filt.cs)
n <- nrow(amgut1.filt.cs)
e <- d

# Generate graph topology and correlation matrix
set.seed(10010)
# graph will store the true network
graph <- SpiecEasi::make_graph('cluster', d, e)
Prec  <- graph2prec(graph)
Cor   <- cov2cor(prec2cov(Prec))

# Generate the synthetic OTU table
X <- synth_comm_from_counts(amgut1.filt.cs, mar=2, distr='zinegbin', Sigma=Cor, n=n)



# Infer network on the synthetic data with SpiecEasi mb (neighborhood selection)
se.mb <- spiec.easi(X, method='mb', lambda.min.ratio=1e-2, nlambda=15)

# Alternatively, you can previously load objects of type se.mb and load them here



###
# PR curve; ROC curve
###


# Plot PR curve with tool from SpiecEasi package. I did NOT use this tool in the thesis
stars.pr(getOptMerge(se.mb), graph, verbose=TRUE)

# Plot ROC curve with the tool from SpiecEasi package
stars.roc(getOptMerge(se.mb), graph, verbose=TRUE)

# getOptMerge(se.mb) gets the weighted adjacency matrix of 
# the optimal (final) inferred graph as sparse matrix. 
# The weights come from the StARS model selection

# Get PR curve with PRROC package. I used this tool in the thesis for reasons explained there.
mat = as.matrix(getOptMerge(se.mb))
PR_curve <- PRROC::pr.curve(mat[graph == 1], mat[graph == 0], curve=TRUE)
plot(PR_curve)





###
# Visualize inferred network against true network
###


# Get true graph and inferred graph as igraph objects
ig.ground_truth <- SpiecEasi::adj2igraph(graph)
ig.mb <- SpiecEasi::adj2igraph(getRefit(se.mb))

# getRefit(se.mb) gets the adjacency matrix of 
# the optimal (final) inferred graph as sparse matrix.

# Set the size of vertices proportional to the mean of the clr-transformed data
vsize <- rowMeans(clr(X, 1)) + 6

# Get the coordinates of the vertices via the Fruchterman-Reingold algorithm
# Set a seed here if you want to preserve the coordinates for later use
set.seed(3)
am.coord <- igraph::layout.fruchterman.reingold(ig.ground_truth)

# Plot networks separately

plot(ig.ground_truth, layout=am.coord, vertex.size=vsize, vertex.label=NA, main="True Network")

plot(ig.mb, layout=am.coord, vertex.size=vsize, vertex.label=NA, main="SPIEC EASI mb")

# Colored_union is a function that plots the inferred and the true network in the same
# figure and colors the edges correspondingly;
# Load igraph before using this function
# Choose filename = FALSE if you don't want to save the file. Otherwise, provide a file name
# Provide a title name for the plot as well
colored_union(ig.ground_truth, filename = 'SpiecEasi_mb', ig.mb, name = "SPIEC_EASI_mb")



# This is an animation showing inferred graphs with different penalization parameters lambda
# They are stored already in se.mb$est$path
# Set an increasing sequence of indices, but not bigger then length(se.mb$est$path)
seq_lambda_path = c(2,5,7,9,11,14)
for (k in seq_lambda_path){
  
  Sys.sleep(1) 
  ig.mb_current <- adj2igraph(se.mb$est$path[[k]])
  colored_union(ig.ground_truth, filename = FALSE, ig.mb_current, name = "SpiecEasi_mb")
  
}








