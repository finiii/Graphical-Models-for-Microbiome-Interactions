# This function returns the symmetric difference graph and the difference graphs
symm_diff <- function(ig.one, ig.two, taxa_names){
  
  int <- intersection(ig.one, ig.two)
  one_minus_int <- as.matrix(as_adjacency_matrix(ig.one)) - as.matrix(as_adjacency_matrix(int))
  two_minus_int <- as.matrix(as_adjacency_matrix(ig.two)) - as.matrix(as_adjacency_matrix(int))
  symm_difference <- one_minus_int + two_minus_int
  
  
  ig.one_minus_int <- adj2igraph(one_minus_int, vertex.attr = list(name=taxa_names))
  ig.two_minus_int <- adj2igraph(two_minus_int, vertex.attr = list(name=taxa_names))
  
  ig.symm_difference <- adj2igraph(symm_difference, vertex.attr = list(name=taxa_names))
  
  return(list(ig.symm_difference, ig.one_minus_int, ig.two_minus_int))
  
}


get_edge_names <- function(graph, positions){
  edge <- c()
  vertices <- as_ids(V(graph))
  for (i in 1:nrow(positions)){
    edge <- c(edge,paste(vertices[positions[i,1]], vertices[positions[i,2]], sep='|'))
  }
  return(edge)
}

# Get only genera names from taxa names
get_genera <- function(taxa_names){
  
  genera <- c()
  for (i in 1:length(taxa_names)){
    sp <- unlist(strsplit(taxa_names[[i]], "[;]"))
    g <- unlist(strsplit(sp[6], "[__]"))
    f <- unlist(strsplit(sp[5], "[__]"))
    o <- unlist(strsplit(sp[4], "[__]"))
    if (!is.na(g[3])){
      genera <- c(genera, paste(i, g[3], sep = '_'))
    }else{
      if (!is.na(f[3])){
        genera <- c(genera, paste(i, f[3], sep = '_'))
      }else{
        genera <- c(genera, paste(i, o[3], sep = '_'))
      }
    }
  }
  return(genera)
}


# This function plots a colored network of the union graph, highlighting the symmetric difference edges and
# making the intersection edges thin and gray
colored_plot <- function(graph1,graph2,graph_union, taxa_names){
  
  load("am.coord.RData",verbose = TRUE)
  am.coord <- am.c
  vsize <- vs
  genera <- get_genera(taxa_names)
  #graph_union <- union(graph1, graph2)
  ecol <- rep("gray80", ecount(graph_union))
  graph1_minus_graph2 <- symm_diff(graph1,graph2, taxa_names)[[2]]
  graph2_minus_graph1 <- symm_diff(graph1,graph2, taxa_names)[[3]]
  
  for (i in 1:ecount(graph_union)){
    if (as_ids(E(graph_union))[i] %in% as_ids(E(graph1_minus_graph2))){
      ecol[i] <- 'deepskyblue'
    }
    if (as_ids(E(graph_union))[i] %in% as_ids(E(graph2_minus_graph1))){
      ecol[i] <- 'deeppink'
    }
  }
  
  weights <-  E(graph_union)$weight
  weights[weights == 0.1] <- ' '
  pdf(file = "../Plots/Plots_AGP_data/se.mb_symm_diff_male_female.pdf", height = 30, width = 30)
  plot(graph_union, edge.color=ecol, vertex.size=1.3*vsize, layout=am.coord,
       vertex.label=genera,  cex.main = 3, edge.width=50*E(graph_union)$weight,
       vertex.label.cex = 3)
  legend("topleft", title="Edge colors", c("Only male data", "Only female data", "Male and female data"),
         fill=c("deepskyblue", "deeppink", "gray80"), horiz = FALSE, cex = 3)
  dev.off()
}

# This function returns the union graphs with weights and scaled down weights of the intersection edges
get_weighted_graph <- function(ig.male, ig.female, sparse_inferred_adj_matrix_male, sparse_inferred_adj_matrix_female, taxa_names){
  
  # get difference graph male-female as igraph
  male_minus_female <- symm_diff(ig.male,ig.female, taxa_names)[[2]]
  adj <- as.matrix(igraph::as_adjacency_matrix(male_minus_female))
  pos <- which(adj == 1, arr.ind = TRUE) # get indices of entries 1 (corresponding to the edges)
  
  sparse_inferred_adj_matrix_male[pos] # get weights of edges
  
  # Get sparse adjacency matrix of the difference graph
  sparse_male_minus_female <- Matrix::sparseMatrix(pos[,1],pos[,2],x = sparse_inferred_adj_matrix_male[pos],
                                                   dims = c(nrow(sparse_inferred_adj_matrix_male), ncol(sparse_inferred_adj_matrix_male)))
  
  # get difference graph female-male as igraph
  female_minus_male <- symm_diff(ig.male,ig.female, taxa_names)[[3]]
  adj <- as.matrix(as_adjacency_matrix(female_minus_male)) #! add type = 'lower' to avoid repeating edges
  pos <- which(adj == 1, arr.ind = TRUE) # get indices of entries 1 (corresponding to the edges)
  
  # Get sparse adjacency matrix of the difference graph
  sparse_female_minus_male <- sparseMatrix(pos[,1],pos[,2],x = sparse_inferred_adj_matrix_female[pos],
                                           dims = c(nrow(sparse_inferred_adj_matrix_female), ncol(sparse_inferred_adj_matrix_female)))
  
  # get sparse WEIGHTED adjacency matrix of the symmetric difference graph
  weights_symm_diff <- sparse_female_minus_male + sparse_male_minus_female
  
  # get intersection graph as igraph
  int <- igraph::intersection(ig.male, ig.female)
  
  # Lower the weights of the intersection graph for visualization purposes
  # Get the adjacency matrix of the union graph
  un_matrix <- as.matrix(weights_symm_diff) + as.matrix(as_adjacency_matrix(int))/10
  
  # Get union graph as igraph
  union <- SpiecEasi::adj2igraph(un_matrix, vertex.attr = list(name=taxa_names))
  
  return(union)
  
}




