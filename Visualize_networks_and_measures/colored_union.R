colored_union <- function(true_graph, inferred_graph, name, filename = FALSE, plot = TRUE){
  
  V(true_graph)$name <- as.character(V(true_graph))
  V(inferred_graph)$name <- as.character(V(inferred_graph))
  graph_union <- union(true_graph,inferred_graph)
  ecol <- rep("gray80", ecount(graph_union))
  
  for (i in 1:ecount(graph_union)){
    
    if (as_ids(E(graph_union))[i] %in% as_ids(E(true_graph)) && as_ids(E(graph_union))[i]%in% as_ids(E(inferred_graph))){
      ecol[i] <- "green"
    }
    
    if (as_ids(E(graph_union))[i] %in% as_ids(E(true_graph)) && !as_ids(E(graph_union))[i]%in% as_ids(E(inferred_graph))){
      ecol[i] <- "red"
    }
  }
  
  if (plot){
    
    if (filename == FALSE){
      plot(graph_union, edge.color=ecol, layout=am.coord, vertex.size=vsize, vertex.label=NA, main=paste(name))
      legend("topleft", title="Edge colors", c("True positives", "False negatives", "False positives", "True negatives"),
             fill=c("green", "red", "gray80", "white"), horiz = FALSE, cex = 0.5)
    }else{
      tiff(filename = paste('/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Plots/Plots_networks',filename,sep=''), height = 30, width = 30, units='cm', compression = "lzw", res = 300)
      plot(graph_union, edge.color=ecol, layout=am.coord, vertex.size=vsize, vertex.label=NA, main=paste(name), edge.width = 3.5)
      legend("topleft", title="Edge colors", c("True positives", "False negatives", "False positives", "True negatives"), 
             fill=c("green", "red", "gray80", "white"), horiz = FALSE, cex = 1)
      dev.off()
    }
  }
  
}