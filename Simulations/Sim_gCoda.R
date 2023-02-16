library(SpiecEasi)
library(parallel)
library(PRROC)

source("../Measures/Rates.R")

# Computes bootstrap estimates of edge stabilities, section 4.2.1
bt <- function(X, method, m = 50){
  
  edge_count = matrix(0, ncol(X), ncol(X))
  
  if (method == 'COZINE'){
    
    res <- mclapply(1:m, function(i) {
      s <- sample(1:nrow(X), replace = TRUE)
      X_bt <- X[s,]
      fit.cozine = cozine(dat = X_bt, lambda.min.ratio = lambda_min_ratio)
      cozine_path = lapply(fit.cozine$adjMat, COZINE::ORadj)
      return(cozine_path[[which.min(fit.cozine$BIC_etc$BIC)]])
    }, mc.cores = m)
    edge_count <- Reduce('+',res)
    
    
  }
  
  if (method == 'gCoda'){
    
    res <- mclapply(1:m, function(i) {
      s <- sample(1:nrow(X), replace = TRUE)
      X_bt <- X[s,]
      source("/dss/dsshome1/lxc0F/ge59vew2/Petru/gcoda.R")
      fit.gcoda = gcoda(x = X_bt, counts = T, lambda.min.ratio = 1e-2)
      return(fit.gcoda$path[[fit.gcoda$opt.index]])
    }, mc.cores = m)
    edge_count <- Reduce('+',res)
    
  }
  
  edge_freq <- edge_count/m
  
  return(edge_freq)
  
  
}



load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_500_p_127_cn_5_cluster.RData", verbose=TRUE)
info = "e-d_reps_100_zinegbin_samples_500_p_127_cn_5_cluster.RData"

Xs <- lapply(gen_data, '[[', 1L)
graphs <- lapply(gen_data, '[[', 2L)
merge <- list()
res_gcoda <- list()


for (i in 1:length(Xs)){

  graph <- graphs[[i]]
  source("~/Petru/gcoda.R")
  fit.gcoda = gcoda(x = Xs[[i]], counts = T)
  final_net_gcoda <- fit.gcoda$path[[fit.gcoda$opt.index]]
  merge[[i]] <- bt(Xs[[i]], method = 'gCoda', m = 50)

  mat_gcoda <- as.matrix(merge[[i]])
  pr_gcoda_init <- PRROC::pr.curve(mat_gcoda[graph == 1], mat_gcoda[graph == 0])
  pr_gcoda <- pr_gcoda_init$auc.davis.goadrich

  Hamming_abs <- sum(final_net_gcoda != graph)
  Hamming_rel <- Hamming_abs/(nrow(graph)*ncol(graph))
  prec <- TPR(final_net_gcoda[graph == 1])[[2]]/(TPR(final_net_gcoda[graph == 1])[[2]]+FPR(final_net_gcoda[graph == 0])[[2]])
  F_score <- 2*prec*TPR(final_net_gcoda[graph == 1])[[1]]/(prec+TPR(final_net_gcoda[graph == 1])[[1]])
  res_gcoda[[i]] <- list(AUPR = pr_gcoda, TPR = TPR(final_net_gcoda[graph == 1])[[1]],
                         TP = TPR(final_net_gcoda[graph == 1])[[2]],
                         FPR = FPR(final_net_gcoda[graph == 0])[[1]],
                         FP = FPR(final_net_gcoda[graph == 0])[[2]], Precision = prec,
                         Hamming_abs = Hamming_abs, Hamming_rel = Hamming_rel,
                         F_score = F_score)

}

save(res_gcoda, merge, file = paste("../Results/gCoda/results_gCoda_", info, sep=''))






# load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_500_p_127_cn_5_band.RData", verbose=TRUE)
# info = "e-d_reps_100_zinegbin_samples_500_p_127_cn_5_band.RData"
# 
# Xs <- lapply(gen_data, '[[', 1L)
# graphs <- lapply(gen_data, '[[', 2L)
# merge <- list()
# res_gcoda <- list()
# 
# 
# for (i in 1:length(Xs)){
# 
#   graph <- graphs[[i]]
#   source("~/Petru/gcoda.R")
#   fit.gcoda = gcoda(x = Xs[[i]], counts = T)
#   final_net_gcoda <- fit.gcoda$path[[fit.gcoda$opt.index]]
#   merge[[i]] <- bt(Xs[[i]], method = 'gCoda', m = 50)
# 
#   mat_gcoda <- as.matrix(merge[[i]])
#   pr_gcoda_init <- PRROC::pr.curve(mat_gcoda[graph == 1], mat_gcoda[graph == 0])
#   pr_gcoda <- pr_gcoda_init$auc.davis.goadrich
# 
#   Hamming_abs <- sum(final_net_gcoda != graph)
#   Hamming_rel <- Hamming_abs/(nrow(graph)*ncol(graph))
#   prec <- TPR(final_net_gcoda[graph == 1])[[2]]/(TPR(final_net_gcoda[graph == 1])[[2]]+FPR(final_net_gcoda[graph == 0])[[2]])
#   F_score <- 2*prec*TPR(final_net_gcoda[graph == 1])[[1]]/(prec+TPR(final_net_gcoda[graph == 1])[[1]])
#   res_gcoda[[i]] <- list(AUPR = pr_gcoda, TPR = TPR(final_net_gcoda[graph == 1])[[1]],
#                          TP = TPR(final_net_gcoda[graph == 1])[[2]],
#                          FPR = FPR(final_net_gcoda[graph == 0])[[1]],
#                          FP = FPR(final_net_gcoda[graph == 0])[[2]], Precision = prec,
#                          Hamming_abs = Hamming_abs, Hamming_rel = Hamming_rel,
#                          F_score = F_score)
# 
# }
# 
# save(res_gcoda, merge, file = paste("../Results/gCoda/results_gCoda_", info, sep=''))




# 
# load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_250_p_127_cn_100_scale_free.RData", verbose=TRUE)
# info = "e-d_reps_100_zinegbin_samples_250_p_127_cn_100_scale_free.RData"
# TPzero_gCoda_250_scale_free <- c(23, 32, 45, 57, 76, 91)
# gen_data <- gen_data[TPzero_gCoda_250_scale_free]
# Xs <- lapply(gen_data, '[[', 1L)
# graphs <- lapply(gen_data, '[[', 2L)
# merge <- list()
# res_gcoda <- list()
# 
# 
# for (i in 1:length(Xs)){
#   
#   graph <- graphs[[i]]
#   source("~/Petru/gcoda.R")
#   fit.gcoda = gcoda(x = Xs[[i]], counts = T, lambda.min.ratio = 1e-2)
#   final_net_gcoda <- fit.gcoda$path[[fit.gcoda$opt.index]]
#   merge[[i]] <- bt(Xs[[i]], method = 'gCoda', m = 50)
#   
#   mat_gcoda <- as.matrix(merge[[i]])
#   pr_gcoda_init <- PRROC::pr.curve(mat_gcoda[graph == 1], mat_gcoda[graph == 0])
#   pr_gcoda <- pr_gcoda_init$auc.davis.goadrich
#   
#   Hamming_abs <- sum(final_net_gcoda != graph)
#   Hamming_rel <- Hamming_abs/(nrow(graph)*ncol(graph))
#   prec <- TPR(final_net_gcoda[graph == 1])[[2]]/(TPR(final_net_gcoda[graph == 1])[[2]]+FPR(final_net_gcoda[graph == 0])[[2]])
#   F_score <- 2*prec*TPR(final_net_gcoda[graph == 1])[[1]]/(prec+TPR(final_net_gcoda[graph == 1])[[1]])
#   res_gcoda[[i]] <- list(AUPR = pr_gcoda, TPR = TPR(final_net_gcoda[graph == 1])[[1]],
#                          TP = TPR(final_net_gcoda[graph == 1])[[2]],
#                          FPR = FPR(final_net_gcoda[graph == 0])[[1]], 
#                          FP = FPR(final_net_gcoda[graph == 0])[[2]], Precision = prec,
#                          Hamming_abs = Hamming_abs, Hamming_rel = Hamming_rel, 
#                          F_score = F_score)
#   
# }
# 
# save(res_gcoda, merge, file = paste("../Results/gCoda/results_TPzero_gCoda_", info, sep=''))
# 




