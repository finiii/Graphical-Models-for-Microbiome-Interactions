library(HARMONIES)

source("../Measures/Rates.R")


input_function <- function(X, graph){
  
  
  source("../Adapted_functions/my_harmonies_HARMONIES.R")
  source("../Adapted_functions/my_harmonies_get.network.R")
  source("../Adapted_functions/my_harmonies_est.pcor.R")
  har = my_harmonies_HARMONIES(X, phenotype = rep(0,nrow(X)), beta.stars = 0.15)
  
  har_pulsar = har[[2]]
  
  mat_harmonies = as.matrix(har_pulsar$stars$merge[[har_pulsar$stars$opt.index]])
  
  final_net_harmonies = as.matrix(har_pulsar$est$path[[har_pulsar$stars$opt.index]])
  
  
  pr_harmonies_init <- PRROC::pr.curve(mat_harmonies[graph == 1], mat_harmonies[graph == 0])
  pr_harmonies <- pr_harmonies_init$auc.davis.goadrich
  prec <- TPR(final_net_harmonies[graph == 1])[[2]]/(TPR(final_net_harmonies[graph == 1])[[2]]+FPR(final_net_harmonies[graph == 0])[[2]])
  Hamming_abs <- sum(final_net_harmonies != graph)
  Hamming_rel <- Hamming_abs/(nrow(graph)*ncol(graph))
  F_score <- 2*prec*TPR(final_net_harmonies[graph == 1])[[1]]/(prec+TPR(final_net_harmonies[graph == 1])[[1]])
  res_harmonies <- list(AUPR = pr_harmonies, TPR = TPR(final_net_harmonies[graph == 1])[[1]],
                        TP = TPR(final_net_harmonies[graph == 1])[[2]],
                        FPR = FPR(final_net_harmonies[graph == 0])[[1]],
                        FP = FPR(final_net_harmonies[graph == 0])[[2]],
                        Precision = prec, Hamming_abs = Hamming_abs,
                        Hamming_rel = Hamming_rel, F_score = F_score)
  
  
  
  return(list(X = X, graph = graph, HARMONIES = res_harmonies))
  
}


# Graph type: cluster

load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_500_p_127_cn_5_cluster.RData", verbose=TRUE)
info = 'e-d_reps_100_zinegbin_samples_500_p_127_cn_5_cluster.RData'
Xs <- lapply(gen_data, '[[', 1L)
graphs <- lapply(gen_data, '[[', 2L)

library(foreach)
library(doParallel)
library(doRNG)
library(HARMONIES)
library(PRROC)
library(pulsar)
no_cores <- 55
cl <- makeCluster(no_cores, outfile = "")
clusterEvalQ(cl, {library(HARMONIES)
  library(PRROC)
  library(pulsar)}) 
registerDoParallel(cl)

reps = length(gen_data)


fct <- function(i){
  
  out <- input_function(Xs[[i]], graphs[[i]])
  return(out)
  
}

#clusterExport(cl, varlist = c("fct", "X")) #Apparently I don t necessarily need this

#registerDoRNG(6)
time_parallel <- system.time(
  temp <- foreach(i=1:reps) %dopar% {fct(i)})
stopCluster(cl)

save(temp, time_parallel, file = paste("../Results/HARMONIES/results_HARMONIES_", info, sep=''))



# 
# # Graph type: band
# 
# load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_500_p_127_cn_100_band.RData", verbose=TRUE)
# info = 'e-d_reps_100_zinegbin_samples_500_p_127_cn_100_band.RData'
# Xs <- lapply(gen_data, '[[', 1L)
# graphs <- lapply(gen_data, '[[', 2L)
# 
# library(foreach)
# library(doParallel)
# library(doRNG)
# library(HARMONIES)
# library(PRROC)
# library(pulsar)
# no_cores <- 55
# cl <- makeCluster(no_cores, outfile = "")
# clusterEvalQ(cl, {library(HARMONIES)
#   library(PRROC)
#   library(pulsar)}) 
# registerDoParallel(cl)
# 
# reps = length(gen_data)
# 
# 
# fct <- function(i){
#   
#   out <- input_function(Xs[[i]], graphs[[i]])
#   return(out)
#   
# }
# 
# #clusterExport(cl, varlist = c("fct", "X")) #Apparently I don t necessarily need this
# 
# #registerDoRNG(6)
# time_parallel <- system.time(
#   temp <- foreach(i=1:reps) %dopar% {fct(i)})
# stopCluster(cl)
# 
# save(temp, time_parallel, file = paste("../Results/HARMONIES/results_HARMONIES_", info, sep=''))
# 
# 
# 
# 
# # Graph type: scale_free
# 
# load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_500_p_127_cn_100_scale_free.RData", verbose=TRUE)
# info = 'e-d_reps_100_zinegbin_samples_500_p_127_cn_100_scale_free.RData'
# Xs <- lapply(gen_data, '[[', 1L)
# graphs <- lapply(gen_data, '[[', 2L)
# 
# library(foreach)
# library(doParallel)
# library(doRNG)
# library(HARMONIES)
# library(PRROC)
# library(pulsar)
# no_cores <- 55
# cl <- makeCluster(no_cores, outfile = "")
# clusterEvalQ(cl, {library(HARMONIES)
#   library(PRROC)
#   library(pulsar)}) 
# registerDoParallel(cl)
# 
# reps = length(gen_data)
# 
# 
# fct <- function(i){
#   
#   out <- input_function(Xs[[i]], graphs[[i]])
#   return(out)
#   
# }
# 
# #clusterExport(cl, varlist = c("fct", "X")) #Apparently I don t necessarily need this
# 
# #registerDoRNG(6)
# time_parallel <- system.time(
#   temp <- foreach(i=1:reps) %dopar% {fct(i)})
# stopCluster(cl)
# 
# save(temp, time_parallel, file = paste("../Results/HARMONIES/results_HARMONIES_", info, sep=''))





