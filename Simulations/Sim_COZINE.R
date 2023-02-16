source("../Measures/Rates.R")


input_function <- function(X, graph, lambda_min_ratio = 0.1){ #lambda_min_ratio = 0.5 is the default
  
  
  fit.cozine = COZINE::cozine(dat = X, lambda.min.ratio = lambda_min_ratio)
  cozine_path = lapply(fit.cozine$adjMat, COZINE::ORadj)
  final_net_cozine <- as.matrix(cozine_path[[which.min(fit.cozine$BIC_etc$BIC)]])
  
  prec <- TPR(final_net_cozine[graph == 1])[[2]]/(TPR(final_net_cozine[graph == 1])[[2]]+FPR(final_net_cozine[graph == 0])[[2]])
  Hamming_abs <- sum(final_net_cozine != graph)
  Hamming_rel <- Hamming_abs/(nrow(graph)*ncol(graph))
  F_score <- 2*prec*TPR(final_net_cozine[graph == 1])[[1]]/(prec+TPR(final_net_cozine[graph == 1])[[1]])
  res_cozine <- list(TPR = TPR(final_net_cozine[graph == 1])[[1]], 
                     TP = TPR(final_net_cozine[graph == 1])[[2]],
                     FPR = FPR(final_net_cozine[graph == 0])[[1]],  FP = FPR(final_net_cozine[graph == 0])[[2]],
                     Precision = prec, Hamming_abs = Hamming_abs, Hamming_rel = Hamming_rel, 
                     F_score = F_score)
  
  
  return(list(COZINE = res_cozine))
  
}


load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_500_p_127_cn_5_cluster.RData", verbose=TRUE)
info = 'e-d_reps_100_zinegbin_samples_500_p_127_cn_5_cluster.RData'
Xs <- lapply(gen_data, '[[', 1L)
graphs <- lapply(gen_data, '[[', 2L)

library(foreach)
library(doParallel)
library(doRNG)
library(PRROC)
library(pulsar)
library(COZINE)
library(parallel)
no_cores <- 55
cl <- makeCluster(no_cores, outfile = "")
clusterEvalQ(cl, {library(COZINE)
  library(PRROC)
  library(pulsar)
  library(parallel)})
registerDoParallel(cl)

reps = length(gen_data)

fct <- function(i){

  out <- input_function(Xs[[i]], graphs[[i]])
  return(out)

}



time_parallel <- system.time(
  temp <- foreach(i=1:reps) %dopar% {fct(i)})
stopCluster(cl)
time_parallel


save(temp, time_parallel, file = paste("../Results/COZINE/results_COZINE_", info, sep=''))






# load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_500_p_127_cn_5_band.RData", verbose=TRUE)
# info = 'e-d_reps_100_zinegbin_samples_500_p_127_cn_5_band.RData'
# Xs <- lapply(gen_data, '[[', 1L)
# graphs <- lapply(gen_data, '[[', 2L)
# 
# library(foreach)
# library(doParallel)
# library(doRNG)
# library(PRROC)
# library(pulsar)
# library(COZINE)
# library(parallel)
# no_cores <- 55
# cl <- makeCluster(no_cores, outfile = "")
# clusterEvalQ(cl, {library(COZINE)
#   library(PRROC)
#   library(pulsar)
#   library(parallel)})
# 
# reps = length(gen_data)
# 
# fct <- function(i){
# 
#   out <- input_function(Xs[[i]], graphs[[i]])
#   return(out)
# 
# }
# 
# 
# #registerDoRNG(6)
# time_parallel <- system.time(
#   temp <- foreach(i=1:reps) %dopar% {fct(i)})
# stopCluster(cl)
# time_parallel
# 
# 
# save(temp, time_parallel, file = paste("../Results/COZINE/results_COZINE_", info, sep=''))





# 
# load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_250_p_127_cn_100_scale_free.RData", verbose=TRUE)
# info = 'e-d_reps_100_zinegbin_samples_250_p_127_cn_100_scale_free.RData'
# TPzero <- c(4, 5, 25, 27, 40, 42, 44, 52, 58, 62, 66, 68, 72, 75, 93, 99)
# gen_data <- gen_data[TPzero]
# Xs <- lapply(gen_data, '[[', 1L)
# graphs <- lapply(gen_data, '[[', 2L)
# 
# 
# 
# library(foreach)
# library(doParallel)
# library(doRNG)
# library(PRROC)
# library(pulsar)
# library(COZINE)
# library(parallel)
# no_cores <- length(gen_data)
# cl <- makeCluster(no_cores, outfile = "")
# clusterEvalQ(cl, {library(COZINE)
#   library(PRROC)
#   library(pulsar)
#   library(parallel)}) 
# registerDoParallel(cl)
# 
# reps = length(gen_data)
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
# time_parallel
# 
# 
# save(temp, time_parallel, file = paste("../Results/COZINE/results_COZINE_", info, sep=''))
# 






