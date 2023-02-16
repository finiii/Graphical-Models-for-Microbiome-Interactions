library(SpiecEasi)

source("../Measures/Rates.R")


input_function <- function(X, graph, lambda_min_ratio = 1e-2){
  
  
  se.mb <- SpiecEasi::spiec.easi(X, method='mb', lambda.min.ratio=lambda_min_ratio, nlambda=30)

  mat_se_mb = as.matrix(SpiecEasi::getOptMerge(se.mb))

  final_net_se_mb = as.matrix(SpiecEasi::getRefit(se.mb))

  pr_se_mb_init <- PRROC::pr.curve(mat_se_mb[graph == 1], mat_se_mb[graph == 0])
  pr_se_mb <- pr_se_mb_init$auc.davis.goadrich
  prec <- TPR(final_net_se_mb[graph == 1])[[2]]/(TPR(final_net_se_mb[graph == 1])[[2]]+FPR(final_net_se_mb[graph == 0])[[2]])
  Hamming_abs <- sum(final_net_se_mb != graph)
  Hamming_rel <- Hamming_abs/(nrow(graph)*ncol(graph))
  F_score <- 2*prec*TPR(final_net_se_mb[graph == 1])[[1]]/(prec+TPR(final_net_se_mb[graph == 1])[[1]])
  res_se_mb <- list(AUPR = pr_se_mb, TPR = TPR(final_net_se_mb[graph == 1])[[1]],
                    TP = TPR(final_net_se_mb[graph == 1])[[2]],
                    FPR = FPR(final_net_se_mb[graph == 0])[[1]],
                    FP = FPR(final_net_se_mb[graph == 0])[[2]], Precision = prec,
                    Hamming_abs = Hamming_abs, Hamming_rel = Hamming_rel,
                    F_score = F_score)
  
  
  return(list(X = X, graph = graph, SE_mb = res_se_mb))
  
}


# Graph type: cluster

load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_500_p_127_cn_5_cluster.RData", verbose=TRUE)
info = 'e-d_reps_100_zinegbin_samples_500_p_127_cn_5_cluster.RData'
Xs <- lapply(gen_data, '[[', 1L)
graphs <- lapply(gen_data, '[[', 2L)

library(foreach)
library(doParallel)
library(doRNG)
library(SpiecEasi)
# library(SPRING)
# library(HARMONIES)
library(PRROC)
library(pulsar)
no_cores <- 55
cl <- makeCluster(no_cores, outfile = "")
clusterEvalQ(cl, {library(SpiecEasi)
  library(SPRING)
  library(HARMONIES)
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

save(temp, time_parallel, file = paste("../Results/SE_mb/results_SE_mb_", info, sep=''))



# # Graph type: band
# 
# load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_500_p_127_cn_5_band.RData", verbose=TRUE)
# info = 'e-d_reps_100_zinegbin_samples_500_p_127_cn_5_band.RData'
# Xs <- lapply(gen_data, '[[', 1L)
# graphs <- lapply(gen_data, '[[', 2L)
# 
# library(foreach)
# library(doParallel)
# library(doRNG)
# library(SpiecEasi)
# # library(SPRING)
# # library(HARMONIES)
# library(PRROC)
# library(pulsar)
# no_cores <- 55
# cl <- makeCluster(no_cores, outfile = "")
# clusterEvalQ(cl, {library(SpiecEasi)
#   library(SPRING)
#   library(HARMONIES)
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
# time_parallel
# 
# 
# save(temp, time_parallel, file = paste("../Results/SE_mb/results_SE_mb_", info, sep=''))



# 
# # Graph type: scale_free
# 
# load("../Generate_data/gen_data_e-d_reps_100_zinegbin_samples_250_p_127_cn_100_scale_free.RData", verbose=TRUE)
# info = 'e-d_reps_100_zinegbin_samples_250_p_127_cn_100_scale_free.RData'
# Xs <- lapply(gen_data, '[[', 1L)
# graphs <- lapply(gen_data, '[[', 2L)
# 
# library(foreach)
# library(doParallel)
# library(doRNG)
# library(SpiecEasi)
# # library(SPRING)
# # library(HARMONIES)
# library(PRROC)
# library(pulsar)
# no_cores <- 55
# cl <- makeCluster(no_cores, outfile = "")
# clusterEvalQ(cl, {library(SpiecEasi)
#   library(SPRING)
#   library(HARMONIES)
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
# time_parallel
# 
# 
# save(temp, time_parallel, file = paste("../Results/SE_mb/results_SE_mb_", info, sep=''))
# 




