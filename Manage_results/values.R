values <- function(no_samples, graph_type, temp1, temp2, temp3, temp4, temp5, temp6){
  
  AUPR <- list()
  TPR <- list()
  TP <- list()
  FPR <- list()
  FP <- list()
  Precision <- list() 
  Hamming_abs <- list()
  Hamming_rel <- list()
  F_score <- list()
  
  
  # SE_gl
  
  AUPR[[1]] <- unlist(lapply(lapply(temp1, '[[', 3L), '[[', 1L))
  TPR[[1]] <- unlist(lapply(lapply(temp1, '[[', 3L), '[[', 2L))
  TP[[1]] <- unlist(lapply(lapply(temp1, '[[', 3L), '[[', 3L))
  FPR[[1]] <- unlist(lapply(lapply(temp1, '[[', 3L), '[[', 4L))
  FP[[1]] <- unlist(lapply(lapply(temp1, '[[', 3L), '[[', 5L))
  Precision[[1]] <- unlist(lapply(lapply(temp1, '[[', 3L), '[[', 6L))
  Hamming_abs[[1]] <- unlist(lapply(lapply(temp1, '[[', 3L), '[[', 7L))
  Hamming_rel[[1]] <- unlist(lapply(lapply(temp1, '[[', 3L), '[[', 8L))
  F_score[[1]] <- unlist(lapply(lapply(temp1, '[[', 3L), '[[', 9L))

  
  # SE_mb
  
  AUPR[[2]] <- unlist(lapply(lapply(temp2, '[[', 3L), '[[', 1L))
  TPR[[2]] <- unlist(lapply(lapply(temp2, '[[', 3L), '[[', 2L))
  TP[[2]] <- unlist(lapply(lapply(temp2, '[[', 3L), '[[', 3L))
  FPR[[2]] <- unlist(lapply(lapply(temp2, '[[', 3L), '[[', 4L))
  FP[[2]] <- unlist(lapply(lapply(temp2, '[[', 3L), '[[', 5L))
  Precision[[2]] <- unlist(lapply(lapply(temp2, '[[', 3L), '[[', 6L))
  Hamming_abs[[2]] <- unlist(lapply(lapply(temp2, '[[', 3L), '[[', 7L))
  Hamming_rel[[2]] <- unlist(lapply(lapply(temp2, '[[', 3L), '[[', 8L))
  F_score[[2]] <- unlist(lapply(lapply(temp2, '[[', 3L), '[[', 9L))
  
  
  
  # SPRING
  
  AUPR[[3]] <- unlist(lapply(lapply(temp3, '[[', 3L), '[[', 1L))
  TPR[[3]] <- unlist(lapply(lapply(temp3, '[[', 3L), '[[', 2L))
  TP[[3]] <- unlist(lapply(lapply(temp3, '[[', 3L), '[[', 3L))
  FPR[[3]] <- unlist(lapply(lapply(temp3, '[[', 3L), '[[', 4L))
  FP[[3]] <- unlist(lapply(lapply(temp3, '[[', 3L), '[[', 5L))
  Precision[[3]] <- unlist(lapply(lapply(temp3, '[[', 3L), '[[', 6L))
  Hamming_abs[[3]] <- unlist(lapply(lapply(temp3, '[[', 3L), '[[', 7L))
  Hamming_rel[[3]] <- unlist(lapply(lapply(temp3, '[[', 3L), '[[', 8L))
  F_score[[3]] <- unlist(lapply(lapply(temp3, '[[', 3L), '[[', 9L))
  
  
  # HARMONIES
  
  AUPR[[4]] <- unlist(lapply(lapply(temp4, '[[', 3L), '[[', 1L))
  TPR[[4]] <- unlist(lapply(lapply(temp4, '[[', 3L), '[[', 2L))
  TP[[4]] <- unlist(lapply(lapply(temp4, '[[', 3L), '[[', 3L))
  FPR[[4]] <- unlist(lapply(lapply(temp4, '[[', 3L), '[[', 4L))
  FP[[4]] <- unlist(lapply(lapply(temp4, '[[', 3L), '[[', 5L))
  Precision[[4]] <- unlist(lapply(lapply(temp4, '[[', 3L), '[[', 6L))
  Hamming_abs[[4]] <- unlist(lapply(lapply(temp4, '[[', 3L), '[[', 7L))
  Hamming_rel[[4]] <- unlist(lapply(lapply(temp4, '[[', 3L), '[[', 8L))
  F_score[[4]] <- unlist(lapply(lapply(temp4, '[[', 3L), '[[', 9L))
  
  
  
  # gCoda
  
  AUPR[[5]] <- unlist(lapply(lapply(temp5, '[[', 1L), '[[', 1L))
  TPR[[5]] <- unlist(lapply(lapply(temp5, '[[', 2L), '[[', 1L))
  TP[[5]] <- unlist(lapply(lapply(temp5, '[[', 3L), '[[', 1L))
  FPR[[5]] <- unlist(lapply(lapply(temp5, '[[', 4L), '[[', 1L))
  FP[[5]] <- unlist(lapply(lapply(temp5, '[[', 5L), '[[', 1L))
  Precision[[5]] <- unlist(lapply(lapply(temp5, '[[', 6L), '[[', 1L))
  Hamming_abs[[5]] <- unlist(lapply(lapply(temp5, '[[', 7L), '[[', 1L))
  Hamming_rel[[5]] <- unlist(lapply(lapply(temp5, '[[', 8L), '[[', 1L))
  F_score[[5]] <- unlist(lapply(lapply(temp5, '[[', 9L), '[[', 1L))
  
  
  
  
  # COZINE
  
  TPR[[6]] <- unlist(lapply(lapply(temp6, '[[', 1L), '[[', 1L))
  TP[[6]] <- unlist(lapply(lapply(temp6, '[[', 1L), '[[', 2L))
  FPR[[6]] <- unlist(lapply(lapply(temp6, '[[', 1L), '[[', 3L))
  FP[[6]] <- unlist(lapply(lapply(temp6, '[[', 1L), '[[', 4L))
  Precision[[6]] <- unlist(lapply(lapply(temp6, '[[', 1L), '[[', 5L))
  Hamming_abs[[6]] <- unlist(lapply(lapply(temp6, '[[', 1L), '[[', 6L))
  Hamming_rel[[6]] <- unlist(lapply(lapply(temp6, '[[', 1L), '[[', 7L))
  F_score[[6]] <- unlist(lapply(lapply(temp6, '[[', 1L), '[[', 8L))
  
  
  
  if(no_samples == 500 && graph_type == 'scale_free'){
    
    load("../Results/SE_gl/results_SE_gl_samples500_scale_free_X63.RData", verbose = TRUE)
    AUPR[[1]][63] <- res_se_gl[[1]]
    TPR[[1]][63] <- res_se_gl[[2]]
    TP[[1]][63] <- res_se_gl[[3]]
    FPR[[1]][63] <- res_se_gl[[4]]
    FP[[1]][63] <- res_se_gl[[5]]
    Precision[[1]][63] <- res_se_gl[[6]]
    Hamming_abs[[1]][63] <- res_se_gl[[7]]
    Hamming_rel[[1]][63] <- res_se_gl[[8]]
    F_score[[1]][63] <- res_se_gl[[9]]
    
    
    load("../Results/SE_mb/results_SE_mb_samples500_scale_free_X63.RData",verbose=TRUE)
    AUPR[[2]][63] <- res_se_mb[[1]]
    TPR[[2]][63] <- res_se_mb[[2]]
    TP[[2]][63] <- res_se_mb[[3]]
    FPR[[2]][63] <- res_se_mb[[4]]
    FP[[2]][63] <- res_se_mb[[5]]
    Precision[[2]][63] <- res_se_mb[[6]]
    Hamming_abs[[2]][63] <- res_se_mb[[7]]
    Hamming_rel[[2]][63] <- res_se_mb[[8]]
    F_score[[2]][63] <- res_se_mb[[9]]
    
    TPzero_COZINE_500_scale_free <- c(2, 19, 27, 49, 63, 72)
    
    Precision[[6]][TPzero_COZINE_500_scale_free] <- 0
    F_score[[6]][TPzero_COZINE_500_scale_free] <- 0
    
    
  }
  
  
  
  if(no_samples == 250 && graph_type == 'scale_free'){
    
    load("../Results/HARMONIES/results_TPzero_HARMONIES_e-d_reps_100_zinegbin_samples_250_p_127_cn_100_scale_free.RData",verbose=TRUE)
    TPzero_HARMONIES_250_scale_free <- c(18, 38, 49, 52, 64, 68, 73, 83, 92, 97)
    AUPR[[4]][TPzero_HARMONIES_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 3L), '[[', 1L))
    TPR[[4]][TPzero_HARMONIES_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 3L), '[[', 1L))
    TPR[[4]][TPzero_HARMONIES_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 3L), '[[', 2L))
    TP[[4]][TPzero_HARMONIES_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 3L), '[[', 3L))
    FPR[[4]][TPzero_HARMONIES_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 3L), '[[', 4L))
    FP[[4]][TPzero_HARMONIES_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 3L), '[[', 5L))
    Precision[[4]][TPzero_HARMONIES_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 3L), '[[', 6L))
    Hamming_abs[[4]][TPzero_HARMONIES_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 3L), '[[', 7L))
    Hamming_rel[[4]][TPzero_HARMONIES_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 3L), '[[', 8L))
    F_score[[4]][TPzero_HARMONIES_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 3L), '[[', 9L))
    
    
    
    load("../Results/gCoda/results_TPzero_gCoda_e-d_reps_100_zinegbin_samples_250_p_127_cn_100_scale_free.RData",verbose=TRUE)
    temp <- res_gcoda
    TPzero_gCoda_250_scale_free <- c(23, 32, 45, 57, 76, 91)
    AUPR[[5]][TPzero_gCoda_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 1L), '[[', 1L))
    TPR[[5]][TPzero_gCoda_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 2L), '[[', 1L))
    TP[[5]][TPzero_gCoda_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 3L), '[[', 1L))
    FPR[[5]][TPzero_gCoda_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 4L), '[[', 1L))
    FP[[5]][TPzero_gCoda_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 5L), '[[', 1L))
    Precision[[5]][TPzero_gCoda_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 6L), '[[', 1L))
    Hamming_abs[[5]][TPzero_gCoda_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 7L), '[[', 1L))
    Hamming_rel[[5]][TPzero_gCoda_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 8L), '[[', 1L))
    F_score[[5]][TPzero_gCoda_250_scale_free] <- unlist(lapply(lapply(temp, '[[', 9L), '[[', 1L))
    
    TPzero_COZINE_250_scale_free <- c(4, 5, 25, 27, 40, 42, 44, 52, 58, 62, 66, 68, 72, 75, 93, 99)
    
    Precision[[6]][TPzero_COZINE_250_scale_free] <- 0
    F_score[[6]][TPzero_COZINE_250_scale_free] <- 0
    
  }
  
  
  return(list(AUPR, TPR, TP, FPR ,FP, Precision, Hamming_abs, Hamming_rel, F_score))
  
}