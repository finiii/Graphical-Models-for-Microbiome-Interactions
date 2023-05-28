get_mean <- function(score){
  return(lapply(score, mean))
}
get_sd <- function(score){
  return(lapply(score, sd))
}

get_results <- function(scores){
  
  for (i in 1:length(scores)){
    
    cat(paste(names(scores)[[i]], '\n\n', scores[[i]], '\n', 'mean:', get_mean(scores[[i]]),'\n', 'standard deviation:', get_sd(scores[[i]]),'\n\n'))
    
  }
  
}




####
# Since the results were stored slightly differently for each of the tools, 
# in the following, we handle them separately
####




########

# SE_gl

########

# Choose the results you want to load
load("/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Results/SE_gl/results_SE_gl_e-d_reps_100_zinegbin_samples_500_p_127_cn_5_cluster.RData",verbose=TRUE)

AUPR <- list(SE_gl = unlist(lapply(lapply(temp, '[[', 3L), '[[', 1L)))
TPR <- list(SE_gl = unlist(lapply(lapply(temp, '[[', 3L), '[[', 2L)))
TP <- list(SE_gl = unlist(lapply(lapply(temp, '[[', 3L), '[[', 3L)))
FPR <- list(SE_gl = unlist(lapply(lapply(temp, '[[', 3L), '[[', 4L)))
FP <- list(SE_gl = unlist(lapply(lapply(temp, '[[', 3L), '[[', 5L)))
Precision <- list(SE_gl = unlist(lapply(lapply(temp, '[[', 3L), '[[', 6L)))
Hamming_abs <- list(SE_gl = unlist(lapply(lapply(temp, '[[', 3L), '[[', 7L)))
Hamming_rel <- list(SE_gl = unlist(lapply(lapply(temp, '[[', 3L), '[[', 8L)))
F_score <- list(SE_gl = unlist(lapply(lapply(temp, '[[', 3L), '[[', 9L)))

scores <- list(AUPR, TPR, TP, FPR, FP, Precision, Hamming_abs, Hamming_rel, F_score)
names(scores) <- c('AUPR', 'TPR', 'TP', 'FPR', 'FP' , 'Precision', 'Hamming_abs', 'Hamming_rel', 'F_score')

cat("\n\nSE_gl:\n\n")

get_results(scores)




########

# SE_mb

########

# Choose the results you want to load
load("../Results/SE_mb/results_SE_mb_e-d_reps_100_zinegbin_samples_500_p_127_cn_100_band.RData",verbose=TRUE)

AUPR <- list(SE_mb = unlist(lapply(lapply(temp, '[[', 3L), '[[', 1L)))
TPR <- list(SE_mb = unlist(lapply(lapply(temp, '[[', 3L), '[[', 2L)))
TP <- list(SE_mb = unlist(lapply(lapply(temp, '[[', 3L), '[[', 3L)))
FPR <- list(SE_mb = unlist(lapply(lapply(temp, '[[', 3L), '[[', 4L)))
FP <- list(SE_mb = unlist(lapply(lapply(temp, '[[', 3L), '[[', 5L)))
Precision <- list(SE_mb = unlist(lapply(lapply(temp, '[[', 3L), '[[', 6L)))
Hamming_abs <- list(SE_mb = unlist(lapply(lapply(temp, '[[', 3L), '[[', 7L)))
Hamming_rel <- list(SE_mb = unlist(lapply(lapply(temp, '[[', 3L), '[[', 8L)))
F_score <- list(SE_mb = unlist(lapply(lapply(temp, '[[', 3L), '[[', 9L)))

scores <- list(AUPR, TPR, TP, FPR, FP, Precision, Hamming_abs, Hamming_rel, F_score)
names(scores) <- c('AUPR', 'TPR', 'TP', 'FPR', 'FP' , 'Precision', 'Hamming_abs', 'Hamming_rel', 'F_score')

cat("\n\nSE_mb:\n\n")

get_results(scores)




########

# COZINE

########

# Choose the results you want to load
load("../Results/COZINE/results_COZINE_e-d_reps_100_zinegbin_samples_250_p_127_cn_100_band.RData",verbose=TRUE)

TPR <- list(COZINE = unlist(lapply(lapply(temp, '[[', 1L), '[[', 1L)))
TP <- list(COZINE = unlist(lapply(lapply(temp, '[[', 1L), '[[', 2L)))
FPR <- list(COZINE = unlist(lapply(lapply(temp, '[[', 1L), '[[', 3L)))
FP <- list(COZINE = unlist(lapply(lapply(temp, '[[', 1L), '[[', 4L)))
Precision <- list(COZINE = unlist(lapply(lapply(temp, '[[', 1L), '[[', 5L)))
Hamming_abs <- list(COZINE = unlist(lapply(lapply(temp, '[[', 1L), '[[', 6L)))
Hamming_rel <- list(COZINE = unlist(lapply(lapply(temp, '[[', 1L), '[[', 7L)))
F_score <- list(COZINE = unlist(lapply(lapply(temp, '[[', 1L), '[[', 8L)))

scores <- list(TPR, TP, FPR, FP, Precision, Hamming_abs, Hamming_rel, F_score)
names(scores) <- c('TPR', 'TP', 'FPR', 'FP' , 'Precision', 'Hamming_abs', 'Hamming_rel', 'F_score')

cat("\n\nCOZINE:\n\n")

get_results(scores)





########

# gCoda

########

# Choose the results you want to load
load("../Results/gCoda/results_gCoda_e-d_reps_100_zinegbin_samples_250_p_127_cn_100_band.RData", verbose=TRUE)
temp <- res_gcoda

AUPR <- list(gCoda = unlist(lapply(lapply(temp, '[[', 1L), '[[', 1L)))
TPR <- list(gCoda = unlist(lapply(lapply(temp, '[[', 2L), '[[', 1L)))
TP <- list(gCoda = unlist(lapply(lapply(temp, '[[', 3L), '[[', 1L)))
FPR <- list(gCoda = unlist(lapply(lapply(temp, '[[', 4L), '[[', 1L)))
FP <- list(gCoda = unlist(lapply(lapply(temp, '[[', 5L), '[[', 1L)))
Precision <- list(gCoda = unlist(lapply(lapply(temp, '[[', 6L), '[[', 1L)))
Hamming_abs <- list(gCoda = unlist(lapply(lapply(temp, '[[', 7L), '[[', 1L)))
Hamming_rel <- list(gCoda = unlist(lapply(lapply(temp, '[[', 8L), '[[', 1L)))
F_score <- list(gCoda = unlist(lapply(lapply(temp, '[[', 9L), '[[', 1L)))

scores <- list(AUPR, TPR, TP, FPR, FP, Precision, Hamming_abs, Hamming_rel, F_score)
names(scores) <- c('AUPR', 'TPR', 'TP', 'FPR', 'FP' , 'Precision', 'Hamming_abs', 'Hamming_rel', 'F_score')

cat("\n\ngCoda:\n\n")

get_results(scores)




########

# SPRING

########

# Choose the results you want to load
load("../Results/SPRING/results_SPRING_e-d_reps_100_zinegbin_samples_250_p_127_cn_100_band.RData",verbose=TRUE)

AUPR <- list(SPRING = unlist(lapply(lapply(temp, '[[', 3L), '[[', 1L)))
TPR <- list(SPRING = unlist(lapply(lapply(temp, '[[', 3L), '[[', 2L)))
TP <- list(SPRING = unlist(lapply(lapply(temp, '[[', 3L), '[[', 3L)))
FPR <- list(SPRING = unlist(lapply(lapply(temp, '[[', 3L), '[[', 4L)))
FP <- list(SPRING = unlist(lapply(lapply(temp, '[[', 3L), '[[', 5L)))
Precision <- list(SPRING = unlist(lapply(lapply(temp, '[[', 3L), '[[', 6L)))
Hamming_abs <- list(SPRING = unlist(lapply(lapply(temp, '[[', 3L), '[[', 7L)))
Hamming_rel <- list(SPRING = unlist(lapply(lapply(temp, '[[', 3L), '[[', 8L)))
F_score <- list(SPRING = unlist(lapply(lapply(temp, '[[', 3L), '[[', 9L)))

scores <- list(AUPR, TPR, TP, FPR, FP, Precision, Hamming_abs, Hamming_rel, F_score)
names(scores) <- c('AUPR', 'TPR', 'TP', 'FPR', 'FP' , 'Precision', 'Hamming_abs', 'Hamming_rel', 'F_score')

cat("\n\nSPRING:\n\n")

get_results(scores)




########

# HARMONIES

########

# Choose the results you want to load
load("../Results/HARMONIES/results_HARMONIES_e-d_reps_100_zinegbin_samples_250_p_127_cn_100_band.RData",verbose=TRUE)

AUPR <- list(HARMONIES = unlist(lapply(lapply(temp, '[[', 3L), '[[', 1L)))
TPR <- list(HARMONIES = unlist(lapply(lapply(temp, '[[', 3L), '[[', 2L)))
TP <- list(HARMONIES = unlist(lapply(lapply(temp, '[[', 3L), '[[', 3L)))
FPR <- list(HARMONIES = unlist(lapply(lapply(temp, '[[', 3L), '[[', 4L)))
FP <- list(HARMONIES = unlist(lapply(lapply(temp, '[[', 3L), '[[', 5L)))
Precision <- list(HARMONIES = unlist(lapply(lapply(temp, '[[', 3L), '[[', 6L)))
Hamming_abs <- list(HARMONIES = unlist(lapply(lapply(temp, '[[', 3L), '[[', 7L)))
Hamming_rel <- list(HARMONIES = unlist(lapply(lapply(temp, '[[', 3L), '[[', 8L)))
F_score <- list(HARMONIES = unlist(lapply(lapply(temp, '[[', 3L), '[[', 9L)))

scores <- list(AUPR, TPR, TP, FPR, FP, Precision, Hamming_abs, Hamming_rel, F_score)
names(scores) <- c('AUPR', 'TPR', 'TP', 'FPR', 'FP' , 'Precision', 'Hamming_abs', 'Hamming_rel', 'F_score')

cat("\n\nHARMONIES:\n\n")

get_results(scores)

