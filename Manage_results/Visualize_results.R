
source("/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Manage_results/plot_violin.R")
source("/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Manage_results/values.R")
source("/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Manage_results/stats.R")


# Load the results you need: either all band, or all cluster, or all scale_free. Here, as example, band.

# Choose graph type between 'band', 'cluster' or 'scale_free'
graph_type = 'cluster'

# Choose number of samples between 250 and 500
no_samples = 250

load(paste("/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Results/SE_gl/results_SE_gl_e-d_reps_100_zinegbin_samples_",no_samples,"_p_127_cn_100_",graph_type,".RData", sep=''), verbose=TRUE)
temp1 <- temp
load(paste("/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Results/SE_mb/results_SE_mb_e-d_reps_100_zinegbin_samples_",no_samples,"_p_127_cn_100_",graph_type,".RData", sep=''), verbose=TRUE)
temp2 <- temp
load(paste("/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Results/SPRING/results_SPRING_e-d_reps_100_zinegbin_samples_",no_samples,"_p_127_cn_100_",graph_type,".RData", sep=''), verbose=TRUE)
temp3 <- temp
load(paste("/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Results/HARMONIES/results_HARMONIES_e-d_reps_100_zinegbin_samples_",no_samples,"_p_127_cn_100_",graph_type,".RData", sep=''), verbose=TRUE)
temp4 <- temp
load(paste("/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Results/gCoda/results_gCoda_e-d_reps_100_zinegbin_samples_",no_samples,"_p_127_cn_100_",graph_type,".RData", sep=''), verbose=TRUE)
temp <- res_gcoda
temp5 <- temp
load(paste("/dss/dsshome1/03/ga27hec2/Graphical-Models-for-Microbiome-Interactions/Results/COZINE/results_COZINE_e-d_reps_100_zinegbin_samples_",no_samples,"_p_127_cn_100_",graph_type,".RData", sep=''), verbose=TRUE)
temp6 <- temp

val <- values(no_samples = no_samples, graph_type = graph_type, temp1 = temp1, temp2 = temp2, temp3 = temp3, temp4 = temp4, temp5 = temp5, temp6 = temp6)
plot_violin(value = val[[1]], measure = 'AUPR', graph_type = graph_type, no_samples = no_samples)
plot_violin(value = val[[2]], measure = 'TPR', graph_type = graph_type, no_samples = no_samples)
plot_violin(value = val[[4]], measure = 'FPR', graph_type = graph_type, no_samples = no_samples)
plot_violin(value = val[[6]], measure = 'Precision', graph_type = graph_type, no_samples = no_samples)
plot_violin(value = val[[8]], measure = 'Hamming_rel', graph_type = graph_type, no_samples = no_samples)
plot_violin(value = val[[9]], measure = 'F_score', graph_type = graph_type, no_samples = no_samples)


# Check some stats
m <- stats(value = val,stat = "mean")
d <- stats(value = val,stat = "sd")
m
d
