
library(SpiecEasi)
library(igraph)
library(Matrix)

# If the data is available, infer the networks; otherwise load results obtained and saved previously
data_is_available = FALSE

if (data_is_available){
  
  ###
  # Process data
  ###
  
  data_sex <- read.csv("HGP_SEX.csv")
  
  # Split data based on sex
  data_sex_male <- data_sex[data_sex$Outcome == 'male',]
  data_sex_female <- data_sex[data_sex$Outcome == 'female',]
  dim(data_sex_male)
  dim(data_sex_female)
  
  #From the 31st column starts the OTU table
  dim(data_sex_male[,31:ncol(data_sex_male)])
  
  
  # Get OTU tables
  data_sex_male_OTU_table <- as.matrix(data_sex_male[,31:ncol(data_sex_male)])
  data_sex_male_OTU_table <- matrix(as.numeric(data_sex_male_OTU_table), ncol = ncol(data_sex_male_OTU_table))
  data_sex_male_OTU_table
  data_sex_female_OTU_table <- as.matrix(data_sex_female[,31:ncol(data_sex_female)])
  data_sex_female_OTU_table <- matrix(as.numeric(data_sex_female_OTU_table), ncol = ncol(data_sex_female_OTU_table))
  data_sex_female_OTU_table
  
  taxa_names <- as.character(data_sex[1,31:ncol(data_sex)])
  save(taxa_names, file = "taxa_names.RData")
  
  ###
  # Infer networks on the data
  ###
  
  library(SpiecEasi)
  library(SPRING)
  
  se.mb.data_sex_male <- SpiecEasi::spiec.easi(data_sex_male_OTU_table, method='mb', lambda.min.ratio=1e-2, nlambda=30)
  save(se.mb.data_sex_male, file = "se.mb.data_sex_male.RData")
  
  se.mb.data_sex_female <- SpiecEasi::spiec.easi(data_sex_female_OTU_table, method='mb', lambda.min.ratio=1e-2, nlambda=30)
  save(se.mb.data_sex_female, file = "se.mb.data_sex_female.RData")
  
  SPRING.data_sex_male <- SPRING::SPRING(data_sex_male_OTU_table, Rmethod = "original", quantitative = TRUE, lambdaseq = "data-specific", nlambda = 30)
  save(SPRING.data_sex_male, file = "SPRING.data_sex_male.RData")
  
  SPRING.data_sex_female <- SPRING::SPRING(data_sex_female_OTU_table, Rmethod = "original", quantitative = TRUE, lambdaseq = "data-specific", nlambda = 30)
  save(SPRING.data_sex_female, file = "SPRING.data_sex_female.RData")
  
}

# Load results
load("se.mb.data_sex_male.RData", verbose = TRUE)
load("se.mb.data_sex_female.RData", verbose = TRUE)
load("SPRING.data_sex_male.RData", verbose = TRUE)
load("SPRING.data_sex_female.RData", verbose = TRUE)


#! De schimbat aici
library(igraph)
coordinates_and_sizes_of_vertices_are_available = TRUE
if(!coordinates_and_sizes_of_vertices_are_available){
  ig.mb <- SpiecEasi::adj2igraph(getRefit(se.mb))
  set.seed(3)
  am.coord <- igraph::layout.fruchterman.reingold(ig.ground_truth)
}
set.seed(3)
am.coord <- igraph::layout.fruchterman.reingold(ig.ground_truth)

# Load coordinates of vertices and their sizes saved previously so that you obtain the same graph layout for comparison purposes
load("am.coord.RData",verbose = TRUE)
am.coord <- am.c
vsize <- vs


load("taxa_names.RData")


# Plot networks separately


ig.mb.male <- SpiecEasi::adj2igraph(getRefit(se.mb.data_sex_male), vertex.attr = list(name=taxa_names))
pdf("../Plots/Plots_AGP_data/SE_mb_male.pdf")
plot(ig.mb.male, layout=am.coord, vertex.size=vsize, vertex.label=1:length(V(ig.mb.male)), vertex.label.cex = 1.3, main="SPIEC EASI mb - male", cex.main = 3)
dev.off()

ig.mb.female <- SpiecEasi::adj2igraph(getRefit(se.mb.data_sex_female), vertex.attr = list(name=taxa_names))
pdf("../Plots/Plots_AGP_data/SE_mb_female.pdf")
plot(ig.mb.female, layout=am.coord, vertex.size=vsize, vertex.label=1:length(V(ig.mb.female)), main="SPIEC EASI mb - female")
dev.off()

ig.SPRING.male <- SpiecEasi::adj2igraph(as.matrix(SPRING.data_sex_male$fit$est$path[[SPRING.data_sex_male$output$stars$opt.index]]), vertex.attr = list(name=taxa_names))
pdf("../Plots/Plots_AGP_data/SPRING_male.pdf")
plot(ig.SPRING.male, layout=am.coord, vertex.size=vsize, vertex.label=1:length(V(ig.SPRING.male)), main="SPRING - male")
dev.off()


ig.SPRING.female <- SpiecEasi::adj2igraph(as.matrix(SPRING.data_sex_female$fit$est$path[[SPRING.data_sex_female$output$stars$opt.index]]), vertex.attr = list(name=taxa_names))
pdf("../Plots/Plots_AGP_data/SPRING_female.pdf")
plot(ig.SPRING.female, layout=am.coord, vertex.size=vsize, vertex.label=NA, vertex.label=1:length(V(ig.SPRING.female)), main="SPRING - female")
dev.off()

# Load file with necessary functions
source("../Analyse_data/manage_networks.R")

# Get the union graphs with weights and scaled down weights of the intersection edges
union <- get_weighted_graph(ig.mb.male,ig.mb.female,getOptMerge(se.mb.data_sex_male),getOptMerge(se.mb.data_sex_female),taxa_names)

# Get a colored network of the union graph, highlighting the symmetric difference edges and
# making the intersection edges thin and gray
colored_plot(ig.mb.male, ig.mb.female, union, taxa_names)





