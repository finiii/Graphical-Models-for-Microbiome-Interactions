library(SpiecEasi)
library(igraph)


# Read data, get OTU table
data_sex <- read.csv("HGP_SEX.csv")
data_sex_matrix <- as.matrix(data_sex[3:nrow(data_sex),31:ncol(data_sex)])
data_sex_matrix <- matrix(as.numeric(data_sex_matrix), ncol = ncol(data_sex_matrix))
data_sex_matrix


# Load results
load("se.mb.data_sex_male.RData", verbose = TRUE)
load("se.mb.data_sex_female.RData", verbose = TRUE)

# Load taxa names
load("taxa_names.RData")


ig.mb.male <- SpiecEasi::adj2igraph(getRefit(se.mb.data_sex_male), vertex.attr = list(name=taxa_names))
ig.mb.female <- SpiecEasi::adj2igraph(getRefit(se.mb.data_sex_female), vertex.attr = list(name=taxa_names))


# Get indices of nodes that are connected in the symmetric difference graph
se.mb.symm.diff <- symm_diff(ig.mb.male,ig.mb.female, taxa_names)[[1]]
s.diff <- as.matrix(igraph::as_adjacency_matrix(se.mb.symm.diff))
cs <- colSums(s.diff)
indices <- (1:ncol(s.diff))[cs!=0]



# Get sex variable. Assign 0 and 1 values to categories
v <- data_sex$Outcome[3:length(data_sex$Outcome)]
v[v == 'male'] <- 0
v[v == 'female'] <- 1
v <- as.numeric(v)

data <- as.data.frame(data_sex_matrix[,indices])
colnames(data) <- paste0("T", indices)
colnames(data)


# Build data frame with indicator functions of the variables
q <- final > 0
q <- as.data.frame(matrix(as.numeric(q), ncol = ncol(q)))
colnames(q) <- paste0("I_",colnames(final))
colnames(q)

# Get the final data frame
final <- cbind(data,q)
# Add the predicted variable
final$Y <- v

# Get first formula for the glm function
f <- paste(colnames(final)[1:42], collapse= "+")
f <- as.formula(paste("Y ~ ", f))
f

# Perform logistic regression with main effects to see which are significant
log_regression <- glm(formula = g,data = final, family=binomial(link='logit'))
summary(log_regression)


# Pick the indices corresponding to the significant taxa according to the results from above
significant_indices = c(4,5,7,10,11,18,19,22,42)

# Get formula for the glm function
g <- paste(colnames(final)[significant_indices], collapse= "+")
h <- paste(colnames(final)[43:84][significant_indices], collapse= "+")
h_ <- paste("(",h,")","^2",sep="")
g_h <- as.formula(paste("Y ~ ", paste(g,h_,sep="+"),"-","(",h,")"))
g_h


# Perform logistic regression on significant taxa as main effects, plus interactions of the indicator functions
# of the corresponding taxa
log_regression_2 <- glm(formula = f_3_4,data = final, family=binomial(link='logit'))
summary(log_regression_2)


