###### MGMT590 MAND FINAL PROJECT ######
#
# Team members :
# Chun-Yi Chiang 
# Nagraj Deshmukh
# Sahana Reddy
# Sanjay Katyal
#
###### ---------------- ######


#################
# Setup
#################

library(igraph)
library(network)
library(intergraph)

# Set working directory
wd <- "D:/Purdue/Academics/Spring/Mod4 Multivariate/Homeworks/Project"
setwd(wd)


#################
# Creating the graph/network
#################

# CA-GrQc.txt
# CA-HepTh.txt

# Read the network data, skipping the first four lines of comments
network_data <- read.table("CA-GrQc.txt", header = FALSE, skip = 4, col.names = c("V1", "V2"))

# Clean data: Remove self-loops and parallel edges
network_data <- network_data[network_data$V1 != network_data$V2, ]
network_data <- t(apply(network_data, 1, sort))
network_data <- unique(network_data)
network_data <- as.data.frame(network_data, stringsAsFactors = FALSE)
colnames(network_data) <- c("V1", "V2")

# Create an undirected network
net <- network(network_data, directed = FALSE, matrix.type = "edgelist")
g <- asIgraph(net)

#################
# Ensuring that the metrics match with the ones mentioned in the snap data source.
#################

# number of nodes
num_nodes <- length(V(g))
cat("Number of Nodes: ", num_nodes, "\n")

# Average degree
average_degree <- mean(igraph::degree(g))
cat("Average Degree: ", average_degree, "\n")

# Node with maximum degree
max_degree <- which.max(igraph::degree(g))
cat("Node with the maximum degree: ", V(g)[max_degree], " with degree ", igraph::degree(g)[max_degree], "\n")

# Components analysis
comp <- igraph::components(g)
cat("Number of components: ", comp$no, "\n")
cat("Size of largest component: ", max(comp$csize), "\n")

# Clustering Coefficient
clust_coeff <- igraph::transitivity(g, type = "average")
cat("Average Clustering Coefficient: ", clust_coeff, "\n")

# Number of triangles
triangles_count <- length(igraph::triangles(g))  
cat("Number of triangles: ", triangles_count, "\n")

# Fraction of closed triangles (global clustering coefficient)
closed_triangles <- igraph::transitivity(g, type = "global")
cat("Fraction of closed triangles: ", closed_triangles, "\n")

# Diameter of the graph
graph_diameter <- igraph::diameter(g, directed = FALSE)
cat("Diameter (longest shortest path): ", graph_diameter, "\n")

# Calculate 90-percentile effective diameter
distances <- distances(g)
valid_distances <- distances[distances != Inf & distances > 0] # Flatten the matrix and remove Inf and self-distances (zeroes)
effective_diameter90 <- quantile(valid_distances, 0.9)
cat("90-percentile effective diameter: ", effective_diameter90, "\n")


#################
# Network simulation with ERGM.
#################

library(ergm)

print(inherits(g, "network"))
print(inherits(net, "network"))

print.network(net)
summary(net ~ edges + triangle + degree(1:3) )
# + gwesp(0.25)


# Define the ERGM model and view model fit
ergm_model_edge <- ergm(net ~ edges)
summary(ergm_model_edge)

### The below ones dont work and throw the model degeneracy errors.
# summary(net ~ edges + triangle + degree(1:3) + gwesp(0.25) )
# ergm_model_tri <- ergm(net ~ triangle)
# ergm_model_deg <- ergm(net ~ degree(1:3))

# Now print the coefficients from the MPLE estimation
print(coef(ergm_model_edge))



#################
# Community Detection
#################

# Walktrap
comm_walktrap <- cluster_walktrap(g)
mod_walktrap <- modularity(comm_walktrap)
cat("Walktrap Modularity:", mod_walktrap, "\n")
num_communities <- length(communities(comm_walktrap))
cat("Number of communities detected by Walktrap:", num_communities, "\n")

# Visualization of the walktrap community clusters
plot(comm_walktrap, g, vertex.size=3, vertex.label=NA)

#################
# Community Detection with a subgraph with supernodes
#################

# Define supernodes as those with a degree greater than the 75th percentile
degrees <- igraph::degree(g)
degree_threshold <- quantile(degrees, 0.75)
supernodes <- which(degrees > degree_threshold)

# Get the neighbors of these supernodes
neighbor_ids <- unique(unlist(neighbors(g, supernodes)))

# Include supernodes and their neighbors in the subgraph
important_nodes <- unique(c(supernodes, neighbor_ids))
subg <- induced_subgraph(g, important_nodes)

num_nodes_subgraph <- length(V(subg))
cat("Number of Nodes: ", num_nodes_subgraph, "\n")

# Perform community detection on the subgraph
comm_walktrap_subg <- cluster_walktrap(subg)

# Calculate and print modularity
mod_walktrap_subg <- modularity(comm_walktrap_subg)
cat("Walktrap Modularity for subgraph:", mod_walktrap_subg, "\n")

# Print the number of communities
num_communities_subg <- length(communities(comm_walktrap_subg))
cat("Number of communities in subgraph detected by Walktrap:", num_communities_subg, "\n")

# Plot the subgraph
plot(comm_walktrap_subg, subg, vertex.size=3, vertex.label=NA, main="Subgraph with Supernodes and their Neighbors")











################################################
# Small world and scale free network simulation


