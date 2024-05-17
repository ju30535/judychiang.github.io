library(igraph)
library(ggplot2)
library(statnet)
library(intergraph)
library(sna)

wd <- "your path"
setwd(wd)

# parameters 
igraph_options(
  vertex.size = 5, edge.arrow.mode = "-",
  vertex.label.dist = 1.5, vertex.label.font = 3
)

"CA-HepTh.txt"
name <- c("CA-GrQc.txt")

for (n in name) {
  title <- ifelse(grepl("GrQc", n), "General Relativity", "High Energy Physics")
  
  data <- read.table(n, skip = 4, header = FALSE)
  data_matrix <- t(apply(data, 1, sort))
  unique_edges <- unique(data_matrix)
  nodes <- unique(c(unique_edges[,1], unique_edges[,2]))
  
  # Initialize an empty adjacency matrix
  adj_matrix <- matrix(0, nrow = length(nodes), ncol = length(nodes))
  
  # Iterate over the unique edges and fill in the adjacency matrix
  for (i in 1:nrow(unique_edges)) {
    # Get indices of the nodes in the adjacency matrix
    node1_index <- which(nodes == unique_edges[i,1])
    node2_index <- which(nodes == unique_edges[i,2])
    
    # Set the corresponding entry in the adjacency matrix to 1
    adj_matrix[node1_index, node2_index] <- 1
    adj_matrix[node2_index, node1_index] <- 1  # Assuming undirected graph
  }
  
  # Create a network object
  data <- network(adj_matrix, directed = FALSE)
  
  graph <- asIgraph(data)
  igraph::plot.igraph(graph, vertex.label = NA, main = paste("Network (", title, ")"))
  
  # maximum degree of node/ degree distribution
  degree.max <- max(igraph::degree(graph))
  degree.mean <- mean(igraph::degree(graph))
  cat( title, "max/ mean/ mode degree of nodes = ", degree.max, " / ", degree.mean, "\n")
  
  plot(
    x = 0:max(igraph::degree(graph)),
    y = degree_distribution(graph), type = "b", 
    main = paste0("Degree Distribution (", title, ")"), 
    xlab = "Degree", ylab = "Proportion", xlim = c(0, degree.max)
  )
  
  # betweenness distribution
  betweenness_dist <- betweenness(data)
  max.be <- max(betweenness_dist)
  mean.be <- mean(betweenness_dist)
  cat( title, "max/ mean betweenness = ", max.be, " / ",  mean.be, "\n")
  # still figuring out the distribution plot for betweenness -> error on length for y
  # hist(betweenness_dist, main = paste0("Betweenness Distribution (", title, ")"), xlab = "Betweenness", ylab = "Frequency") # working hist / not preferred fomat
  betweenness_freq <- table(betweenness_dist)
  plot(
    x = as.numeric(names(betweenness_freq)),  # Convert names to numeric for x-axis
    y = betweenness_freq, type = "b", 
    main = paste0("Betweenness Distribution (", title, ")"), 
    xlab = "Betweenness", ylab = "Frequency"
  )
  
  
  # density
  d <- graph.density(graph) 
  cat( title, "density = ", d, "\n")
  
  # transitivity = clustering coefficient
  t <- gtrans(data)
  cat( title, "transitivity = ", t, "\n")
  
  # geodisc <- memory exhausted
  d2 <- diameter(graph) # diameter = largest component's maximal geodesic
  cat( title, "diameter = ", d2, "\n")
  
  # paths <- geodist(data, predecessors = TRUE)
  # print(paths)

  
  probs <- c(0.005, 0.01, 0.05, 0.075, 0.08, 0.1, 0.5)
  
  for (prob in probs) {
    ig_smwrld <- watts.strogatz.game(dim=1,size=vcount(graph),nei=2,p= prob)
    g_smwrld <- intergraph::asNetwork(ig_smwrld)
    t <- gtrans(g_smwrld)
    cat("samll world (p =", prob , ") transtivity = ", t, "\n")
  }
  
  
}

# centrality measures for closeness distribution
#Please note this code was developed in R but adapted to Python as seen in the report plots for better visualization.
# Parameters
igraph_options(
  vertex.size = 5, edge.arrow.mode = "-",
  vertex.label.dist = 1.5, vertex.label.font = 3
)

# Files to process
name <- c("CA-GrQc.txt", "CA-HepTh.txt")

for (n in name) {
  title <- ifelse(grepl("GrQc", n), "General Relativity", "High Energy Physics")
  
  # Read the data and prepare the adjacency matrix
  data <- read.table(n, skip = 4, header = FALSE)
  data_matrix <- t(apply(data, 1, sort))
  unique_edges <- unique(data_matrix)
  nodes <- unique(c(unique_edges[,1], unique_edges[,2]))
  
  adj_matrix <- matrix(0, nrow = length(nodes), ncol = length(nodes))
 
  for (i in 1:nrow(unique_edges)) {
    node1_index <- which(nodes == unique_edges[i,1])
    node2_index <- which(nodes == unique_edges[i,2])
    
    adj_matrix[node1_index, node2_index] <- 1
    adj_matrix[node2_index, node1_index] <- 1
  }
  
  # Create a network object and convert to igraph
  network_obj <- network(adj_matrix, directed = FALSE)
  graph <- asIgraph(network_obj)
  
  igraph::plot.igraph(graph, vertex.label = NA, main = paste("Network (", title, ")"))
  
  # Calculate closeness
  closeness_dist <- igraph::closeness(graph, normalized = TRUE)  # Use normalized closeness
  
  # Plot the closeness distribution
  closeness_freq <- table(round(closeness_dist, 5))  # Round to reduce floating-point issues
  plot(
    x = as.numeric(names(closeness_freq)),
    y = closeness_freq, type = "b",
    main = paste0("Closeness Centrality Distribution (", title, ")"),
    xlab = "Closeness Centrality", ylab = "Frequency"
  )
}



