##################################################################################################################
#                                  Load data and creating igraph and cheddar objects
#                                                   and network metrics                   
##################################################################################################################

#FMestre
#25-09-2025

# 0. Load Config
# 1. Load data 
# 2. Create metaweb graph and save it in GraphML format
# 3. Convert metaweb to cheddar community
# 4. Create local networks adjacency matrices
# 5. Create igraph and cheddar objects representing local FW
# 6. Derive network metrics
  # 6.1. Based on the igraph list "local_networks_list_igraph"
  # 6.2. Using package NetIndices
  # 6.3. Based on the cheddar community collection

## --------------------------------------------------------------------------------------
# 0. Load Config
## --------------------------------------------------------------------------------------

source("config.R")

## --------------------------------------------------------------------------------------
# 1. Load data
## --------------------------------------------------------------------------------------

metaweb <- read.csv("C:\\Users\\mestr\\Documents\\0. Artigos\\brasil_predator_prey_mammal_networks\\metaweb_brasil.csv", sep=";")
site_metrics <- read.csv("C:\\Users\\mestr\\Documents\\0. Artigos\\brasil_predator_prey_mammal_networks\\metricas_paisagem.csv", sep = ";")
site_species <- read.csv("C:\\Users\\mestr\\Documents\\0. Artigos\\brasil_predator_prey_mammal_networks\\sites_species.csv", sep = ";")

#View(metaweb)
#View(site_metrics)
#View(site_species)

rownames(metaweb) <- metaweb$X
metaweb <- metaweb[,-1]
metaweb[is.na(metaweb)] <- 0

## --------------------------------------------------------------------------------------
# 2. Create metaweb graph and save it in GraphML format
## --------------------------------------------------------------------------------------

# Convert to predator-prey pairs
edges1 <- which(metaweb == 1, arr.ind = TRUE)
edges1 <- data.frame(
  predator = rownames(metaweb)[edges1[,1]],
  prey     = colnames(metaweb)[edges1[,2]]
)

#COnvert to igraph
metaweb_igraph <- igraph::graph_from_data_frame(edges1, directed = TRUE)
#igraph::V(metaweb_igraph)
#igraph::E(metaweb_igraph)
#plot(metaweb_igraph)

# Save the network in GraphML format
igraph::write_graph(metaweb_igraph, file = "metaweb_brasil.graphml", format = "graphml")

## --------------------------------------------------------------------------------------
# 3. Convert metaweb to cheddar community
## --------------------------------------------------------------------------------------

# Load adajacency data frame
metaweb_adjacency <- read.csv("C:\\Users\\mestr\\Documents\\0. Artigos\\brasil_predator_prey_mammal_networks\\metaweb_brasil.csv", sep=";")
rownames(metaweb_adjacency) <- metaweb_adjacency$X
metaweb_adjacency <- metaweb_adjacency[,-1]
metaweb_adjacency[is.na(metaweb_adjacency)] <- 0

# Convert to cheddar object
#Nodes
metaweb_nodes1 <- data.frame(colnames(metaweb_adjacency))
names(metaweb_nodes1)[1] <- "node"

#Edges
metaweb_edges1 <- which(metaweb_adjacency == 1, arr.ind = TRUE)
metaweb_edges1 <- data.frame(
  consumer = rownames(metaweb_adjacency)[metaweb_edges1[,1]],
  resource     = colnames(metaweb_adjacency)[metaweb_edges1[,2]]
)

metaweb_cheddar <- cheddar::Community(nodes = metaweb_nodes1, properties = list(title="metaweb"), trophic.links = metaweb_edges1)

# Plot
plot3d_fw(1, label_type = "all", list(metaweb_igraph), list(metaweb_cheddar))

# Save & Load
cheddar::SaveCommunity(metaweb_cheddar, "metaweb_cheddar.RData")
metaweb_cheddar <- cheddar::LoadCommunity("metaweb_cheddar.RData")

## --------------------------------------------------------------------------------------
# 4. Create local networks adjacency matrices
## --------------------------------------------------------------------------------------

sites_names <- site_metrics$paisagem
local_networks_list <- list()

for(i in 1:length(sites_names)){
  
  site1 <- sites_names[i]
  species1 <- site_species[site_species$paisagem == site1,]
  local_matrix1 <- matrix(0, ncol=nrow(species1), nrow=nrow(species1))
  colnames(local_matrix1) <- species1$especie
  rownames(local_matrix1) <- species1$especie
        for(j in 1:nrow(local_matrix1)){
          #getting the preys for 'predator1' from the metaweb
          predator1 <- species1$especie[j]
          preys1 <- metaweb[predator1,]
          #select columns with value 1
          preys1 <- colnames(preys1)[as.vector(preys1) == 1]
          preys1 <- preys1[!is.na(preys1)]
          #keeping only the preys that are present in the site
          preys_in_site <- preys1[preys1 %in% species1$especie]
          #filling the local matrix
          local_matrix1[j, colnames(local_matrix1) %in% preys_in_site] <- 1
          #local_matrix1[j, !(colnames(local_matrix1) %in% preys_in_site)] <- 0
          
        }
  #Sendo to list of local matrices
  local_networks_list[[site1]] <- local_matrix1
  
  message(site1)
          
}

#length(local_networks_list)

#Save it as RData
save(local_networks_list, file = "local_networks_list.RData")

## --------------------------------------------------------------------------------------
# 5. Create igraph and cheddar objects representing local FW
## --------------------------------------------------------------------------------------

#Create empty cheddar list of objects
local_networks_list_igraph <- list()
local_networks_list_cheddar <- list()

for(i in 1:length(local_networks_list)){

# Create folders to save individual FW objects (both igraph and cheddar)
if(!dir.exists("igraph_local_fw")) dir.create("igraph_local_fw")
if(!dir.exists("cheddar_local_fw")) dir.create("cheddar_local_fw")

#Get the adjacency matrix
local_fw1_names <- names(local_networks_list)[i]
local_fw1 <- local_networks_list[[i]]

### --- Igraph object
local_edges1 <- which(local_fw1 == 1, arr.ind = TRUE)
local_edges1 <- data.frame(
  predator = rownames(local_fw1)[local_edges1[,1]],
  prey     = colnames(local_fw1)[local_edges1[,2]]
)

local_fw1_igraph <- igraph::graph_from_data_frame(local_edges1, directed = TRUE)
#plot(local_fw1_igraph)

#Save igraph object
igraph::write_graph(local_fw1_igraph, file = paste0("igraph_local_fw/", local_fw1_names, "_igraph.graphml"), format = "graphml")
message("Saved ", local_fw1_names, " as a igraph object in ", "'",paste0(local_fw1_names, "_igraph.graphml"), "'!")

# Send to list
local_networks_list_igraph[[i]] <- local_fw1_igraph

### ---- Cheddar object

nodes1 <- data.frame(colnames(local_fw1))
names(nodes1)[1] <- "node"
names(local_edges1) <- c("consumer", "resource")

ch1 <- cheddar::Community(nodes = nodes1, properties = list(title=local_fw1_names), trophic.links = local_edges1)

metrics_ch1 <- list(
  title = local_fw1_names,
  Number_Of_Trophic_Links       = NumberOfTrophicLinks(ch1),
  Number_Of_Nodes               = NumberOfNodes(ch1),
  Fraction_Basal_Nodes          = FractionBasalNodes(ch1),
  Fraction_Intermediate_Nodes   = FractionIntermediateNodes(ch1),
  Fraction_Top_Level_Nodes      = FractionTopLevelNodes(ch1),
  Fraction_Isolated_Nodes       = FractionIsolatedNodes(ch1),
  Fraction_Non_Basal_Nodes      = FractionNonBasalNodes(ch1),
  Fraction_Connected_Nodes      = FractionConnectedNodes(ch1),
  Fraction_Non_Top_Level_Nodes  = FractionNonTopLevelNodes(ch1),
  Mean_Maximum_Trophic_Similarity = MeanMaximumTrophicSimilarity(ch1)
)

ch1 <- cheddar::Community(nodes = nodes1, properties = metrics_ch1, trophic.links = local_edges1)

#Save cheddar object
save(ch1, file = paste0("cheddar_local_fw/", local_fw1_names, "_local_FW.RData"))
message("Saved ", local_fw1_names, " as a cheddar object in ", "'", paste0(local_fw1_names, "_local_FW.RData"), "'!")

# Send to list
local_networks_list_cheddar[[i]] <- ch1

}

# Rename list
names(local_networks_list_igraph) <- names(local_networks_list)
names(local_networks_list_cheddar) <- names(local_networks_list)

#In cheddar we can create a cheddar object with all local food webs - a community
community_collection <- CommunityCollection(local_networks_list_cheddar)
#plot(community_collection)

#Checkout community properties
collectionCPS_data_frame <- CollectionCPS(community_collection, properties=NULL)

#plot one food web
plot3d_fw(1, label_type = "all", local_networks_list_igraph, local_networks_list_cheddar)

#Save the lists as an RData
save(local_networks_list_igraph, file = "local_networks_list_igraph.RData")
save(local_networks_list_cheddar, file = "local_networks_list_cheddar.RData")
save(community_collection, file = "community_collection_cheddar.RData")
cheddar::SaveCollection(community_collection, "community_collection_folder") # this is an option from cheddar to save the full community collection

#load these
load("local_networks_list_igraph.RData")
load("local_networks_list_cheddar.RData")
load("community_collection_cheddar.RData")

#Load cheddar community collection
community_collection <- cheddar::LoadCollection("community_collection_folder")

## --------------------------------------------------------------------------------------
# 6. Derive network metrics
## --------------------------------------------------------------------------------------

  # 6.1. Based on the igraph list "local_networks_list_igraph" --------------------------

# In-degree distribution - both culumative and non-cumulative

local_networks_list_igraph <- get(load("local_networks_list_igraph.RData"))

in_degree_distributions <- list()
out_degree_distributions <- list()
total_degree_distributions <- list()
#
in_degree_distributions_non_cumulative <- list()
out_degree_distributions_non_cumulative <- list()
total_degree_distributions_non_cumulative <- list()

for(i in 1:length(local_networks_list_igraph)){

# Cumulative
indeg <- degree_distribution(local_networks_list_igraph[[i]], cumulative = TRUE, mode = "in") # in degree distribution
outdeg <- degree_distribution(local_networks_list_igraph[[i]], cumulative = TRUE, mode = "out") #out degreee distribution
fulldeg <- degree_distribution(local_networks_list_igraph[[i]], cumulative = TRUE, mode = "all")  # total degree
# Non-Cumulative
indeg_nc <- degree_distribution(local_networks_list_igraph[[i]], cumulative = FALSE, mode = "in") # in degree distribution
outdeg_nc <- degree_distribution(local_networks_list_igraph[[i]], cumulative = FALSE, mode = "out") #out degreee distribution
fulldeg_nc <- degree_distribution(local_networks_list_igraph[[i]], cumulative = FALSE, mode = "all")  # total degree

#plot(indeg, type="l")
#plot(outdeg, type="l")
#plot(fulldeg, type="l")

#plot(indeg_nc, type="l")
#plot(outdeg_nc, type="l")
#plot(fulldeg_nc, type="l")
  
in_degree_distributions[[i]] <- indeg
out_degree_distributions[[i]] <- outdeg
total_degree_distributions[[i]] <- fulldeg
#
in_degree_distributions_non_cumulative[[i]] <- indeg_nc
out_degree_distributions_non_cumulative[[i]] <- outdeg_nc
total_degree_distributions_non_cumulative[[i]] <- fulldeg_nc


message(names(local_networks_list_igraph)[i])

}

names(in_degree_distributions) <- names(local_networks_list_igraph)
names(out_degree_distributions) <- names(local_networks_list_igraph)
names(total_degree_distributions) <- names(local_networks_list_igraph)

names(in_degree_distributions_non_cumulative) <- names(local_networks_list_igraph)
names(out_degree_distributions_non_cumulative) <- names(local_networks_list_igraph)
names(total_degree_distributions_non_cumulative) <- names(local_networks_list_igraph)

# Save degree distribution lists
save(in_degree_distributions, file = "in_degree_distributions.RData")
save(out_degree_distributions, file = "out_degree_distributions.RData")
save(total_degree_distributions, file = "total_degree_distributions.RData")
#
save(in_degree_distributions_non_cumulative, file = "in_degree_distributions.RData")
save(out_degree_distributions_non_cumulative, file = "out_degree_distributions.RData")
save(total_degree_distributions_non_cumulative, file = "total_degree_distributions.RData")

  # 6.2. Using package NetIndices ----------------------------------------------------------

net_indices_metrics <- data.frame(
  N_number_of_compartments = rep(NA, 55),
  T_total_System_Throughput = rep(NA, 55),
  TST_total_System_Throughflow = rep(NA, 55),
  Lint_number_of_Internal_links = rep(NA, 55),
  Ltot_total_number_of_links = rep(NA, 55),
  LD_link_Density = rep(NA, 55),
  C_connectance = rep(NA, 55),
  Tijbar_average_Link_Weight = rep(NA, 55),
  TSTbar_average_Compartment_Throughflow = rep(NA, 55),
  Cbar_compartmentalization = rep(NA, 55)
)

for(i in 1:length(local_networks_list_igraph)){
  
test.graph.adj <- as_adjacency_matrix(local_networks_list_igraph[[i]],sparse=F)
test.graph.properties <- NetIndices::GenInd(test.graph.adj)

net_indices_metrics$N_number_of_compartments[i] <- test.graph.properties$N
net_indices_metrics$T_total_System_Throughput[i] <- test.graph.properties$T..
net_indices_metrics$TST_total_System_Throughflow[i] <- test.graph.properties$TST
net_indices_metrics$Lint_number_of_Internal_links[i] <- test.graph.properties$Lint
net_indices_metrics$Ltot_total_number_of_links[i] <- test.graph.properties$Ltot
net_indices_metrics$LD_link_Density[i] <- test.graph.properties$LD
net_indices_metrics$C_connectance[i] <- test.graph.properties$C
net_indices_metrics$Tijbar_average_Link_Weight[i] <- test.graph.properties$Tijbar
net_indices_metrics$TSTbar_average_Compartment_Throughflow[i] <- test.graph.properties$TSTbar
net_indices_metrics$Cbar_compartmentalization[i] <- test.graph.properties$Cbar
  
}

rownames(net_indices_metrics) <- names(local_networks_list_igraph)

metrics_df <- data.frame(collectionCPS_data_frame,
           net_indices_metrics
          )

#View(metrics_df)

# Save
write.csv(net_indices_metrics, file = "net_indices_metrics.csv")
write.csv(collectionCPS_data_frame, file = "collectionCPS_data_frame.csv")
write.csv(metrics_df, file = "metrics_df.csv")


  # 6.3. Based on the cheddar community collection. "community_collection" -----------------
community_collection <- cheddar::LoadCollection("community_collection_folder") # this is an option from cheddar to load the full community collection

#Node metrics - cheddar
Prey_Averaged_Trophic_Level <- sapply(community_collection, "PreyAveragedTrophicLevel")
Shortest_Trophic_Level <- sapply(community_collection, "ShortestTrophicLevel")
Short_Weighted_Trophic_Level <- sapply(community_collection, "ShortWeightedTrophicLevel")
Longest_Trophic_Level <- sapply(community_collection, "LongestTrophicLevel")
Long_Weighted_Trophic_Level <- sapply(community_collection, "LongWeightedTrophicLevel")
Chain_Averaged_Trophic_Level <- sapply(community_collection, "ChainAveragedTrophicLevel")
Trophic_Height <- sapply(community_collection, "TrophicHeight")

# Save these lists
save(Prey_Averaged_Trophic_Level, file = "Prey_Averaged_Trophic_Level.RData")
save(Shortest_Trophic_Level, file = "Shortest_Trophic_Level.RData")
save(Short_Weighted_Trophic_Level, file = "Short_Weighted_Trophic_Level.RData")
save(Longest_Trophic_Level, file = "Longest_Trophic_Level.RData")
save(Long_Weighted_Trophic_Level, file = "Long_Weighted_Trophic_Level.RData")
save(Chain_Averaged_Trophic_Level, file = "Chain_Averaged_Trophic_Level.RData")
save(Trophic_Height, file = "Trophic_Height.RData")

