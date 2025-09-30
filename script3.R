##################################################################################################################
#                                     Node centrality and landscape structure
##################################################################################################################

#FMestre
#30-09-2025

# 0. Load Config
# 1. Landscape metrics
# 2. Landscape metrics
# 3. Load metaweb
# 4. Load local food webs
# 5. Derive species metrics in several networks
# 6. Plot species metrics through landscape

## --------------------------------------------------------------------------------------
# 0. Load Config
## --------------------------------------------------------------------------------------

source("config.R")

## --------------------------------------------------------------------------------------
# 2. Landscape metrics
## --------------------------------------------------------------------------------------

site_metrics <- read.csv("C:\\Users\\mestr\\Documents\\0. Artigos\\brasil_predator_prey_mammal_networks\\metricas_paisagem.csv", sep = ";")

## --------------------------------------------------------------------------------------
# 3. Load metaweb
## --------------------------------------------------------------------------------------

# Load cheddar metaweb
metaweb_cheddar <- cheddar::LoadCommunity("metaweb_cheddar.RData")
#cheddar::BasalNodes(metaweb_cheddar)
#cheddar::IntermediateNodes(metaweb_cheddar)
#cheddar::TopLevelNodes(metaweb_cheddar)

# Load igraph metaweb
metaweb_igraph <- igraph::read_graph("metaweb_brasil.graphml", format = "graphml")

# Load cheddar community collection
community_collection <- cheddar::LoadCollection("community_collection_folder")
#lapply(community_collection, "BasalNodes")
#lapply(community_collection, "IntermediateNodes")
#lapply(community_collection, "TopLevelNodes")

#Checkout community properties
collectionCPS_data_frame <- CollectionCPS(community_collection, properties=NULL)

## --------------------------------------------------------------------------------------
# 4. Load local food webs
## --------------------------------------------------------------------------------------

#Load adjacency data frames for local FW
local_networks_list <- get(load("local_networks_list.RData"))

#load igraph list
local_networks_list_igraph <- get(load("local_networks_list_igraph.RData"))

#Load cheddar list
local_networks_list_cheddar <- get(load("local_networks_list_cheddar.RData"))
#Get all species from cheddar

## --------------------------------------------------------------------------------------
# 5. Derive species metrics in several networks
## --------------------------------------------------------------------------------------

# Node Metrics in Several Networks - igraph
in_degree_list <- lapply(local_networks_list_igraph, function(g) igraph::degree(g, mode = "in"))
out_degree_list <- lapply(local_networks_list_igraph, function(g) igraph::degree(g, mode = "out"))
total_degree_list <- lapply(local_networks_list_igraph, function(g) igraph::degree(g, mode = "all"))
#
closenness_list <- lapply(local_networks_list_igraph, "closeness")
betweenness_list <- lapply(local_networks_list_igraph, "betweenness")

#Do this using lapply
page_rank_list <- lapply(local_networks_list_igraph, function(g) page_rank(g)$vector)
ivi_list <- lapply(local_networks_list_igraph, "ivi")

# Convert lists do data frames
in_degree_df <- list_to_df(in_degree_list)
out_degree_df <- list_to_df(out_degree_list)
total_degree_df <- list_to_df(total_degree_list)
closeness_df <- list_to_df(closenness_list)
betweenness_df <- list_to_df(betweenness_list)
page_rank_df <- list_to_df(page_rank_list)
ivi_df <- list_to_df(ivi_list)

#Node metrics - cheddar
Prey_Averaged_Trophic_Level <- sapply(community_collection, "PreyAveragedTrophicLevel")
Shortest_Trophic_Level <- sapply(community_collection, "ShortestTrophicLevel")
Short_Weighted_Trophic_Level <- sapply(community_collection, "ShortWeightedTrophicLevel")
Longest_Trophic_Level <- sapply(community_collection, "LongestTrophicLevel")
Long_Weighted_Trophic_Level <- sapply(community_collection, "LongWeightedTrophicLevel")
Chain_Averaged_Trophic_Level <- sapply(community_collection, "ChainAveragedTrophicLevel")
Trophic_Height <- sapply(community_collection, "TrophicHeight")

# First, get all unique species across all cheddar networks
all_cheddar_species <- lapply(local_networks_list, "colnames")
all_cheddar_species <- unique(as.vector(unlist(all_cheddar_species)))

# Convert lists do data frames
Prey_Averaged_Trophic_Level_df <- cheddar_list_to_df(Prey_Averaged_Trophic_Level, all_cheddar_species)
Shortest_Trophic_Level_df <- cheddar_list_to_df(Shortest_Trophic_Level, all_cheddar_species)
Short_Weighted_Trophic_Level_df <- cheddar_list_to_df(Short_Weighted_Trophic_Level, all_cheddar_species)
Longest_Trophic_Level_df <- cheddar_list_to_df(Longest_Trophic_Level, all_cheddar_species)
Long_Weighted_Trophic_Level_df <- cheddar_list_to_df(Long_Weighted_Trophic_Level, all_cheddar_species)
Chain_Averaged_Trophic_Level_df <- cheddar_list_to_df(Chain_Averaged_Trophic_Level, all_cheddar_species)
Trophic_Height_df <- cheddar_list_to_df(Trophic_Height, all_cheddar_species)

# Save all data frames
save(in_degree_df, file = "in_degree_df.RData")
save(out_degree_df, file = "out_degree_df.RData")
save(total_degree_df, file = "total_degree_df.RData")
save(closeness_df, file = "closeness_df.RData")
save(betweenness_df, file = "betweenness_df.RData")
save(page_rank_df, file = "page_rank_df.RData")
save(ivi_df, file = "ivi_df.RData")
save(Prey_Averaged_Trophic_Level_df, file = "Prey_Averaged_Trophic_Level_df.RData")
save(Shortest_Trophic_Level_df, file = "Shortest_Trophic_Level_df.RData")
save(Short_Weighted_Trophic_Level_df, file = "Short_Weighted_Trophic_Level_df.RData")
save(Longest_Trophic_Level_df, file = "Longest_Trophic_Level_df.RData")
save(Long_Weighted_Trophic_Level_df, file = "Long_Weighted_Trophic_Level_df.RData")
save(Chain_Averaged_Trophic_Level_df, file = "Chain_Averaged_Trophic_Level_df.RData")
save(Trophic_Height_df, file = "Trophic_Height_df.RData")

## --------------------------------------------------------------------------------------
# 6. Plot species metrics through landscape
## --------------------------------------------------------------------------------------

## IVI ----------------------------------------------------------------------------------

ivi_df_T <- t(ivi_df)
landscape_metrics_ivi_df_T <- cbind(site_metrics, ivi_df_T)
#names(landscape_metrics_ivi_df_T)[27:54]

plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,27], main = names(landscape_metrics_ivi_df_T)[27])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,28], main = names(landscape_metrics_ivi_df_T)[28])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,29], main = names(landscape_metrics_ivi_df_T)[29])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,30], main = names(landscape_metrics_ivi_df_T)[30])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,31], main = names(landscape_metrics_ivi_df_T)[31])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,32], main = names(landscape_metrics_ivi_df_T)[32])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,33], main = names(landscape_metrics_ivi_df_T)[33])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,34], main = names(landscape_metrics_ivi_df_T)[34])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,35], main = names(landscape_metrics_ivi_df_T)[35])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,36], main = names(landscape_metrics_ivi_df_T)[36])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,37], main = names(landscape_metrics_ivi_df_T)[37])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,38], main = names(landscape_metrics_ivi_df_T)[38])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,39], main = names(landscape_metrics_ivi_df_T)[39])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,40], main = names(landscape_metrics_ivi_df_T)[40])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,41], main = names(landscape_metrics_ivi_df_T)[41])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,42], main = names(landscape_metrics_ivi_df_T)[42])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,43], main = names(landscape_metrics_ivi_df_T)[43])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,44], main = names(landscape_metrics_ivi_df_T)[44])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,45], main = names(landscape_metrics_ivi_df_T)[45])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,46], main = names(landscape_metrics_ivi_df_T)[46])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,47], main = names(landscape_metrics_ivi_df_T)[47])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,48], main = names(landscape_metrics_ivi_df_T)[48])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,49], main = names(landscape_metrics_ivi_df_T)[49])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,50], main = names(landscape_metrics_ivi_df_T)[50])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,51], main = names(landscape_metrics_ivi_df_T)[51])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,52], main = names(landscape_metrics_ivi_df_T)[52])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,53], main = names(landscape_metrics_ivi_df_T)[53])
plot(landscape_metrics_ivi_df_T$NatV_1400, landscape_metrics_ivi_df_T[,54], main = names(landscape_metrics_ivi_df_T)[54])

## Betweenness -------------------------------------------------------------------------

betweenness_df_T <- t(betweenness_df)
landscape_metrics_betweenness_df_T <- cbind(site_metrics, betweenness_df_T)
#names(landscape_metrics_betweenness_df_T)[27:54]

plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,27], main = names(landscape_metrics_betweenness_df_T)[27])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,28], main = names(landscape_metrics_betweenness_df_T)[28])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,29], main = names(landscape_metrics_betweenness_df_T)[29])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,30], main = names(landscape_metrics_betweenness_df_T)[30])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,31], main = names(landscape_metrics_betweenness_df_T)[31])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,32], main = names(landscape_metrics_betweenness_df_T)[32])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,33], main = names(landscape_metrics_betweenness_df_T)[33])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,34], main = names(landscape_metrics_betweenness_df_T)[34])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,35], main = names(landscape_metrics_betweenness_df_T)[35])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,36], main = names(landscape_metrics_betweenness_df_T)[36])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,37], main = names(landscape_metrics_betweenness_df_T)[37])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,38], main = names(landscape_metrics_betweenness_df_T)[38])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,39], main = names(landscape_metrics_betweenness_df_T)[39])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,40], main = names(landscape_metrics_betweenness_df_T)[40])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,41], main = names(landscape_metrics_betweenness_df_T)[41])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,42], main = names(landscape_metrics_betweenness_df_T)[42])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,43], main = names(landscape_metrics_betweenness_df_T)[43])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,44], main = names(landscape_metrics_betweenness_df_T)[44])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,45], main = names(landscape_metrics_betweenness_df_T)[45])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,46], main = names(landscape_metrics_betweenness_df_T)[46])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,47], main = names(landscape_metrics_betweenness_df_T)[47])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,48], main = names(landscape_metrics_betweenness_df_T)[48])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,49], main = names(landscape_metrics_betweenness_df_T)[49])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,50], main = names(landscape_metrics_betweenness_df_T)[50])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,51], main = names(landscape_metrics_betweenness_df_T)[51])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,52], main = names(landscape_metrics_betweenness_df_T)[52])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,53], main = names(landscape_metrics_betweenness_df_T)[53])
plot(landscape_metrics_betweenness_df_T$NatV_1400, landscape_metrics_betweenness_df_T[,54], main = names(landscape_metrics_betweenness_df_T)[54])




