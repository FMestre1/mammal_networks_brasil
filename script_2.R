##################################################################################################################
#                                     Mammal networks and landscape structure
##################################################################################################################

#FMestre
#29-09-2025

# Compute ------------------------------------------------------------------------------------------
RPL_RG <- compare_degree_distributions(local_networks_list_igraph)
plot(RPL_RG)

RPL_RG$dist_to_PG <- sqrt((RPL_RG$RPL - 1)^2 + (RPL_RG$RG - 0)^2)
RPL_RG$dist_to_PL <- sqrt((RPL_RG$RPL - 0)^2 + (RPL_RG$RG - 1)^2)

rownames(RPL_RG) <- names(local_networks_list_igraph)

#Save
write.csv(RPL_RG, file = "RPL_RG.csv")


#plot with a rede gradient using the dost_to_10 column
plot(RPL_RG[,1:2])

# Basic scatter plot with colors based on the third column
plot(RPL_RG[,1:2], col = heat.colors(nrow(RPL_RG))[rank(RPL_RG$dist_to_PG)], pch = 19, 
     xlab = "RPL", ylab = "RG", main = "Distance to PG")


plot(RPL_RG[,1:2], col = heat.colors(nrow(RPL_RG))[rank(RPL_RG$dist_to_PL)], pch = 19, 
     xlab = "RPL", ylab = "RG", main = "Distance to PL")


# Combine with metrics -----------------------------------------------------------------------------

#Load nmetrics
metrics_df <- read.csv("metrics_df.csv", row.names = 1)

#Combine
metrics_with_distances_to_pure_topologies <- data.frame(RPL_RG, metrics_df)

#View
#View(metrics_with_distances_to_pure_topologies)

site_metrics <- read.csv("C:\\Users\\mestr\\Documents\\0. Artigos\\brasil_predator_prey_mammal_networks\\metricas_paisagem.csv", sep = ";")

#Explore
plot(metrics_with_distances_to_pure_topologies$LD_link_Density, site_metrics$ENN_3000)
#
glm(metrics_with_distances_to_pure_topologies$LD_link_Density ~ site_metrics$ENN_3000)

