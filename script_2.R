##################################################################################################################
#                                     Analysis based on Mestre et al. (2022)
##################################################################################################################

#FMestre
#29-09-2025

# 0. Load Config
# 1. Distance to pure topologies (as in Mestre et al 2022)
# 2. Combine with Network Metrics

## --------------------------------------------------------------------------------------
# 0. Load Config
## --------------------------------------------------------------------------------------

source("config.R")

## --------------------------------------------------------------------------------------
# 1. Distance to pure topologies (as in Mestre et al 2022)
## --------------------------------------------------------------------------------------

RPL_RG <- compare_degree_distributions(local_networks_list_igraph)
plot(RPL_RG)

RPL_RG$dist_to_PG <- sqrt((RPL_RG$RPL - 1)^2 + (RPL_RG$RG - 0)^2)
RPL_RG$dist_to_PL <- sqrt((RPL_RG$RPL - 0)^2 + (RPL_RG$RG - 1)^2)

rownames(RPL_RG) <- names(local_networks_list_igraph)

#Save
write.csv(RPL_RG, file = "RPL_RG.csv")
#Read
RPL_RG <- read.csv("RPL_RG.csv", row.names = 1)

#plot with a rede gradient using the dost_to_10 column
plot(RPL_RG[,1:2])

# Basic scatter plot with colors based on the third column
plot(RPL_RG[,1:2], col = heat.colors(nrow(RPL_RG))[rank(RPL_RG$dist_to_PG)], pch = 19, 
     xlab = "RPL", ylab = "RG", main = "Distance to PG")


plot(RPL_RG[,1:2], col = heat.colors(nrow(RPL_RG))[rank(RPL_RG$dist_to_PL)], pch = 19, 
     xlab = "RPL", ylab = "RG", main = "Distance to PL")



## --------------------------------------------------------------------------------------
# 2. Combine with Network Metrics
## --------------------------------------------------------------------------------------

#Load metrics
metrics_df <- read.csv("metrics_df.csv", row.names = 1)

#Combine with RPL_RG data frame
metrics_with_distances_to_pure_topologies <- data.frame(RPL_RG, metrics_df)

#View
#View(metrics_with_distances_to_pure_topologies)

#Save & Read
write.csv(metrics_with_distances_to_pure_topologies, file = "metrics_with_distances_to_pure_topologies.csv")
metrics_with_distances_to_pure_topologies <- read.csv("metrics_with_distances_to_pure_topologies.csv", row.names = 1)
metrics_with_distances_to_pure_topologies <- metrics_with_distances_to_pure_topologies[,-5]

#Load site metrics
site_metrics <- read.csv("C:\\Users\\mestr\\Documents\\0. Artigos\\brasil_predator_prey_mammal_networks\\metricas_paisagem.csv", sep = ";")

#Explore
#View(metrics_with_distances_to_pure_topologies)
#View(site_metrics)
response_vars <- names(metrics_with_distances_to_pure_topologies[,-c(1:2)])
predictive_vars <- names(site_metrics)[-1]

sig_matrix <- data.frame(matrix(nrow = length(response_vars), ncol = length(predictive_vars)))
colnames(sig_matrix) <- predictive_vars
rownames(sig_matrix) <- response_vars

for (i in 1:length(response_vars)) {
  for (j in 1:length(predictive_vars)) {
    


    #
    p1 <- glm(metrics_with_distances_to_pure_topologies[,response_vars[i]] ~ site_metrics[,predictive_vars[j]])
    
    # Extract coefficients with p-values
    sig_level <- round(coef(summary(p1))[2,4], 3)

    if(sig_level < 0.05){

      plot(
          site_metrics[, predictive_vars[j]],
          metrics_with_distances_to_pure_topologies[, response_vars[i]],
          xlab = predictive_vars[j],     # label for the x-axis
          ylab = response_vars[i]        # label for the y-axis
          )
      # Add model fit
      xvals <- seq(min(site_metrics[, predictive_vars[j]], na.rm = TRUE),
             max(site_metrics[, predictive_vars[j]], na.rm = TRUE),
             length.out = 200)

      yvals <- predict(p1, newdata = data.frame(`site_metrics[, predictive_vars[j]]` = xvals), type = "response")

      lines(xvals, yvals, col = "red", lwd = 2)
      
    }
    
    sig_matrix[i, j] <- sig_level
  }

}

#View
View(sig_matrix)
View(sig_matrix < 0.05)
#Save
write.csv(sig_matrix, file = "sig_matrix.csv")
