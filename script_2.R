##################################################################################################################
#                                     Analysis based on Mestre et al. (2022)
##################################################################################################################

#FMestre
#29-09-2025

## --------------------------------------------------------------------------------------
#  -- Paper --
## --------------------------------------------------------------------------------------

# Mestre, F., Rozenfeld, A., & Araújo, M. B. (2022). Human disturbances affect the topology 
# of food webs. Ecology Letters, 25(11), 2476-2488.
https://doi.org/10.1111/ele.14107

## --------------------------------------------------------------------------------------

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
  
# Get variable names as strings
resp_name <- response_vars[i]
pred_name <- predictive_vars[j]

# Build formula using variable names
formula <- as.formula(paste(resp_name, "~", pred_name))

# Prepare data frame for modeling
model_data <- data.frame(
  response = metrics_with_distances_to_pure_topologies[, resp_name],
  predictor = site_metrics[, pred_name]
)
names(model_data) <- c(resp_name, pred_name)

# Fit GLM model
p1 <- glm(formula, data = model_data)

# Extract p-value for the predictor coefficient
sig_level <- round(coef(summary(p1))[2, 4], 3)

# Only plot if predictor significant at 0.05 level
if (sig_level < 0.05) {
  # Scatter plot of predictor vs response
  plot(
    site_metrics[, pred_name],
    metrics_with_distances_to_pure_topologies[, resp_name],
    xlab = pred_name,
    ylab = resp_name
  )
  
  # Generate sequence of predictor values for fitted line
  xvals <- seq(
    min(site_metrics[, pred_name], na.rm = TRUE),
    max(site_metrics[, pred_name], na.rm = TRUE),
    length.out = 200
  )
  
  # Create newdata data frame for prediction with correct column name
  newdata <- data.frame(xvals)
  names(newdata) <- pred_name
  
  # Predict fitted values on newdata
  yvals <- predict(p1, newdata = newdata, type = "response")
  
  # Add fitted line to plot
  lines(xvals, yvals, col = "red", lwd = 2)

  sig_matrix[i, j] <- sig_level

        }
    }
}

#View
View(sig_matrix)
View(sig_matrix < 0.05)

#Save
write.csv(sig_matrix, file = "sig_matrix.csv")
