##################################################################################################################
#                                     Proportion of the Metaweb Represented Locally
##################################################################################################################

#FMestre
#30-09-2025

source("config.R")

## --------------------------------------------------------------------------------------
# 0. Landscape metrics
## --------------------------------------------------------------------------------------

site_metrics <- read.csv("C:\\Users\\mestr\\Documents\\0. Artigos\\brasil_predator_prey_mammal_networks\\metricas_paisagem.csv", sep = ";")

## --------------------------------------------------------------------------------------
# 1. Load metaweb
## --------------------------------------------------------------------------------------

# Load igraph metaweb
metaweb_igraph <- igraph::read_graph("metaweb_brasil.graphml", format = "graphml")

# Create a data frame of edges
edges_metaweb_df <- igraph::as_data_frame(metaweb_igraph, what = "edges")

## --------------------------------------------------------------------------------------
# 2. Load local food webs
## --------------------------------------------------------------------------------------

#load igraph list
local_networks_list_igraph <- get(load("local_networks_list_igraph.RData"))

percentage_overlap <- rep(NA, length(local_networks_list_igraph))

# Percentage of overlap
for(i in 1:length(local_networks_list_igraph)){
  
edges_local_df <- igraph::as_data_frame(local_networks_list_igraph[[i]], what = "edges")
percentage_overlap[[i]] <- round((nrow(edges_local_df)/nrow(edges_metaweb_df))*100, 3)

}

fraction_metaweb_in_local <- data.frame(names(local_networks_list_igraph), percentage_overlap)
names(fraction_metaweb_in_local) <- c("site", "fraction_metaweb_in_local")

#
overlap1 <- cbind(site_metrics, fraction_metaweb_in_local)

landscape_metrics <- names(site_metrics)[-1]

p_value <- rep(NA, length(landscape_metrics))

for(i in 1:length(landscape_metrics)){

model1 <- glm(overlap1[,landscape_metrics[i]] ~ overlap1$fraction_metaweb_in_local)
sum1 <- summary(model1)
p_value[i] <- sum1$coefficients[2,4]

}

p_value < 0.05 # No single variable is significant!

### -----------------------------------------------------------------------------------------
#                  Generate all possible models up until three variables
### -----------------------------------------------------------------------------------------

# Vector of candidate predictors
landscape_metrics <- names(site_metrics)[-1]

# Correlation filter (remove highly correlated vars first, e.g. > 0.7)
cor_matrix <- cor(site_metrics[, landscape_metrics], use = "pairwise.complete.obs")
highly_correlated <- caret::findCorrelation(cor_matrix, cutoff = 0.7)  # needs caret
uncorrelated_metrics <- landscape_metrics[-highly_correlated]

# Generate all combinations of up to 3 predictors
all_combos <- unlist(
  lapply(1:3, function(k) combn(uncorrelated_metrics, k, simplify = FALSE)),
  recursive = FALSE
)

# Storage for results
results <- data.frame(
  predictors = character(),
  AIC = numeric(),
  p_values = I(list())
)

coef_tables <- list()

# Loop through all combinations
for (combo in all_combos) {
  formula <- as.formula(
    paste("fraction_metaweb_in_local ~", paste(combo, collapse = " + "))
  )
  model <- glm(formula, data = overlap1)
  sum_model <- summary(model)
  
  results <- rbind(
    results,
    data.frame(
      predictors = paste(combo, collapse = " + "),
      AIC = AIC(model),
      p_values = I(list(sum_model$coefficients[-1, 4])) # drop intercept
    )
  )

    # Store coefficient table
  coef_tables[[paste(combo, collapse = " + ")]] <- sum_model
  #coef_tables[[paste(combo, collapse = " + ")]] <- sum_model$coefficients

}

# Look at significant models (any term < 0.05)
sig_models <- results[sapply(results$p_values, function(p) any(p < 0.05)), ]
sig_models <- sig_models[order(sig_models$AIC), ]
sig_models[1,]

coef_tables[[50]]
#coef_tables[[343]]




















