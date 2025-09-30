
### ----------------------------------------------------------------------
#install.packages("MuMIn")
#library(MuMIn)
### ----------------------------------------------------------------------

# Suppose you already have:
# uncorrelated_metrics <- landscape_metrics[-highly_correlated]

# Vector of candidate predictors
landscape_metrics <- names(site_metrics)[-1]

# Correlation filter (remove highly correlated vars first, e.g. > 0.7)
cor_matrix <- cor(site_metrics[, landscape_metrics], use = "pairwise.complete.obs")
highly_correlated <- caret::findCorrelation(cor_matrix, cutoff = 0.7)  # needs caret
uncorrelated_metrics <- landscape_metrics[-highly_correlated]

# Build formula dynamically:
#  - main effects
#  - squared terms with I(x^2)

main_terms    <- paste(uncorrelated_metrics, collapse = " + ")
squared_terms <- paste0("I(", uncorrelated_metrics, "^2)", collapse = " + ")

# Combine into one formula string
formula_str <- paste(
  "fraction_metaweb_in_local ~",
  main_terms, "+",
  squared_terms
)

# Turn string into formula
global_formula <- as.formula(formula_str)

# Define global model
global_model <- glm(
  global_formula,
  data = overlap1,
  na.action = na.fail   # required for dredge()
)

# Run dredge: fits all possible submodels
model_set <- dredge(global_model)

# Show top models (sorted by AICc by default)
head(model_set)

# Extract coefficients of best model(s)
best_model <- get.models(model_set, subset = 1)[[1]]
summary(best_model)

### ----------------------------------------------------------------------
