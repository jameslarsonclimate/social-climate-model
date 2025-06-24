rm(list=ls())
# Load necessary libraries
library(rpart)         # For regression trees
library(randomForest)  # For Random Forests
library(ggplot2)       # For plotting
library(caret)         # For data splitting and evaluation
library(reshape2)      # For data manipulation

# Set seed for reproducibility
set.seed(123)

# ----------------------------
# Data Simulation
# ----------------------------

# Parameters for data simulation
n_obs <- 1000     # Number of observations
n_features <- 20  # Number of features
n_relevant <- 5   # Number of truly relevant features

# Simulate predictor variables (features)
X <- matrix(rnorm(n_obs * n_features), nrow = n_obs, ncol = n_features)
colnames(X) <- paste0("X", 1:n_features)

# Simulate true coefficients (only first 'n_relevant' predictors are non-zero)
beta <- c(runif(n_relevant, 1, 3), rep(0, n_features - n_relevant))

# Generate response variable with some noise
epsilon <- rnorm(n_obs, mean = 0, sd = 1)
Y <- X %*% beta + epsilon

################### Turn this off if running a continuous Y ####################
# Make Y a categorical variable. Make unbalanced by altering threshold. 
summary(Y)
threshold <- 13
Y[Y<threshold] <- 0
Y[Y>=threshold] <- 1
sum(Y==1)
# Convert X to a data frame
X_df <- as.data.frame(X)

# Combine X and Y into a single data frame
data <- cbind(X_df, Y = as.vector(Y))

# ----------------------------
# Split Data into Training and Testing Sets
# ----------------------------

# Create training and testing indices
set.seed(123)
train_indices <- sample(1:n_obs, size = floor(0.7 * n_obs))
test_indices <- setdiff(1:n_obs, train_indices)

# Training data
train_data <- data[train_indices, ]

# Testing data
test_data <- data[test_indices, ]

# ----------------------------
# Fit a Regression Tree
# ----------------------------

# Fit the regression tree using rpart
tree_model <- rpart(
  Y ~ .,
  data = train_data,
  method = "anova",         # Regression tree
  control = rpart.control(
    cp = 0.01               # Complexity parameter for pruning
  )
)

# IF CONTINUOUS Y
# Predict on test data
# pred_tree <- predict(tree_model, newdata = test_data)

### IF CATEGORICAL Y 
# Predict on test data
pred_tree <- predict(tree_model, newdata = test_data)
pred_tree[pred_tree<0.5] <- 0
pred_tree[pred_tree>=0.5] <- 1
conf_matrix <- confusionMatrix(as.factor(pred_tree), as.factor(test_data$Y))
print(conf_matrix)
# Calculate Mean Squared Error
mse_tree <- mean((test_data$Y - pred_tree)^2)

# ----------------------------
# Fit Random Forests with Varying Number of Trees
# ----------------------------

# Define a sequence of number of trees to try
num_trees_seq <- seq(1, 200, by = 5)

# Initialize vector to store MSEs
mse_rf <- numeric(length(num_trees_seq))

# Loop over different numbers of trees
for (i in seq_along(num_trees_seq)) {
  ntree <- num_trees_seq[i]
  
  # Fit Random Forest model
  rf_model <- randomForest(
    Y ~ .,
    data = train_data,
    ntree = ntree,          # Number of trees
    mtry = sqrt(n_features),# Number of variables sampled at each split
    importance = TRUE,      # Enable calculation of variable importance
    nodesize = 5            # Minimum size of terminal nodes
  )
  
  # Predict on test data
  pred_rf <- predict(rf_model, newdata = test_data)
  
  # Calculate Mean Squared Error
  mse_rf[i] <- mean((test_data$Y - pred_rf)^2)
}

############## Confusion Matrix for Random Forest ######################
pred_rf[pred_rf<0.5] <- 0
pred_rf[pred_rf>=0.5] <- 1
conf_matrix <- confusionMatrix(as.factor(pred_rf), as.factor(test_data$Y))
print(conf_matrix)

# ----------------------------
# Plot MSE vs. Number of Trees
# ----------------------------

# Combine results into a data frame
mse_df <- data.frame(
  Num_Trees = num_trees_seq,
  MSE = mse_rf
)

# Add MSE from regression tree for comparison
mse_df <- rbind(mse_df, data.frame(Num_Trees = 1, MSE = mse_tree))

# Plot MSE vs. Number of Trees
mse_plot <- ggplot(mse_df, aes(x = Num_Trees, y = MSE)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_hline(yintercept = mse_tree, linetype = "dashed", color = "red") +
  annotate("text", x = max(num_trees_seq) * 0.8, y = mse_tree + 0.5, label = "Single Regression Tree", color = "red") +
  ggtitle("Test MSE vs. Number of Trees in Random Forest") +
  xlab("Number of Trees") +
  ylab("Mean Squared Error (Test Data)") +
  theme_bw()

print(mse_plot)

# ----------------------------
# Fit Final Random Forest Model
# ----------------------------

# Choose the number of trees that gives the lowest MSE
best_ntree <- num_trees_seq[which.min(mse_rf)]
cat("Optimal number of trees:", best_ntree, "\n")

# Fit Random Forest with optimal number of trees
final_rf_model <- randomForest(
  Y ~ .,
  data = train_data,
  ntree = best_ntree,
  mtry = sqrt(n_features),
  importance = TRUE,
  nodesize = 5
)

# ----------------------------
# Feature Importance from Random Forest
# ----------------------------

# Get variable importance
importance_rf <- importance(final_rf_model, type = 1)  # Type 1 for mean decrease in accuracy
importance_df <- data.frame(
  Feature = rownames(importance_rf),
  Importance = importance_rf[, 1]
)

# Sort by importance
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]

# Plot top 10 important features
importance_plot <- ggplot(importance_df[1:10, ], aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  ggtitle("Top 10 Important Features (Random Forest)") +
  xlab("Features") +
  ylab("Importance (Mean Decrease in Accuracy)") +
  theme_bw()

print(importance_plot)
ggsave(paste0("../results/randomForest/test.png"), #, fig_suffix, "_", analysis_label, "_", pct_label, ".png"), 
importance_plot, width=8, height=10)


# ----------------------------
# Explanation 
# ----------------------------

cat("\nExplanation:\n")
cat("1. **Data Simulation**: We simulated a dataset with 1000 observations and 20 features. Only the first 5 features are truly relevant to predicting the target variable Y.\n")
cat("2. **Regression Tree**: We fitted a single regression tree to the data. The dashed red line in the MSE plot represents the MSE of this tree on the test data.\n")
cat("3. **Random Forests with Varying Trees**: We fitted Random Forest models with the number of trees ranging from 1 to 200. As the number of trees increases, the test MSE generally decreases and stabilizes, illustrating how ensemble methods improve performance.\n")
cat("4. **MSE vs. Number of Trees Plot**: The plot shows the test MSE for Random Forests with different numbers of trees. It demonstrates that a Random Forest with more trees tends to perform better than a single tree.\n")
cat("5. **Optimal Number of Trees**: We identified the number of trees that resulted in the lowest test MSE and used it to fit the final Random Forest model.\n")
cat("6. **Feature Importance**: The Random Forest provides measures of feature importance. The importance plot displays the top 10 features contributing most to the model's predictive power. Ideally, the truly relevant features (e.g., X1 to X5) should appear at the top.\n")
cat("7. **Understanding Random Forests**: Random Forests improve upon single trees by reducing variance through averaging multiple trees trained on bootstrapped samples and random subsets of features. This helps prevent overfitting and enhances generalization.\n")
cat("8. **Educational Insight**: This simulation demonstrates the power of ensemble methods like Random Forests in handling larger datasets with many features, and how they can outperform single models by aggregating the predictions of multiple models.\n")
