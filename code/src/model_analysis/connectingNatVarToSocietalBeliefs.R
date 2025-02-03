# This script runs model.R multiple times while varying evidenceeffect1 and biassedassimilation1 in nested loops.
# For each (evidenceeffect1, biassedassimilation1) pair:
#   1) Runs the model 100 times and concatenates outputs to form a long time series
#   2) Calculates correlation and linear regression between m$naturalvariability and m$distributions[,3]
#   3) Stores correlation and regression slope in 2D matrices
# Finally, plots 2D heatmaps of correlation and regression coefficients.

# Load dependencies and model
source("src/model.R")
source("src/functions.R")
library(ggplot2)

# Parameter grids
evidence_vals <- seq(0, 0.3, by = 0.03)       # Evidence effect values
bias_vals     <- seq(0, 1,   by = 0.1)        # Biased assimilation values

# Define a user-controllable lag (default = 1)
# For a lag of 1, we compare naturalvariability[i] with distributions[i+1]
lagParam <- 0
shiftingbaselines1 = 1
replace_high_values <- FALSE       # If TRUE, replace values in long_distribution3 above a threshold with NaN
high_threshold <- 0.95            # Threshold above which values will be replaced with NaN
nRuns <- 10  # Number of model iterations

# Matrices to store correlation, regression slope, and correlation squared
cor_mat     <- matrix(NA, nrow = length(evidence_vals), ncol = length(bias_vals))
reg_mat     <- matrix(NA, nrow = length(evidence_vals), ncol = length(bias_vals))
cor_sq_mat  <- matrix(NA, nrow = length(evidence_vals), ncol = length(bias_vals))  # New matrix for correlation squared

# Nested loops over evidence_vals and bias_vals
for (i in seq_along(evidence_vals)) {
  print(paste("Running evidence effect", evidence_vals[i]))
  for (j in seq_along(bias_vals)) {
    
    # Vectors to store statistics for each model iteration
    run_cor_values   <- numeric(nRuns)
    run_slope_values <- numeric(nRuns)
    run_intercept_values <- numeric(nRuns)
    run_cor_sq_values    <- numeric(nRuns)

    # Set global parameters (assumes these are read in from parameters.R within model.R)
    evidenceeffect1       <- evidence_vals[i]
    biassedassimilation1  <- bias_vals[j]
    
    # Concatenate outputs across {} model runs
    long_naturalvar    <- numeric(0)   # Will store extended naturalvariability
    long_distribution3 <- numeric(0)   # Will store extended distribution for index [ ,3 ]
    
    # Run the model {} times
    for (run_idx in 1:nRuns) {
      m <- model()  # natvar = TRUE, historical = TRUE)
      long_naturalvar    <- c(long_naturalvar,    m$naturalvariability)
      long_distribution3 <- c(long_distribution3, m$distributions[,3])
    }
    
    # Optionally replace values greater than high_threshold with NaN
    if (replace_high_values) {
      long_distribution3[long_distribution3 > high_threshold] <- NaN
    }

    # Create lagged vectors:
    # # For a lag of 1, we compare naturalvariability[i] with distributions[i+1]
    # # so we slice accordingly to avoid out-of-bounds
    end_idx <- length(long_naturalvar)
    NVLag   <- long_naturalvar[1:(end_idx - lagParam)]
    DistLag <- long_distribution3[(lagParam + 1):end_idx]
    # end_idx <- length(long_naturalvar)
    # if (lagParam >= 0) {
    #   # For lagParam >= 0:
    #   # Compare naturalvariability[i] with distributions[i+lagParam]
    #   NVLag  <- long_naturalvar[1:(end_idx - lagParam)]
    #   DistLag <- long_distribution3[(lagParam + 1):end_idx]
    # } else {
    #   # For lagParam < 0:
    #   # Let posLag be the absolute lag value.
    #   posLag <- abs(lagParam)
    #   # Compare naturalvariability[i+posLag] with distributions[i]
    #   NVLag  <- long_naturalvar[(posLag + 1):end_idx]
    #   DistLag <- long_distribution3[1:(end_idx - posLag)]
    # }
    
    # Standardize the lagged vectors: subtract mean then divide by standard deviation
    NVLag_std  <- (NVLag - mean(NVLag, na.rm = TRUE)) / sd(NVLag, na.rm = TRUE)
    DistLag_std <- (DistLag - mean(DistLag, na.rm = TRUE)) / sd(DistLag, na.rm = TRUE)

    # Compute regression and correlation statistics using the function from functions.R
    stats <- compute_regression_stats(NVLag_std, DistLag_std)
    
    # Extract needed statistics
    cor_val    <- stats$correlation
    reg_coef   <- stats$slope
    intercept  <- stats$intercept
    cor_sq_val <- cor_val^2

    # Store results
    cor_mat[i, j]    <- cor_val
    reg_mat[i, j]    <- reg_coef
    cor_sq_mat[i, j] <- cor_sq_val  

    # If certain conditions are met, plot the time series along with the regression fit line
    if (evidence_vals[i] == 0.15 && bias_vals[j] == 0.5) {
      # Create a time axis (one value per observation, starting 2020)
      time_full <- seq(2020, by = 1, length.out = length(long_naturalvar))
      
      # For the regression fit line, use the lagged time axis corresponding to NVLag
      time_lag <- time_full[1:(end_idx - lagParam)]
      predictedDist <- intercept + reg_coef * NVLag
      
      # Standardize the full time series for plotting
      NV_std  <- (long_naturalvar - mean(long_naturalvar, na.rm = TRUE)) / sd(long_naturalvar, na.rm = TRUE)
      Dist_std <- (long_distribution3 - mean(long_distribution3, na.rm = TRUE)) / sd(long_distribution3, na.rm = TRUE)
      
      # Build a data frame for the standardized raw series (unlagged)
      ts_df <- data.frame(
        Year = time_full,
        NaturalVariability = NV_std,
        Distribution3 = Dist_std
      )
            
      # Plot the timeseries of both standardized variables and overlay the regression fit line
      ts_plot <- ggplot(ts_df, aes(x = Year)) +
        geom_line(aes(y = NaturalVariability, color = "Natural Variability")) +
        geom_line(aes(y = Distribution3, color = "Distribution Column 3")) +
        labs(title = paste0("Standardized Time Series Plot (EvidenceEffect = ", evidence_vals[i],
                            ", BiasedAssimilation = ", bias_vals[j], ")"),
            y = "Standardized Value") +
        theme_minimal() +
        theme(legend.title = element_blank(), aspect.ratio = 0.5)
      
      print(ts_plot)
    }
  }
}

#-----------------#
#  Plot Results   #
#-----------------#

# Turn matrices into data frames for ggplot
# Each row in cor_df/reg_df: (evidenceeffect, biassedassimilation, value)
library(reshape2)

# Correlation DataFrame
cor_df <- melt(cor_mat, varnames = c("i", "j"), value.name = "Correlation")
cor_df$EvidenceEffect <- evidence_vals[cor_df$i]
cor_df$BiasedAssim    <- bias_vals[cor_df$j]

# Regression DataFrame
reg_df <- melt(reg_mat, varnames = c("i", "j"), value.name = "RegressionSlope")
reg_df$EvidenceEffect <- evidence_vals[reg_df$i]
reg_df$BiasedAssim    <- bias_vals[reg_df$j]

# Correlation Squared DataFrame
cor_sq_df <- melt(cor_sq_mat, varnames = c("i", "j"), value.name = "CorrelationSquared")
cor_sq_df$EvidenceEffect <- evidence_vals[cor_sq_df$i]
cor_sq_df$BiasedAssim    <- bias_vals[cor_sq_df$j]

# Plot the correlation heatmap
fig = ggplot(cor_df, aes(x = EvidenceEffect, y = BiasedAssim, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-0.65, 0.65)) +
    labs(title = paste0("Correlation: m$naturalvariability vs m$distributions[,3] - Lag ", lagParam),
        x = "Evidence Effect",
        y = "Biased Assimilation") +
    theme_minimal()

ggsave(paste0("../results/corr-NatVar_climateSupport-lag", lagParam, ".png"), plot=fig, width=8, height=6)

# Plot the correlation squared heatmap
fig <- ggplot(cor_sq_df, aes(x = EvidenceEffect, y = BiasedAssim, fill = CorrelationSquared)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0.5, limits = c(-0.45, 0.45)) +  # Adjust midpoint as needed
  labs(title = paste0("Correlation Squared: m$naturalvariability vs m$distributions[,3] - Lag ", lagParam),
       x = "Evidence Effect",
       y = "Biased Assimilation") +
  theme_minimal()

ggsave(paste0("../results/corrSq-NatVar_climateSupport-lag", lagParam, ".png"), plot = fig, width = 8, height = 6)

# Plot the regression slope heatmap
fig = ggplot(reg_df, aes(x = EvidenceEffect, y = BiasedAssim, fill = RegressionSlope)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limits = c(-0.65, 0.65)) +
    labs(title = paste0("Regression Slope: m$distributions[,3] ~ m$naturalvariability - lag ", lagParam),
        x = "Evidence Effect",
        y = "Biased Assimilation") +
    theme_minimal()

ggsave(paste0("../results/reg-NatVar_climateSupport-lag", lagParam, ".png"), plot=fig, width=8, height=6)

# Construct a data frame for plotting
df_timeseries <- data.frame(
  Year = seq(2020, by = 1, length.out = length(m$naturalvariability)),
  NatVarZ = (m$naturalvariability - mean(m$naturalvariability)) / sd(m$naturalvariability),
  Dist3Z  = (m$distributions[,3] - mean(m$distributions[,3])) / sd(m$distributions[,3])
)

# Example dimensions for a plot twice as wide as tall in an interactive environment:
options(repr.plot.width = 10, repr.plot.height = 5)

# Plot
ggplot(df_timeseries, aes(x = Year)) +
  geom_line(aes(y = NatVarZ, color = "Natural Variability (std)")) +
  geom_line(aes(y = Dist3Z,  color = "Distributions[,3] (std)")) +
  labs(
    title = "Standardized Natural Variability vs. Distributions[,3]",
    x = "Year",
    y = "Standardized Value"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(aspect.ratio = 1/2)

