# Load dependencies and model
source("src/model.R")
source("src/functions.R")
library(ggplot2)
library(reshape2)

# ---------------------------------------------------------------------------
# User-specified fixed parameters for the model run:
evidenceeffect1      <- 0.15  # Fixed evidence effect value
biassedassimilation1 <- 0.9   # Fixed biased assimilation value

# For a lag of 1, we compare naturalvariability[i] with distributions[i+1]
lagParam            <- 1     # Define a user-controllable lag (default = 1)
shiftingbaselines1  <- 1     
replace_high_values <- FALSE # If TRUE, replace values in long_distribution3 above a threshold with NaN
high_threshold      <- 0.95  # Threshold for high values
nRuns               <- 10     # Number of model iterations
temp_0              <- 0     # Initial temperature

# Timeseries plot parameters for fractions (if desired)
ts_plot_opp    <- 0.3   # Example value for frac_opp_01 that triggers time series plotting
ts_plot_neut   <- 0.2   # Example value for frac_neut_01 that triggers time series plotting

# ---------------------------------------------------------------------------
# Create parameter grid for initial population fractions with steps of 0.1
opp_vals  <- seq(0.1, 1, by = 0.1)
neut_vals <- seq(0, 1, by = 0.1)
frac_grid <- expand.grid(frac_opp_01 = opp_vals, frac_neut_01 = neut_vals)
frac_grid <- subset(frac_grid, frac_opp_01 + frac_neut_01 <= 1)

# Pre-allocate matrices to store correlation, regression slope, correlation squared,
# and the mean final distribution values. (Rows correspond to opp_vals and columns to neut_vals)
cor_mat        <- matrix(NA, nrow = length(opp_vals), ncol = length(neut_vals))
reg_mat        <- matrix(NA, nrow = length(opp_vals), ncol = length(neut_vals))
cor_sq_mat     <- matrix(NA, nrow = length(opp_vals), ncol = length(neut_vals))
final_dist_mat <- matrix(NA, nrow = length(opp_vals), ncol = length(neut_vals))

# Nested loops over fraction values (opp and neut)
for (i in seq_along(opp_vals)) {
  cat("Running frac_opp =", opp_vals[i], "\n")
  for (j in seq_along(neut_vals)) {
    # Skip combinations where sum exceeds 1
    if (opp_vals[i] + neut_vals[j] > 1) next
    
    # Set current fraction parameters for the initial population
    frac_opp_01  <- opp_vals[i]
    frac_neut_01 <- neut_vals[j]
    
    # Vectors to store statistics for each model run for this fraction combination
    run_cor_values      <- numeric(nRuns)
    run_slope_values    <- numeric(nRuns)
    run_intercept_values<- numeric(nRuns)
    run_cor_sq_values   <- numeric(nRuns)
    run_final_dist      <- numeric(nRuns)
    
    # To concatenate full time series across runs (if needed for plotting)
    long_naturalvar    <- numeric(0)
    long_distribution3 <- numeric(0)
    
    # Run the model nRuns times
    for (run_idx in 1:nRuns) {
      # Run the model 
      m <- model()  # controlRun = TRUE
      
      # Concatenate full series across runs for potential timeseries plotting
      long_naturalvar    <- c(long_naturalvar,    m$weather)  # m$naturalvariability)
      long_distribution3 <- c(long_distribution3, m$distributions[,3])
      
      natvar_vec <- m$weather  # m$naturalvariability
      distr_vec  <- m$distributions[,3]
      
      # Optionally replace high values with NaN
      if (replace_high_values) {
        distr_vec[distr_vec > high_threshold] <- NaN
      }
      
      # Save the final value of this run
      run_final_dist[run_idx] <- tail(distr_vec, 1)
      
      # Create lagged vectors (handle positive and negative lag)
      end_idx <- length(natvar_vec)
      if (lagParam >= 0) {
        NVLag  <- natvar_vec[1:(end_idx - lagParam)]
        DistLag<- distr_vec[(lagParam + 1):end_idx]
      } else {
        posLag <- abs(lagParam)
        NVLag  <- natvar_vec[(posLag + 1):end_idx]
        DistLag<- distr_vec[1:(end_idx - posLag)]
      }
      
      # Standardize the lagged vectors
      NVLag_std  <- (NVLag - mean(NVLag, na.rm = TRUE)) / sd(NVLag, na.rm = TRUE)
      DistLag_std<- (DistLag - mean(DistLag, na.rm = TRUE)) / sd(DistLag, na.rm = TRUE)
      
      # Compute regression and correlation statistics with custom function
      stats <- compute_regression_stats(NVLag_std, DistLag_std)
      
      run_cor_values[run_idx]    <- stats$correlation
      run_slope_values[run_idx]  <- stats$slope
      run_intercept_values[run_idx]<- stats$intercept
      run_cor_sq_values[run_idx] <- stats$correlation^2
    }  # end nRuns loop
    
    # Average the statistics over all runs for the current fraction combination
    cor_mat[i, j]        <- mean(run_cor_values, na.rm = TRUE)
    reg_mat[i, j]        <- mean(run_slope_values, na.rm = TRUE)
    cor_sq_mat[i, j]     <- mean(run_cor_sq_values, na.rm = TRUE)
    final_dist_mat[i, j] <- mean(run_final_dist, na.rm = TRUE)
    
    # (Optional) If desired, plot the time series for specific fraction conditions:
    if (round(frac_opp_01, 2) == ts_plot_opp && round(frac_neut_01, 2) == ts_plot_neut) {
      print("Plotting time series...")
      # Create a time axis (one value per observation, starting 2020)
      time_full <- seq(2020, by = 1, length.out = length(long_naturalvar))
      # Standardize full time series for plotting
      NV_std   <- (long_naturalvar - mean(long_naturalvar, na.rm = TRUE)) / sd(long_naturalvar, na.rm = TRUE)
      Dist_std <- (long_distribution3 - mean(long_distribution3, na.rm = TRUE)) / sd(long_distribution3, na.rm = TRUE)
      
      # Build a data frame for the standardized series
      ts_df <- data.frame(
        Year = time_full,
        NaturalVariability = NV_std,
        Distribution3 = Dist_std
      )
      
      ts_plot <- ggplot(ts_df, aes(x = Year)) +
        geom_line(aes(y = NaturalVariability, color = "Natural Variability")) +
        geom_line(aes(y = Distribution3, color = "Distribution Column 3")) +
        labs(title = paste0("Standardized Time Series Plot\n(Fixed: EvidenceEffect = ", evidenceeffect1,
                            ", BiassedAssimilation = ", biassedassimilation1, 
                            " | Varying Init Fractions: Opp = ", frac_opp_01,
                            ", Neut = ", frac_neut_01, ", Lag = ", lagParam,
                            ", nRuns = ", nRuns, ""),
             y = "Standardized Value") +
        theme_minimal() +
        theme(legend.title = element_blank(), aspect.ratio = 0.5)
      
      print(ts_plot)
      
      outfile_ts <- paste0("../results/ts_plot-NatVar_climateSupport-lag", lagParam,
                           "_opp", frac_opp_01, "_neut", frac_neut_01,
                           "_nRuns", nRuns, "_Evidence", evidenceeffect1,
                           "_Bias", biassedassimilation1, ".png")
      ggsave(filename = outfile_ts, plot = ts_plot, width = 10, height = 5)
    }
  }  # end inner loop
}  # end outer loop

# -----------------------------#
#         Plot Results         #
# -----------------------------#

# Create data frames for plotting from the matrices
# Correlation DataFrame
cor_df <- melt(cor_mat, varnames = c("i", "j"), value.name = "Correlation")
cor_df$FracOpp  <- opp_vals[cor_df$i]
cor_df$FracNeut <- neut_vals[cor_df$j]

# Final distribution values DataFrame
final_dist_df <- melt(final_dist_mat, varnames = c("i", "j"), value.name = "FinalValue")
final_dist_df$FracOpp  <- opp_vals[final_dist_df$i]
final_dist_df$FracNeut <- neut_vals[final_dist_df$j]

# Plot the correlation heatmap.
fig <- ggplot(cor_df, aes(x = FracOpp, y = FracNeut, fill = Correlation)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0, limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.2),
    labels = function(x) sprintf("%.1f", x),
    oob = scales::oob_squish,
    guide = guide_colorbar(barwidth = 1.5, barheight = 15, title.position = "top")
  ) +
  # scale_x_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1), expand = c(0, 0)) +
  # scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1), expand = c(0, 0)) +
  labs(title = paste0("Correlation: m$weather vs m$distributions[,3]\n",  # m$naturalvariability vs
                      "Varying Init Fractions (Opp & Neut) - Lag ", lagParam,
                      "\nFixed: EvidenceEffect = ", evidenceeffect1, ", BiassedAssimilation = ", biassedassimilation1,
                      ", nRuns = ", nRuns, ""),
       x = "Fraction Opposing", y = "Fraction Neutral") +
  theme_minimal() +
  geom_text(data = final_dist_df, 
            mapping = aes(x = FracOpp, y = FracNeut, label = sprintf("%.2f", FinalValue)),
            color = "black", size = 4, inherit.aes = FALSE)

outfile_corr <- paste0("../results/corr-weather_climateSupport-lag", lagParam,
                       "_oppRange", round(min(opp_vals),2), "to", round(max(opp_vals),2),
                       "_neutRange", round(min(neut_vals),2), "to", round(max(neut_vals),2),
                        "_Evidence", evidenceeffect1,
                        "_Bias", biassedassimilation1, "_nRuns", nRuns, ".png"
                      )
ggsave(outfile_corr, plot = fig, width = 8, height = 6)

# Save output CSV files with filenames noting the range of initial fractions
outfile_cor_csv   <- paste0("../results/outputCSVfiles/cor_mat_lag", lagParam,
                            "_oppRange", round(min(opp_vals),2), "to", round(max(opp_vals),2),
                            "_neutRange", round(min(neut_vals),2), "to", round(max(neut_vals),2),
                            "_nRuns", nRuns, ".csv")
outfile_reg_csv   <- paste0("../results/outputCSVfiles/reg_mat_lag", lagParam,
                            "_oppRange", round(min(opp_vals),2), "to", round(max(opp_vals),2),
                            "_neutRange", round(min(neut_vals),2), "to", round(max(neut_vals),2),
                            "_nRuns", nRuns, ".csv")
outfile_final_csv <- paste0("../results/outputCSVfiles/final_dist_mat_lag", lagParam,
                            "_oppRange", round(min(opp_vals),2), "to", round(max(opp_vals),2),
                            "_neutRange", round(min(neut_vals),2), "to", round(max(neut_vals),2),
                            "_nRuns", nRuns, ".csv")

write.csv(cor_mat, file = outfile_cor_csv, row.names = FALSE)
write.csv(reg_mat, file = outfile_reg_csv, row.names = FALSE)
write.csv(final_dist_mat, file = outfile_final_csv, row.names = FALSE)