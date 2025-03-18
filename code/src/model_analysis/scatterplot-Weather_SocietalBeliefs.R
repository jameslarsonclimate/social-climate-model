# This script runs model.R multiple times while varying evidenceeffect1 and biassedassimilation1 in nested loops.
# For each (evidenceeffect1, biassedassimilation1) pair:
#   1) Runs the model 100 times and concatenates outputs to form a long time series
#   2) Calculates correlation and linear regression between m$naturalvariability and m$distributions[,3]
#   3) Stores correlation and regression slope in 2D matrices
# Finally, plots 2D heatmaps of correlation and regression coefficients.

# Load dependencies and model
setwd("/Users/jglarson/Documents/Research/social-climate-model/code")
source("src/model.R")
source("src/functions.R")
library(ggplot2)

# Parameter grids
# evidence_vals <- c(0.05, 0.15, 0.25)  # Evidence effect values
# bias_vals     <- c(0.1, 0.5, 0.9)     # Biased assimilation values
evidence_vals <- c(0.1)  # Evidence effect values
bias_vals     <- c(0.1)     # Biased assimilation values

# For a lag of 1, we compare naturalvariability[i] with distributions[i+1]
lagParam            <- 1     # Define a user-controllable lag (default = 1)
shiftingbaselines1  <- 1     
nRuns               <- 100    # Number of model iterations
frac_opp_01         <- 0.5   # Fraction of population opposing climate policy at t=0
frac_neut_01        <- 0.3   # Fraction of population neutral at t=0
temp_0              <- 0     # Initial temperature (°C) in 2020 = 1.21 °C
ts_plot_evidence    <- 0.24  # Set the evidence effect value for timeseries plotting
ts_plot_bias        <- 0.4   # Set the biased assimilation value for timeseries plotting


# Nested loops over evidence_vals and bias_vals
for (i in seq_along(evidence_vals)) {
  print(paste("Running evidence effect", evidence_vals[i]))
  for (j in seq_along(bias_vals)) {
    
    # Vectors to store statistics for each model iteration
    run_cor_values   <- numeric(nRuns)
    run_slope_values <- numeric(nRuns)
    run_intercept_values <- numeric(nRuns)
    run_cor_sq_values    <- numeric(nRuns)
    run_final_dist      <- numeric(nRuns)   # To store final value of m$distributions[,3] for each run

    # Set global parameters (assumes these are read in from parameters.R within model.R)
    evidenceeffect1       <- evidence_vals[i]
    biassedassimilation1  <- bias_vals[j]
    
    # Concatenate outputs across {} model runs
    long_weather    <- numeric(0)   # Will store extended naturalvariability
    long_distribution3 <- numeric(0)   # Will store extended distribution for index [ ,3 ]
    long_anomaly       <- numeric(0)   # Will store extended perceived anomaly
    
    # Initialize vectors to store lagged data
    all_weatherLag <- numeric(0)
    all_anomLag <- numeric(0)
    all_DistLag <- numeric(0)
    years <- numeric(0)

    # Run the model {} times
    for (run_idx in 1:nRuns) {
      # Run the model
      m <- model()  # controlRun=TRUE) # noNatVar=TRUE)  # natvar = TRUE, historical = TRUE)
      
      # Get this iteration's time series
      weather_vec <- m$weather
      anomaly_vec <- m$anomaly
      distr_vec <- m$distributions[,3]
      
      # Create lagged vectors for this individual run
      end_idx <- length(weather_vec)
      if (lagParam >= 0) {
      run_weatherLag <- weather_vec[1:(end_idx - lagParam)]
      run_anomLag <- anomaly_vec[1:(end_idx - lagParam)]
      run_DistLag <- distr_vec[(lagParam + 1):end_idx]
      # Add the corresponding years from m$year
      run_years <- m$year[1:(end_idx - lagParam)]

      } else {
      # For negative lag
      posLag <- abs(lagParam)
      run_weatherLag <- weather_vec[(posLag + 1):end_idx]
      run_anomLag <- anomaly_vec[(posLag + 1):end_idx]
      run_DistLag <- distr_vec[1:(end_idx - posLag)]
      # Add the corresponding years from m$year
      run_years <- m$year[(posLag + 1):end_idx]

      }
      
      # Concatenate lagged vectors from this run to the accumulated lagged vectors
      all_weatherLag <- c(all_weatherLag, run_weatherLag)
      all_anomLag <- c(all_anomLag, run_anomLag)
      all_DistLag <- c(all_DistLag, run_DistLag)
      years <- c(years, run_years)

      # Save the final value of this run
      run_final_dist[run_idx] <- tail(distr_vec, 1)
    }
    
    # Use the accumulated lagged vectors for analysis and plotting
    weatherLag <- all_weatherLag
    anomLag <- all_anomLag
    DistLag <- all_DistLag
    
    # Standardize the lagged vectors (subtract mean, divide by standard deviation)
    # weatherLag_std  <- (weatherLag - mean(weatherLag, na.rm = TRUE)) / sd(weatherLag, na.rm = TRUE)
    # DistLag_std <- (DistLag - mean(DistLag, na.rm = TRUE)) / sd(DistLag, na.rm = TRUE)      
    # anomLag_std <- (anomLag - mean(anomLag, na.rm = TRUE)) / sd(anomLag, na.rm = TRUE)      

    # Plot the weather and distribution time series on a scatterplot
    scatter_df <- data.frame(weatherLag = weatherLag, DistLag = DistLag, Year = years)
    scatter_plot <- ggplot(scatter_df, aes(x = weatherLag, y = DistLag, color = Year)) +
      geom_point(alpha = 0.7) +
      scale_color_viridis_c(option = "plasma") +
      labs(title = paste0("Scatterplot of Weather vs Frac of Climate Policy Supporters\n(EvidenceEffect = ", 
      evidence_vals[i], ", BiasedAssimilation = ", bias_vals[j], 
      ", frac_opp_01 = ", frac_opp_01, ", frac_neut_01 = ", frac_neut_01,
      ",\nLag = ", lagParam, ", nRuns = ", nRuns, ")"),
      x = "Weather [K]",
      y = "Fraction of climate policy supporters") +
      xlim(-1, 5.5) +
      ylim(-0.1, 1.1) +
      theme_minimal()
    print(scatter_plot)
    outfile_scatter <- paste0("../results/scatterplots/scatterplot-WeatherLag_vs_DistLag-lag", lagParam,
        "_opp", frac_opp_01, "_neut", frac_neut_01, "_nRuns", nRuns, 
        "_Evidence", evidence_vals[i], "_Bias", bias_vals[j], ".png")
    ggsave(filename = outfile_scatter, plot = scatter_plot, width = 8, height = 6)

    # # Plot the anomaly and distribution time series on a scatterplot (standardized)
    # scatter_df <- data.frame(anomLag_std = anomLag_std, DistLag_std = DistLag_std)
    # scatter_plot <- ggplot(scatter_df, aes(x = anomLag_std, y = DistLag_std)) +
    #   geom_point(color = "#023743", alpha = 0.3) +
    #   labs(title = paste0("Scatterplot of Perceived Anomaly vs Frac of Climate Policy Supporters - Standardized\n(EvidenceEffect = ", 
    #     evidence_vals[i], ", BiasedAssimilation = ", bias_vals[j],
    #     ", frac_opp_01 = ", frac_opp_01, ", frac_neut_01 = ", frac_neut_01,
    #     ",\nLag = ", lagParam, ", nRuns = ", nRuns, ")"),
    #   x = "Standardized Perceived Anomaly",
    #   y = "Standardized Distribution Column 3 (DistLag_std)") +
    #   xlim(-3.5, 3.5) +
    #   ylim(-3.5, 3.5) +
    #   theme_minimal()
    # print(scatter_plot)
    # outfile_scatter <- paste0("../results/scatterplots/scatterplot-AnomLag_vs_DistLag-standardized-lag", lagParam,
    #       "_opp", frac_opp_01, "_neut", frac_neut_01, "_nRuns", nRuns, 
    #       "_Evidence", evidence_vals[i], "_Bias", bias_vals[j], ".png")
    # ggsave(filename = outfile_scatter, plot = scatter_plot, width = 8, height = 6)

  }
}

