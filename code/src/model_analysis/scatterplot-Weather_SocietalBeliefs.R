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
evidence_vals <- c(0.05, 0.15, 0.25)  # Evidence effect values
bias_vals     <- c(0.1, 0.5, 0.9)     # Biased assimilation values

# For a lag of 1, we compare naturalvariability[i] with distributions[i+1]
lagParam            <- 1     # Define a user-controllable lag (default = 1)
shiftingbaselines1  <- 1     
nRuns               <- 20    # Number of model iterations
frac_opp_01         <- 0.4   # Fraction of population opposing climate policy at t=0
frac_neut_01        <- 0.2   # Fraction of population neutral at t=0
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
    
    # Run the model {} times
    for (run_idx in 1:nRuns) {
      # *Remember, if changing controlRun or others, change titles in plots and save filenames*
      m <- model()  # controlRun=TRUE) # noNatVar=TRUE)  # natvar = TRUE, historical = TRUE)
      long_weather       <- c(long_weather,       m$weather) # m$naturalvariability), m$temp[,1]
      long_anomaly       <- c(long_anomaly,       m$anomaly) # m$naturalvariability), m$temp[,1]
      long_distribution3 <- c(long_distribution3, m$distributions[,3])

      # Get this iteration's time series
      natvar_vec <- m$weather # m$naturalvariability, m$temp[,1]
      distr_vec  <- m$distributions[,3]

      # Save the final value of this run (using the optionally replaced distr_vec)
      run_final_dist[run_idx] <- tail(distr_vec, 1)

      # # # Save the average value of climate supporter frac of this run (using the optionally replaced distr_vec)
      # run_final_dist[run_idx] <- mean(distr_vec, na.rm = TRUE)
    }

    # Create lagged vectors with support for both positive and negative lags:
    end_idx <- length(long_weather)
    if (lagParam >= 0) {
      weatherLag  <- long_weather[1:(end_idx - lagParam)]
      anomLag  <- long_anomaly[1:(end_idx - lagParam)]
      DistLag <- long_distribution3[(lagParam + 1):end_idx]
    } else {
      # For negative lag, shift in the opposite direction.
      posLag <- abs(lagParam)
      weatherLag  <- long_weather[(posLag + 1):end_idx]
      anomLag  <- long_anomaly[(posLag + 1):end_idx]
      DistLag <- long_distribution3[1:(end_idx - posLag)]
    }
    
    # Standardize the lagged vectors (subtract mean, divide by standard deviation)
    weatherLag_std  <- (weatherLag - mean(weatherLag, na.rm = TRUE)) / sd(weatherLag, na.rm = TRUE)
    DistLag_std <- (DistLag - mean(DistLag, na.rm = TRUE)) / sd(DistLag, na.rm = TRUE)      
    anomLag_std <- (anomLag - mean(anomLag, na.rm = TRUE)) / sd(anomLag, na.rm = TRUE)      

    # Plot the weather and distribution time series on a scatterplot
    scatter_df <- data.frame(weatherLag = weatherLag, DistLag = DistLag)
    scatter_plot <- ggplot(scatter_df, aes(x = weatherLag, y = DistLag)) +
      geom_point(color = "#023743", alpha = 0.7) +
      labs(title = paste0("Scatterplot of Weather vs Frac of Climate Policy Supporters\n(EvidenceEffect = ", 
        evidence_vals[i], ", BiasedAssimilation = ", bias_vals[j], 
        ", frac_opp_01 = ", frac_opp_01, ", frac_neut_01 = ", frac_neut_01,
        ",\nLag = ", lagParam, ", nRuns = ", nRuns, ")"),
      x = "Weather",
      y = "Distribution Column 3 (DistLag)") +
      xlim(-1, 5.5) +
      ylim(-0.1, 1.1) +
      theme_minimal()
    print(scatter_plot)
    outfile_scatter <- paste0("../results/scatterplots/scatterplot-WeatherLag_vs_DistLag-lag", lagParam,
          "_opp", frac_opp_01, "_neut", frac_neut_01, "_nRuns", nRuns, 
          "_Evidence", evidence_vals[i], "_Bias", bias_vals[j], ".png")
    ggsave(filename = outfile_scatter, plot = scatter_plot, width = 8, height = 6)

    # Plot the weather and distribution time series on a scatterplot (standardized)
    scatter_df <- data.frame(weatherLag_std = weatherLag_std, DistLag_std = DistLag_std)
    scatter_plot <- ggplot(scatter_df, aes(x = weatherLag_std, y = DistLag_std)) +
      geom_point(color = "#023743", alpha = 0.7) +
      labs(title = paste0("Scatterplot of Weather vs Frac of Climate Policy Supporters - Standardized\n(EvidenceEffect = ", 
        evidence_vals[i], ", BiasedAssimilation = ", bias_vals[j],
        ", frac_opp_01 = ", frac_opp_01, ", frac_neut_01 = ", frac_neut_01,
        "\nLag = ", lagParam, ", nRuns = ", nRuns, ")"),
      x = "Standardized Weather",
      y = "Standardized Distribution Column 3 (DistLag_std)") +
      xlim(-3.5, 3.5) +
      ylim(-3.5, 3.5) +
      theme_minimal()
    print(scatter_plot)
    outfile_scatter <- paste0("../results/scatterplots/scatterplot-weatherLag_vs_DistLag-standardized-lag", lagParam,
          "_opp", frac_opp_01, "_neut", frac_neut_01, "_nRuns", nRuns, 
          "_Evidence", evidence_vals[i], "_Bias", bias_vals[j], ".png")
    ggsave(filename = outfile_scatter, plot = scatter_plot, width = 8, height = 6)

    # Plot the anomaly and distribution time series on a scatterplot
    scatter_df <- data.frame(anomLag = anomLag, DistLag = DistLag)
    scatter_plot <- ggplot(scatter_df, aes(x = anomLag, y = DistLag)) +
      geom_point(color = "#023743", alpha = 0.7) +
      labs(title = paste0("Scatterplot of Perceived Anomaly vs Frac of Climate Policy Supporters\n(EvidenceEffect = ", 
        evidence_vals[i], ", BiasedAssimilation = ", bias_vals[j],
        ", frac_opp_01 = ", frac_opp_01, ", frac_neut_01 = ", frac_neut_01,
        ",\nLag = ", lagParam, ", nRuns = ", nRuns, ")"),
      x = "Perceived Anomaly",
      y = "Distribution Column 3 (DistLag)") +
      xlim(-2, 2) +
      ylim(-0.1, 1.1) +
      theme_minimal()
    print(scatter_plot)
    outfile_scatter <- paste0("../results/scatterplots/scatterplot-AnomLag_vs_DistLag-lag", lagParam,
          "_opp", frac_opp_01, "_neut", frac_neut_01, "_nRuns", nRuns, 
          "_Evidence", evidence_vals[i], "_Bias", bias_vals[j], ".png")
    ggsave(filename = outfile_scatter, plot = scatter_plot, width = 8, height = 6)

    # Plot the anomaly and distribution time series on a scatterplot (standardized)
    scatter_df <- data.frame(anomLag_std = anomLag_std, DistLag_std = DistLag_std)
    scatter_plot <- ggplot(scatter_df, aes(x = anomLag_std, y = DistLag_std)) +
      geom_point(color = "#023743", alpha = 0.7) +
      labs(title = paste0("Scatterplot of Perceived Anomaly vs Frac of Climate Policy Supporters - Standardized\n(EvidenceEffect = ", 
        evidence_vals[i], ", BiasedAssimilation = ", bias_vals[j],
        ", frac_opp_01 = ", frac_opp_01, ", frac_neut_01 = ", frac_neut_01,
        ",\nLag = ", lagParam, ", nRuns = ", nRuns, ")"),
      x = "Standardized Perceived Anomaly",
      y = "Standardized Distribution Column 3 (DistLag_std)") +
      xlim(-3.5, 3.5) +
      ylim(-3.5, 3.5) +
      theme_minimal()
    print(scatter_plot)
    outfile_scatter <- paste0("../results/scatterplots/scatterplot-AnomLag_vs_DistLag-standardized-lag", lagParam,
          "_opp", frac_opp_01, "_neut", frac_neut_01, "_nRuns", nRuns, 
          "_Evidence", evidence_vals[i], "_Bias", bias_vals[j], ".png")
    ggsave(filename = outfile_scatter, plot = scatter_plot, width = 8, height = 6)

  }
}

