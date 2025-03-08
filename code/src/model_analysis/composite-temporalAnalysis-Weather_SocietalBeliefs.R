# Add this after the existing analysis code

library(tidyr)
library(dplyr)
library(ggplot2)

# Load dependencies and model
setwd("/Users/jglarson/Documents/Research/social-climate-model/code")
source("src/model.R")
source("src/functions.R")

# Parameter grids
# evidence_vals <- c(0.05, 0.15, 0.25)  # Evidence effect values
# bias_vals     <- c(0.1, 0.5, 0.9)     # Biased assimilation values
evidence_vals <- c(0.15)  # Evidence effect values
bias_vals     <- c(0.1)     # Biased assimilation values

# For a lag of 1, we compare naturalvariability[i] with distributions[i+1]
lagParam            <- 1     # Define a user-controllable lag (default = 1)
shiftingbaselines1  <- 1     
nRuns               <- 5     # Number of model iterations
frac_opp_01         <- 0.4   # Fraction of population opposing climate policy at t=0
frac_neut_01        <- 0.2   # Fraction of population neutral at t=0
temp_0              <- 0     # Initial temperature (°C) in 2020 = 1.21 °C


# Create time series plots showing averages around sequence endpoints
for (i in seq_along(evidence_vals)) {
  for (j in seq_along(bias_vals)) {
    
    # Set global parameters (as before)
    evidenceeffect1 <- evidence_vals[i]
    biassedassimilation1 <- bias_vals[j]
    
    # For each sequence length
    for (seq_length in 2:6) {
      # Initialize storage for accumulated windows across all runs
      all_weather_windows <- matrix(nrow=0, ncol=21)
      all_anomaly_windows <- matrix(nrow=0, ncol=21)
      all_dist_windows <- matrix(nrow=0, ncol=21)
      total_valid_windows <- 0
      
      # Process each run individually
      for (run_idx in 1:nRuns) {
        # Create model for this run
        m <- model()
        
        # Create lagged vectors for this specific run
        end_idx <- length(m$weather)
        if (lagParam >= 0) {
          weatherLag <- m$weather[1:(end_idx - lagParam)]
          anomLag <- m$anomaly[1:(end_idx - lagParam)]
          DistLag <- m$distributions[(lagParam + 1):end_idx, 3]
        } else {
          posLag <- abs(lagParam)
          weatherLag <- m$weather[(posLag + 1):end_idx]
          anomLag <- m$anomaly[(posLag + 1):end_idx]
          DistLag <- m$distributions[1:(end_idx - posLag), 3]
        }
        
        # Data frame for this run
        run_data <- data.frame(
          weatherLag = weatherLag,
          anomLag = anomLag,
          DistLag = DistLag
        )
        
        # Find sequence endpoints for this run
        sequence_indices <- identify_anomaly_sequences(anomLag, seq_length)
        true_positions <- which(sequence_indices)
        
        if (length(true_positions) == 0) {
          next  # No sequences found in this run
        }
        
        # Extract windows for each sequence endpoint in this run
        window_size <- 21  # 10 before + 1 + 10 after
        for (pos in true_positions) {
          # Calculate window indices
          start_idx <- pos - 10
          end_idx <- pos + 10
          
          # Skip if window extends outside data bounds
          if (start_idx < 1 || end_idx > nrow(run_data)) {
            next
          }
          
          # Add this window to our matrices
          all_weather_windows <- rbind(all_weather_windows, run_data$weatherLag[start_idx:end_idx])
          all_anomaly_windows <- rbind(all_anomaly_windows, run_data$anomLag[start_idx:end_idx])
          all_dist_windows <- rbind(all_dist_windows, run_data$DistLag[start_idx:end_idx])
          
          total_valid_windows <- total_valid_windows + 1
        }
      }  # End of run loop
      
      # Check if we found any valid windows across all runs
      if (total_valid_windows == 0) {
        print(paste("No valid windows for sequences of length", seq_length, 
                    "for evidence =", evidence_vals[i], "and bias =", bias_vals[j]))
        next
      }
      
      # Calculate means for each relative position
      avg_weather <- colMeans(all_weather_windows, na.rm=TRUE)
      avg_anomaly <- colMeans(all_anomaly_windows, na.rm=TRUE)
      avg_dist <- colMeans(all_dist_windows, na.rm=TRUE)
      
      valid_windows <- total_valid_windows
      
      # Create data frame for plotting
      plot_df <- data.frame(
        relative_pos = -10:10,
        weatherLag = avg_weather,
        anomLag = avg_anomaly,
        DistLag = avg_dist
      )
      
      # Create weather vs distribution time series plot
      weather_ts_plot <- ggplot(plot_df, aes(x=relative_pos)) +
        geom_line(aes(y=weatherLag, color="Weather"), size=1) +
        geom_line(aes(y=DistLag, color="Climate Policy Support"), size=1) +
        scale_color_manual(values=c("Weather"="#023743", "Climate Policy Support"="#72874E")) +
        labs(title=paste0("Avg Weather and Climate Support Around Anomaly Length ", seq_length,
                          "\n(EvidenceEffect = ", evidence_vals[i], 
                          ", BiasedAssimilation = ", bias_vals[j], ")"),
             subtitle=paste0("Based on ", valid_windows, " windows"),
             x="Relative Position (0 = sequence endpoint)",
             y="Value",
             color="Variable") +
        theme_minimal() +
        theme(legend.position="bottom")
      
      print(weather_ts_plot)
      
      # Save the weather plot
      outfile_weather_ts <- paste0("../results/composites/",
                                   "weather_timeseries_seq", seq_length,
                                   "_Evidence", evidence_vals[i], 
                                   "_Bias", bias_vals[j], ".png")
      ggsave(filename=outfile_weather_ts, plot=weather_ts_plot, width=8, height=6)
      
      # Create anomaly vs distribution time series plot
      anomaly_ts_plot <- ggplot(plot_df, aes(x=relative_pos)) +
        geom_line(aes(y=anomLag, color="Perceived Anomaly"), size=1) +
        geom_line(aes(y=DistLag, color="Climate Policy Support"), size=1) +
        scale_color_manual(values=c("Perceived Anomaly"="#023743", "Climate Policy Support"="#72874E")) +
        labs(title=paste0("Avg Anomaly and Climate Support Around Anomaly Length ", seq_length,
                          "\n(EvidenceEffect = ", evidence_vals[i], 
                          ", BiasedAssimilation = ", bias_vals[j], ")"),
             subtitle=paste0("Based on ", valid_windows, " windows"),
             x="Relative Position (0 = sequence endpoint)",
             y="Value",
             color="Variable") +
        theme_minimal() +
        theme(legend.position="bottom")
      
      print(anomaly_ts_plot)
      
      # Save the anomaly plot
      outfile_anomaly_ts <- paste0("../results/composites/",
                                   "anomaly_timeseries_seq", seq_length,
                                   "_Evidence", evidence_vals[i], 
                                   "_Bias", bias_vals[j], ".png")
      ggsave(filename=outfile_anomaly_ts, plot=anomaly_ts_plot, width=8, height=6)
    }
  }
}