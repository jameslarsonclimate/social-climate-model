# This script extends the original analysis by examining how anomalies of varying lengths 
# influence societal change (m$distributions[,3])
# It creates separate scatterplots for anomalies of lengths 1-6 time steps.

# Load dependencies and model
setwd("/Users/jglarson/Documents/Research/social-climate-model/code")
source("src/model.R")
source("src/functions.R")
library(ggplot2)
library(dplyr)

# Parameter grids
# evidence_vals <- c(0.05, 0.15, 0.25)  # Evidence effect values
# bias_vals     <- c(0.1, 0.5, 0.9)     # Biased assimilation values
evidence_vals <- c(0.05)  # Evidence effect values
bias_vals     <- c(0.1)     # Biased assimilation values

# For a lag of 1, we compare naturalvariability[i] with distributions[i+1]
lagParam            <- 1     # Define a user-controllable lag (default = 1)
shiftingbaselines1  <- 1     
nRuns               <- 200    # Number of model iterations
frac_opp_01         <- 0.5   # Fraction of population opposing climate policy at t=0
frac_neut_01        <- 0.3   # Fraction of population neutral at t=0
temp_0              <- 0     # Initial temperature (°C) in 2020 = 1.21 °C

# # Function to identify sequences of consecutive positive/negative anomalies of a specific length
# identify_anomaly_sequences <- function(anomaly_vector, seq_length) {
#   # Initialize result vector
#   result <- rep(FALSE, length(anomaly_vector))
  
#   # For each position in the vector (except the last seq_length-1 positions)
#   for (i in 1:(length(anomaly_vector) - seq_length + 1)) {
#     # Get the sequence starting at position i
#     sequence <- anomaly_vector[i:(i + seq_length - 1)]
    
#     # Check if all values in the sequence have the same sign
#     # (either all positive or all negative)
#     if (all(sequence > 0) || all(sequence < 0)) {
#       # Mark the last position in this sequence
#       result[i + seq_length - 1] <- TRUE
#     }
#   }
  
#   return(result)
# }

# Function to identify exactly seq_length consecutive positive/negative anomalies
identify_anomaly_sequences <- function(anomaly_vector, seq_length) {
  # Initialize result vector
  result <- rep(FALSE, length(anomaly_vector))
  
  # We need a vector with length at least seq_length
  if (length(anomaly_vector) < seq_length) return(result)
  
  # Count consecutive values of the same sign
  count <- 1
  sign_value <- sign(anomaly_vector[1])
  
  # Loop through the vector starting from the second value
  for (i in 2:length(anomaly_vector)) {
    current_sign <- sign(anomaly_vector[i])
    
    # If the sign is the same and non-zero, increase counter
    if (current_sign == sign_value && current_sign != 0) {
      count <- count + 1
      
      # Mark only exactly the seq_length-th consecutive value
      if (count == seq_length) {
        result[i] <- TRUE
      }
    } else {
      # Reset counter and update sign when sequence breaks
      count <- 1
      sign_value <- current_sign
    }
  }
  
  return(result)
}

# Nested loops over evidence_vals and bias_vals
for (i in seq_along(evidence_vals)) {
  print(paste("Running evidence effect", evidence_vals[i]))
  for (j in seq_along(bias_vals)) {
    
    # Set global parameters
    evidenceeffect1       <- evidence_vals[i]
    biassedassimilation1  <- bias_vals[j]
    
    # Concatenate outputs across model runs
    long_weather       <- numeric(0)
    long_distribution3 <- numeric(0)
    long_anomaly       <- numeric(0)
    
    # Run the model multiple times
    for (run_idx in 1:nRuns) {
      m <- model()
      long_weather       <- c(long_weather, m$weather)
      long_anomaly       <- c(long_anomaly, m$anomaly)
      long_distribution3 <- c(long_distribution3, m$distributions[,3])
    }
    
    # Create lagged vectors with support for both positive and negative lags
    end_idx <- length(long_weather)
    if (lagParam >= 0) {
      weatherLag  <- long_weather[1:(end_idx - lagParam)]
      anomLag     <- long_anomaly[1:(end_idx - lagParam)]
      DistLag     <- long_distribution3[(lagParam + 1):end_idx]
    } else {
      # For negative lag, shift in the opposite direction
      posLag      <- abs(lagParam)
      weatherLag  <- long_weather[(posLag + 1):end_idx]
      anomLag     <- long_anomaly[(posLag + 1):end_idx]
      DistLag     <- long_distribution3[1:(end_idx - posLag)]
    }
    
    # Create a data frame for all variables
    all_data <- data.frame(
      weatherLag = weatherLag,
      anomLag = anomLag,
      DistLag = DistLag
    )
    
    # For anomaly lengths 1 to 6, analyze and create scatterplots
    for (seq_length in 2:6) {
      # Skip the first seq_length-1 rows, as they can't be endpoints of sequences
      valid_indices <- (seq_length):length(anomLag)
      
      # Create subset of data for identifying sequences
      anomaly_subset <- anomLag[valid_indices]
      
      # For lengths 2-6, find consecutive sequences of same sign
      sequence_indices <- identify_anomaly_sequences(
        anomLag[(valid_indices[1] - seq_length + 1):tail(valid_indices, 1)], 
        seq_length
      )
      
      # Create a subset of data for this sequence length
      # We add seq_length-1 to match the original indices
      if (seq_length > 1) {
        sequence_data <- all_data[valid_indices[sequence_indices], ]
      } else {
        sequence_data <- all_data[valid_indices, ]
      }
      
      # Skip the plot if no data points found for this sequence length
      if (nrow(sequence_data) == 0) {
        print(paste("No sequences of length", seq_length, "found for evidence =", 
                    evidence_vals[i], "and bias =", bias_vals[j]))
        next
      }
      
      # Plot weather vs distribution for this sequence length
      weather_plot <- ggplot(sequence_data, aes(x = weatherLag, y = DistLag)) +
        geom_point(color = "#023743", alpha = 0.7) +
        labs(title = paste0("Weather vs Climate Policy Support - Anomaly Length ", seq_length, 
                "\n(EvidenceEffect = ", evidence_vals[i], 
                ", BiasedAssimilation = ", bias_vals[j],
                ", nRuns = ", nRuns, ")"),
         subtitle = paste0("Number of data points: ", nrow(sequence_data)),
         x = "Weather",
         y = "Distribution Column 3 (Climate Policy Support)") +
        xlim(-1, 5.5) +
        ylim(-0.1, 1.1) +
        theme_minimal()
      
      print(weather_plot)
      
      # Save the plot
      outfile_weather <- paste0("../results/scatterplots/anomaly_length_analysis/",
               "weather_vs_support-",
               "_Evidence", evidence_vals[i], 
               "_Bias", bias_vals[j],
               "_anomalyLength_", seq_length, 
                "_nRuns", nRuns, ".png")
      ggsave(filename = outfile_weather, plot = weather_plot, width = 8, height = 6)
      
      # Plot anomaly vs distribution for this sequence length
      anomaly_plot <- ggplot(sequence_data, aes(x = anomLag, y = DistLag)) +
        geom_point(color = "#023743", alpha = 0.7) +
        labs(title = paste0("Perceived Anomaly vs Climate Policy Support - Anomaly Length ", seq_length, 
               "\n(EvidenceEffect = ", evidence_vals[i], 
               ", BiasedAssimilation = ", bias_vals[j],
               ", nRuns = ", nRuns, ")"),
         subtitle = paste0("Number of data points: ", nrow(sequence_data)),
         x = "Perceived Anomaly",
         y = "Distribution Column 3 (Climate Policy Support)") +
        xlim(-2, 2) +
        ylim(-0.1, 1.1) +
        theme_minimal()
      
      print(anomaly_plot)
      
      # Save the plot
      outfile_anomaly <- paste0("../results/scatterplots/anomaly_length_analysis/",
               "anomaly_vs_support-",
               "_Evidence", evidence_vals[i], 
               "_Bias", bias_vals[j],
               "_anomalyLength_", seq_length, 
                "_nRuns", nRuns, ".png")
      ggsave(filename = outfile_anomaly, plot = anomaly_plot, width = 8, height = 6)
      
      # Calculate and report statistical relationship
      model_fit <- lm(DistLag ~ anomLag, data = sequence_data)
      correlation <- cor(sequence_data$anomLag, sequence_data$DistLag, use = "complete.obs")
      
      print(paste("Anomaly Length", seq_length, "- Evidence:", evidence_vals[i], 
                  "Bias:", bias_vals[j]))
      print(paste("Correlation:", round(correlation, 4)))
      print(paste("Regression slope:", round(coef(model_fit)[2], 4)))
      print(paste("Number of data points:", nrow(sequence_data)))
      print("----------------------------")
    }
  }
}

# ****************************************************************
# TO DO: Make the code below operational
# ****************************************************************

# Create a timeseries plot to verify identify_anomaly_sequences for a given sequence length
seq_length_to_verify <- 3  # Change this as desired (e.g., 2, 3, 4, 5, or 6)

# Identify valid endpoints for anomaly sequences in the entire anomLag vector
anomaly_flag <- identify_anomaly_sequences(all_data$anomLag, seq_length_to_verify)

# Add a time index and flag to all_data
all_data$time <- 1:nrow(all_data)
all_data$sequence_flag <- ifelse(anomaly_flag, "Sequence Endpoint", "Normal")

# Create the ggplot timeseries plot as a line plot
p <- ggplot(all_data, aes(x = time, y = anomLag)) +
  geom_line(color = "gray60") +
  geom_point(aes(color = sequence_flag), size = 3) +
  labs(title = paste("Perceived Anomaly Timeseries with Seq Length", seq_length_to_verify),
    x = "Time Index", y = "Perceived Anomaly") +
  theme_minimal() +
  theme(aspect.ratio = 0.5) # Makes width twice the height

print(p)
