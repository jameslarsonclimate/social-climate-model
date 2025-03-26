# Clean script to run the social climate model and create a plot

# Setup
setwd('~/Documents/Research/social-climate-model/code')
library(ggplot2)

# Load the model
source("src/model.R")

# Set model parameters
frac_opp_01 <- 0.5        # Fraction opposing climate policy at t=0
frac_neut_01 <- 0.3       # Fraction neutral at t=0
evidenceeffect1 <- 0.15    # Strength of evidence effect
biassedassimilation1 <- 0.1  # Strength of biased assimilation
shiftingbaselines1 <- 1   # Whether shifting baselines are active

# Create a timeseries with a triangular pulse from index 10 to 20
# Initialize a vector of 81 zeros and define the peak value
ts <- numeric(81)
peak <- 2

# Create ascending values from index 10 to 15 and descending values from index 16 to 20
ts[11:15] <- seq(0, peak, length.out = 6)
ts[16:21] <- seq(peak - peak/5, 0, length.out = 5)
  
# Number of runs in ensemble
num_runs <- 100

# Initialize a list to store the results of each run
results_list <- vector("list", num_runs)

# Run the model multiple times and store each result
for (i in 1:num_runs) {
  # Create a temperature anomaly timeseries (optional: can add noise to make each run slightly different)
  # ts <- create_pulse_timeseries()
  
  # Run the model and store the result
  results_list[[i]] <- model()  # model(temperature_anomaly = ts)
  
  # Print progress
  if (i %% 10 == 0) print(paste("Completed run", i, "of", num_runs))
}

# Initialize a structure for averaged results using the first run's structure
avg_results <- results_list[[1]]

# Set all numeric elements to zero
for (name in names(avg_results)) {
  if (is.numeric(avg_results[[name]])) {
    avg_results[[name]] <- rep(0, length(avg_results[[name]]))
  } else if (is.matrix(avg_results[[name]])) {
    avg_results[[name]] <- matrix(0, nrow = nrow(avg_results[[name]]), ncol = ncol(avg_results[[name]]))
  }
}

# Sum up the results from each run
for (i in 1:num_runs) {
  for (name in names(avg_results)) {
    if (is.numeric(avg_results[[name]])) {
      avg_results[[name]] <- avg_results[[name]] + results_list[[i]][[name]]
    } else if (is.matrix(avg_results[[name]])) {
      avg_results[[name]] <- avg_results[[name]] + results_list[[i]][[name]]
    }
  }
}

# Calculate the average by dividing by number of runs
for (name in names(avg_results)) {
  if (is.numeric(avg_results[[name]]) || is.matrix(avg_results[[name]])) {
    avg_results[[name]] <- avg_results[[name]] / num_runs
  }
}

# Use the averaged results for plotting
m <- avg_results

# Create a scaling coefficient for the secondary axis
coeff <- 0.05

# Create a data frame with model outputs
data <- data.frame(
  time = m$year,
  emissions = m$emissions,
  total_emissions = m$totalemissions,
  temperature = m$temp[,1],
  weather = m$weather,
  evidence = m$evidence[,1],
  anomaly = m$anomaly,
  opposed = m$distributions[,1],
  neutral = m$distributions[,2],
  support = m$distributions[,3]
)

# Find the years that correspond to pulse start and end (indices 10 and 20)
pulse_start_year <- m$year[10]
pulse_end_year <- m$year[20]

# Create a plot of key variables
fig <- ggplot(data, aes(x = time)) +
  # Add vertical gray lines at pulse start and end (bottom z-order)
  # geom_vline(xintercept = c(pulse_start_year, pulse_end_year), color = "black", alpha = 0.7) +

  # Primary y-axis
  geom_line(aes(y = total_emissions, color = "Total Emissions"), linewidth = 0.9) +
  geom_line(aes(y = weather, color = "Weather"), linewidth = 0.9) +
  geom_line(aes(y = evidence, color = "Evidence"), linewidth = 0.9) +
  
  # Secondary y-axis (population distributions)
  geom_line(aes(y = opposed/coeff, color = "Opposed"), linewidth = 0.9) +
  geom_line(aes(y = neutral/coeff, color = "Neutral"), linewidth = 0.9) +
  geom_line(aes(y = support/coeff, color = "Support"), linewidth = 0.9) +
  
  # Set up axes
  scale_y_continuous(
    name = "Emissions and Temperature",
    limits = c(-2, 25), 
    sec.axis = sec_axis(~. * coeff, name = "Population Distributions")
  ) +
  
  # Labels and title
  labs(
    x = "Year",
    y = "Model Output",
    title = "Social Climate Model Output - Ensemble Average",
    subtitle = paste("Ensemble size =", num_runs, 
                     ", Evidence Effect =", evidenceeffect1, 
                     ", Biased Assimilation =", biassedassimilation1,
                     ",\nInitial Opposition =", frac_opp_01,
                     ", Initial Neutral =", frac_neut_01)
  ) +
  
  # Color scheme
  scale_color_manual(
    values = c(
      "Total Emissions" = "#5f0f40",    # Dark purple
      "Weather" = "#ffba08",            # Gold
      "Evidence" = "#caf0f8",           # Light blue
      "Opposed" = "#ee4a70",            # Red-pink
      "Neutral" = "#8d99ae",            # Gray
      "Support" = "#06d667"             # Green
    )
  ) +
  
  # Theme customization
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Print and save the plot
print(fig)
ggsave("../results/ensemble_model_run.png", plot = fig, width = 8, height = 6)