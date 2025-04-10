library(ggplot2)
library(plot.matrix)
library(data.table)
library(tidyverse)
library(reshape2)
library(patchwork)
library(forcats)
library(EnvStats)
library(randomForest)
library(randomForestExplainer)

setwd("/Users/jglarson/Documents/Research/social-climate-model/code")
source("src/model_analysis/model_parametertune.R")

fig_suffix = '_pulseTempAnom_2K_2030-2040'
# fig_suffix = ''

# Create a timeseries with a triangular pulse from index 10 to 20
# Initialize a vector of 81 zeros and define the peak value
ts <- numeric(81)
peak <- 2

# Create ascending values from index 10 to 15 and descending values from index 16 to 20
ts[10:15] <- seq(0, peak, length.out = 6)
ts[16:20] <- seq(peak - peak/5, 0, length.out = 5)


# ####------kmeans clustering of tuned output---------

params=fread(paste0("../results/MC Runs/MC Runs_TunedParams/params", fig_suffix, ".csv"))
pol=fread(paste0("../results/MC Runs/MC Runs_TunedParams/policy", fig_suffix, ".csv"))

emissions_file = paste0("../results/MC Runs/MC Runs_TunedParams/emissions", fig_suffix, ".csv")
print(paste("Reading emissions file:", emissions_file))
ems=fread(emissions_file)

mc=dim(params)[1]


# Modified plotting function that adds a density plot of final values
plot_emission_trajectories <- function(ems_data, alpha = 0.001, n_sample = NULL, 
       show_summary = TRUE, years = 2020:2100,
       title_suffix = '') {
  # Convert data if it's data.table or data.frame
  if (inherits(ems_data, "data.table") || inherits(ems_data, "data.frame")) {
  ems_matrix <- as.matrix(ems_data)
  } else {
  ems_matrix <- ems_data
  }
  
  # Sample a subset of trajectories if requested
  if (!is.null(n_sample) && n_sample < nrow(ems_matrix)) {
  sampled_rows <- sample(1:nrow(ems_matrix), n_sample)
  ems_matrix <- ems_matrix[sampled_rows, ]
  cat("Sampled", n_sample, "trajectories from the full dataset\n")
  }
  
  # Convert matrix to long format for plotting
  df_long <- data.frame(
  run = rep(1:nrow(ems_matrix), each = length(years)),
  year = rep(years, nrow(ems_matrix)),
  emission = as.vector(t(ems_matrix))
  )
  
  # Calculate summary statistics if requested
  if (show_summary) {
  summary_df <- data.frame(
  year = years,
  median = apply(ems_matrix, 2, median),
  lower = apply(ems_matrix, 2, quantile, probs = 0.1),
  upper = apply(ems_matrix, 2, quantile, probs = 0.9)
  )
  }
  
  # Get final values for the distribution plot
  final_values <- ems_matrix[, ncol(ems_matrix)]
  
  # Get middle values (timestep 40)
  mid_col <- 41 # round(ncol(ems_matrix)/2)
  mid_year <- years[mid_col]
  mid_values <- ems_matrix[, mid_col]
  
  # Create title with suffix if provided
  main_title <- "Global Emissions Trajectories (2020-2100)"
  if (!is.null(title_suffix) && title_suffix != "") {
  main_title <- paste0(main_title, "\n-", title_suffix)
  }
  
  # Find y-axis limits to ensure both plots align
  y_min <- min(df_long$emission, na.rm = TRUE)
  y_max <- max(df_long$emission, na.rm = TRUE)
  
  # Create the time series plot
  p1 <- ggplot() +
  geom_line(data = df_long, aes(x = year, y = emission, group = run), 
    color = "dodgerblue4", alpha = alpha) +
  labs(x = "Year", y = "Emissions (GtC per year)",
   title = main_title,
   subtitle = paste0(nrow(ems_matrix), " simulated pathways")) +
  ylim(y_min, y_max) +
  theme_minimal(base_size = 14) +
  theme(
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid.major = element_line(color = "grey90"), 
  panel.grid.minor = element_blank()
  )
  
  # Add summary statistics if requested
  if (show_summary) {
  p1 <- p1 +
  geom_line(data = summary_df, aes(x = year, y = median),
    color = "#c42449", size = 1.5) +
  geom_line(data = summary_df, aes(x = year, y = lower),
    color = "#476F84", size = 1, linetype = "dashed") +
  geom_line(data = summary_df, aes(x = year, y = upper),
    color = "#476F84", size = 1, linetype = "dashed") +
  annotate("text", x = max(years) - 5, y = summary_df$median[length(years) - 5], 
     label = "Median", color = "#c42449", hjust = 1) +
  annotate("text", x = max(years) - 5, y = summary_df$upper[length(years) - 5], 
     label = "90th percentile", color = "#476F84", hjust = 1) +
  annotate("text", x = max(years) - 5, y = summary_df$lower[length(years) - 5], 
     label = "10th percentile", color = "#476F84", hjust = 1)
  }
  
  # Create the mid-point density plot
  p_mid <- ggplot() +
  geom_density(aes(y = mid_values), fill = "#476F84", color = "#476F84", alpha = 0.6) +
  ylim(y_min, y_max) +
  labs(x = NULL, y = NULL, title = paste0(mid_year)) +
  theme_minimal(base_size = 14) +
  theme(
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.title = element_text(size = 10, hjust = 0.5),
  plot.margin = ggplot2::margin(t = 47, r = 5, b = 5, l = 0, unit = "pt")
  )
  
  # Create the final values density plot
  p2 <- ggplot() +
  geom_density(aes(y = final_values), fill = "#023743", color = "#023743", alpha = 0.6) +
  ylim(y_min, y_max) +
  labs(x = NULL, y = NULL, title = paste0(max(years))) +
  theme_minimal(base_size = 14) +
  theme(
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.title = element_text(size = 10, hjust = 0.5),
  plot.margin = ggplot2::margin(t = 47, r = 5, b = 5, l = 0, unit = "pt")
  )
  
  # Combine the plots
  combined_plot <- p1 + p_mid + p2 + plot_layout(ncol = 3, widths = c(4, 1, 1))
  
  return(combined_plot)
}

# Example usage:

# 2. Plot all trajectories with very low alpha (for a final visualization)
# full_plot <- plot_emission_trajectories(ems, alpha = 0.01, title_suffix = fig_suffix)
# ggsave(paste0("../results/all_emission_trajectories", fig_suffix, ".jpg"), full_plot, width = 10, height = 7)

# 3. For faster preview or interactive exploration, plot a sample
preview_plot <- plot_emission_trajectories(ems, alpha = 0.003, n_sample = 15000, title_suffix = fig_suffix)
ggsave(paste0("../results/emission_trajectories-preview", fig_suffix, ".jpg"), preview_plot, width = 10, height = 7)


# plot_emission_trajectories_filled <- function(ems_data, n_sample = NULL, 
#        show_summary = TRUE, years = 2020:2100, title_suffix = "") {
  
#   # Convert data if it's a data.table or data.frame
#   if (inherits(ems_data, "data.table") || inherits(ems_data, "data.frame")) {
#     ems_matrix <- as.matrix(ems_data)
#   } else {
#     ems_matrix <- ems_data
#   }
  
#   # Sample a subset of trajectories if requested
#   if (!is.null(n_sample) && n_sample < nrow(ems_matrix)) {
#     sampled_rows <- sample(1:nrow(ems_matrix), n_sample)
#     ems_matrix <- ems_matrix[sampled_rows, ]
#     cat("Sampled", n_sample, "trajectories from the full dataset\n")
#   }
  
#   # Convert matrix to long format for plotting
#   df_long <- data.frame(
#     run = rep(1:nrow(ems_matrix), each = length(years)),
#     year = rep(years, nrow(ems_matrix)),
#     emission = as.vector(t(ems_matrix))
#   )
  
#   # Calculate summary statistics if requested
#   if (show_summary) {
#     summary_df <- data.frame(
#       year = years,
#       median = apply(ems_matrix, 2, median),
#       lower  = apply(ems_matrix, 2, quantile, probs = 0.1),
#       upper  = apply(ems_matrix, 2, quantile, probs = 0.9)
#     )
#   }
  
#   # Create main title
#   main_title <- "Global Emissions Trajectories Density (2020-2100)\n"
#   if (!is.null(title_suffix) && title_suffix != "") {
#     main_title <- paste0(main_title, title_suffix)
#   }
  
#   # Find y-axis limits to align plots
#   y_min <- min(df_long$emission, na.rm = TRUE)
#   y_max <- max(df_long$emission, na.rm = TRUE)
  
#   # Create the density-filled contour plot using geom_density_2d_filled
#   p <- ggplot(df_long, aes(x = year, y = emission)) +
#     geom_density_2d_filled(alpha = 0.8) +
#     labs(x = "Year", y = "Emissions (GtC per year)",
#          title = main_title,
#          subtitle = paste0(nrow(ems_matrix), " simulated pathways")) +
#     ylim(y_min, y_max) +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.background = element_rect(fill = "white", color = NA),
#       plot.background  = element_rect(fill = "white", color = NA),
#       panel.grid.major = element_line(color = "grey90"),
#       panel.grid.minor = element_blank()
#     )
  
#   # Optionally overlay summary statistics
#   if (show_summary) {
#     p <- p +
#       geom_line(data = summary_df, aes(x = year, y = median),
#                 color = "#c42449", size = 1.5) +
#       geom_line(data = summary_df, aes(x = year, y = lower),
#                 color = "#476F84", size = 1, linetype = "dashed") +
#       geom_line(data = summary_df, aes(x = year, y = upper),
#                 color = "#476F84", size = 1, linetype = "dashed") +
#       annotate("text", x = max(years) - 5, y = summary_df$median[length(years) - 5],
#                label = "Median", color = "#c42449", hjust = 1) +
#       annotate("text", x = max(years) - 5, y = summary_df$upper[length(years) - 5],
#                label = "90th percentile", color = "#476F84", hjust = 1) +
#       annotate("text", x = max(years) - 5, y = summary_df$lower[length(years) - 5],
#                label = "10th percentile", color = "#476F84", hjust = 1)
#   }
  
#   return(p)
# }

# # Example usage:
# # Assuming 'ems' contains your 100,000 emission trajectories (rows) over 81 years (columns)
# density_filled_plot <- plot_emission_trajectories_filled(ems, n_sample = 10000, title_suffix = "")
# print(density_filled_plot)

# # Save the plot if desired:
# ggsave("../results/emission_density2d_filled.jpg", density_filled_plot, width = 10, height = 7)

