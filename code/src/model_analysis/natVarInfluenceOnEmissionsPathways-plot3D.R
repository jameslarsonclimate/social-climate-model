plot_emissions_3d_histogram <- function(ems_data, n_sample = NULL, n_bins = 100, years = 2020:2100) {
  
  # [existing code remains the same until the creation of z_matrix]
  
  # Compute the log-transformed frequency values.
  # Adding a small constant avoids issues with log(0)
  log_z_matrix <- log(z_matrix + 1e-6)
  
  # Create the 3D surface plot using plotly
  fig <- plot_ly() %>%
  add_surface(x = ~years,
        y = ~bin_centers,
        z = ~log_z_matrix,
        colorscale = "Viridis",
        contours = list(
          z = list(show = TRUE, 
               usecolormap = TRUE, 
               highlightcolor = "#ff0000", 
               project = list(z = TRUE))
        )) %>%
  layout(title = paste0("3D Histogram of Emission Pathways (", ncol(ems_matrix), " years, ", nrow(ems_matrix), " trajectories)"),
       scene = list(
       xaxis = list(title = "Year"),
       yaxis = list(title = "Emissions (GtC per year)"),
       zaxis = list(title = "log(Frequency)")
       ))
  
  return(fig)
}


#### 3D plot of emissions

# Create a 3D density visualization of emission pathways over time
library(ggplot2)
library(viridis)
library(data.table)
library(reshape2)

# Function to create a 3D heatmap of emission pathways
plot_emissions_3d_density <- function(ems_data, n_sample = NULL, 
                                      n_bins = 100, 
                                      years = 2020:2100) {
  
  # Convert data if needed
  if (inherits(ems_data, "data.table") || inherits(ems_data, "data.frame")) {
    ems_matrix <- as.matrix(ems_data)
  } else {
    ems_matrix <- ems_data
  }
  
  # Sample if requested
  if (!is.null(n_sample) && n_sample < nrow(ems_matrix)) {
    set.seed(123)
    sampled_rows <- sample(1:nrow(ems_matrix), n_sample)
    ems_matrix <- ems_matrix[sampled_rows, ]
    cat("Sampled", n_sample, "trajectories from the full dataset\n")
  }
  
  # Reshape the data to long format
  df_long <- data.frame(
    year = rep(years, each = nrow(ems_matrix)),
    emission = as.vector(t(ems_matrix)),
    trajectory = rep(1:nrow(ems_matrix), times = ncol(ems_matrix))
  )
  
  # Find the range of emissions for binning
  emission_min <- min(df_long$emission, na.rm = TRUE)
  emission_max <- max(df_long$emission, na.rm = TRUE)
  
  # Calculate 2D histogram
  year_bins <- sort(unique(df_long$year))
  emission_bins <- seq(emission_min, emission_max, length.out = n_bins)
  
  # Create 2D histogram
  hist_2d <- ggplot(df_long, aes(x = year, y = emission)) +
    stat_density_2d(aes(fill = after_stat(density)), 
                   geom = "tile", 
                   contour = FALSE,
                   n = n_bins) +
    scale_fill_viridis(option = "plasma", name = "Density") +
    labs(
      x = "Year",
      y = "Emissions (GtC per year)",
      title = "3D Density of Emission Pathways (2020-2100)",
      subtitle = paste0(nrow(ems_matrix), " simulated pathways")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right"
    )
  
  # Create a contour version
  contour_plot <- ggplot(df_long, aes(x = year, y = emission)) +
    stat_density_2d(aes(color = after_stat(level)), 
                   geom = "contour",
                   n = 15,
                   linewidth = 0.5) +
    scale_color_viridis(option = "plasma", name = "Density") +
    labs(
      x = "Year",
      y = "Emissions (GtC per year)",
      title = "Contour Map of Emission Pathway Density",
      subtitle = paste0(nrow(ems_matrix), " simulated pathways")
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right"
    )
  
  return(list(heatmap = hist_2d, contour = contour_plot))
}

# Usage:
# Read in the data (assuming it's already loaded as 'ems')
# plots <- plot_emissions_3d_density(ems, n_sample = 100000) 
plots <- plot_emissions_3d_density(ems, n_sample=10000, n_bins = 150)

# Save the plots
ggsave("../results/emissions_3d_density_heatmap.jpg", plots$heatmap, width = 10, height = 8)
ggsave("../results/emissions_3d_density_contour.jpg", plots$contour, width = 10, height = 8)

# Display the heatmap
print(plots$heatmap)

