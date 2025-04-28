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

# fig_suffix = '_pulseTempAnom_2K_2030-2040'
# fig_suffix = '_pulseTempAnom_2K_2070-2080'
# fig_suffix = ''
fig_suffix = '_noNatVar'
# fig_suffix = '_fixedNatVar-lackOfClimateSupport'
# fig_suffix = '_fixedNatVar-mediumClimateSupport'
# fig_suffix = '_fixedNatVar-highClimateSupport'



# ####------kmeans clustering of tuned output---------

params=fread(paste0("../results/MC Runs/MC Runs_TunedParams/params", fig_suffix, ".csv"))
pol=fread(paste0("../results/MC Runs/MC Runs_TunedParams/policy", fig_suffix, ".csv"))

emissions_file = paste0("../results/MC Runs/MC Runs_TunedParams/emissions", fig_suffix, ".csv")
print(paste("Reading emissions file:", emissions_file))
ems=fread(emissions_file)

mc=dim(params)[1]


########## Add a user-defined mask of values to the data ###########################

# mask <- params$Evidence > 0.2
# mask_title = "Evidence>0.2"

mask <- params$Evidence > 0.2 & params$"Shifting Baselines" != 0
mask_title = "Evidence>0.2_and_ShiftingBaselines!=0"

# mask <- params$"Shifting Baselines" == 0
# mask_title = "shiftingBaselines=0"

# mask_title = ''
# rm(mask)


if (exists("mask")) {
  print(paste("Masking values outside", mask_title))
  ems <- ems[mask, , drop = FALSE]
  params <- params[mask, , drop = FALSE]
  pol <- pol[mask, , drop = FALSE]
}




##########################################################################################################
# Plotting function that adds a density plot of final values
##########################################################################################################


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
  
  # Create title with mask title and suffix if provided
  main_title <- "Global Emissions Trajectories (2020-2100)"
  if (!is.null(title_suffix) && title_suffix != "") {
    main_title <- paste0(main_title, " - ", title_suffix)
  }
  main_title <- paste0(main_title, "\nMask: ", mask_title)

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
  # ylim(y_min, y_max) +
  ylim(y_min, 22) +
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
# ggsave(paste0("../results/all_emission_trajectories", fig_suffix, "_", mask_title, ".jpg"), full_plot, width = 10, height = 7)

# 3. For faster preview or interactive exploration, plot a sample
# preview_plot <- plot_emission_trajectories(ems, alpha = 0.003, n_sample = 15000, title_suffix = fig_suffix)
# ggsave(paste0("../results/emission_trajectories-preview", fig_suffix, "_", mask_title, ".jpg"), preview_plot, width = 10, height = 7)




######################################################################################################
# New function: plot density of emissions at each decade (2020, 2030, ..., 2100)
######################################################################################################


plot_decadal_emission_densities <- function(ems_data, years = 2020:2100, title_suffix = '') {
  # Convert to matrix if needed
  if (inherits(ems_data, "data.table") || inherits(ems_data, "data.frame")) {
    ems_matrix <- as.matrix(ems_data)
  } else {
    ems_matrix <- ems_data
  }
  
  # Indices for start of each decade
  decade_years <- seq(2020, 2100, by = 10)
  decade_indices <- match(decade_years, years)
  
  # Gather all values for y-axis limits
  all_vals <- as.vector(ems_matrix[, decade_indices])
  y_min <- min(all_vals, na.rm = TRUE)
  y_max <- max(all_vals, na.rm = TRUE)
  
  # Color palette for decades
  colors <- colorRampPalette(c("#A4BED5", "#476F84", "#023743"))(length(decade_years))
  
  # Define x‐axis maxima for each of the 9 density plots
  x_max_values <- c(1.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1.5, 1.5, 1.5)

  # Create density plots for each decade
  plot_list <- lapply(seq_along(decade_indices), function(i) {
    idx      <- decade_indices[i]
    year     <- decade_years[i]
    vals     <- ems_matrix[, idx]
    mean_val <- mean(vals, na.rm = TRUE)
    p <- ggplot() +
      geom_density(aes(y = vals), fill = colors[i], color = colors[i], alpha = 0.7) +
      ylim(-2, 22) +
      xlim(0, x_max_values[i]) +
      labs(x = NULL, y = NULL, title = as.character(year)) +
      theme_minimal(base_size = 13) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        panel.grid       = element_blank(),
        axis.text.y      = element_blank(),
        axis.ticks.y     = element_blank(),
        plot.title       = element_text(size = 13, hjust = 0.5),
        plot.margin      = ggplot2::margin(t = 30, r = 5, b = 5, l = 5, unit = "pt")
      ) +
      geom_hline(yintercept = mean_val, linetype = "dashed", color = "black", size = 0.5) +
      annotate("text",
               x     = Inf, y = mean_val,
               label = sprintf("Mean: %.1f", mean_val),
               hjust = 1.1, vjust = -0.5, size = 3)
    p
  })
  
  # Combine into a 3x3 grid
  combined_plot <- wrap_plots(plot_list, ncol = 3) +
    plot_annotation(
      title = paste0("Emissions Distribution at Start of Each Decade", 
                     if (title_suffix != '') paste0("\n", title_suffix) else "\n",
                     if (exists("mask_title") && mask_title != "") paste0(" - Mask: ", mask_title) else ""),
      subtitle = paste0(nrow(ems_matrix), " simulated pathways"),
      theme = theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)
      )
    )
  return(combined_plot)
}

# Example usage:
decade_density_plot <- plot_decadal_emission_densities(ems, years = 2020:2100, title_suffix = fig_suffix)
ggsave(paste0("../results/emissions_decade_density", fig_suffix, 
              if (exists("mask_title") && mask_title != "") paste0("_", mask_title) else "", ".png"),
       decade_density_plot, width = 12, height = 9)




######################################################################################################
# New function: plot timeseries + 3 density plots (mid, 2040, final) for 10 sequential subsets
######################################################################################################




plot_emission_trajectories_by_sequential_subsets <- function(
  ems_data, alpha = 0.001, n_sample = NULL, years = 2020:2100, title_suffix = '') {
  # Convert data if it's data.table or data.frame
  if (inherits(ems_data, "data.table") || inherits(ems_data, "data.frame")) {
    ems_matrix <- as.matrix(ems_data)
  } else {
    ems_matrix <- ems_data
  }
  
  # Optionally sample a subset of trajectories
  if (!is.null(n_sample) && n_sample < nrow(ems_matrix)) {
    sampled_rows <- sample(1:nrow(ems_matrix), n_sample)
    ems_matrix <- ems_matrix[sampled_rows, ]
    cat("Sampled", n_sample, "trajectories from the full dataset\n")
  }
  
  n_total <- nrow(ems_matrix)
  n_per_row <- floor(n_total / 10)
  row_indices <- split(1:n_total, ceiling(seq_along(1:n_total) / n_per_row))
  # Ensure exactly 10 rows
  row_indices <- row_indices[1:10]
  
  # Indices for density plots
  mid_col <- 41
  mid_year <- years[mid_col]
  y2040_col <- 21
  y2040_year <- years[y2040_col]
  y2060_col <- 41
  y2060_year <- years[y2060_col]
  y2080_col <- 61
  y2080_year <- years[y2080_col]
  
  # Find global y-axis limits for all plots
  all_vals <- as.vector(ems_matrix)
  y_min <- min(all_vals, na.rm = TRUE)
  y_max <- max(all_vals, na.rm = TRUE)
  
  # Prepare color palette for density plots
  colors <- c("#FED789", "#476F84", "#023743") # 2040, 2060, 2080
  
  # For each subset, create a row of plots
  row_plots <- lapply(1:10, function(i) {
    idxs <- row_indices[[i]]
    subset_matrix <- ems_matrix[idxs, , drop = FALSE]
    
    # Timeseries plot
    df_long <- data.frame(
      run = rep(1:nrow(subset_matrix), each = length(years)),
      year = rep(years, nrow(subset_matrix)),
      emission = as.vector(t(subset_matrix))
    )
    # Calculate summary statistics
    summary_df <- data.frame(
      year = years,
      median = apply(subset_matrix, 2, median),
      lower = apply(subset_matrix, 2, quantile, probs = 0.1),
      upper = apply(subset_matrix, 2, quantile, probs = 0.9)
    )
    p1 <- ggplot() +
      geom_line(data = df_long, aes(x = year, y = emission, group = run),
          color = "dodgerblue4", alpha = alpha) +
      # Add summary statistics
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
         label = "10th percentile", color = "#476F84", hjust = 1) +
      labs(
        x = if (i == 10) "Year" else "",
        y = if (i == 1) "Emissions (GtC per year)" else "",
        title = if (i == 1) paste0("Subset 1 (rows 1-", n_per_row, ")") 
            else paste0("Subset ", i, " (rows ", min(idxs), "-", max(idxs), ")")
      ) +
      ylim(y_min, y_max) +
      theme_minimal(base_size = 11) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        plot.title = element_text(size = 10, hjust = 0.5),
        axis.text.x = if(i == 10) element_text() else element_blank()
      )
    
    # 2040 density plot (now first)
    y2040_values <- subset_matrix[, y2040_col]
    y2040_mean <- mean(y2040_values, na.rm = TRUE)
    p_2040 <- ggplot() +
      geom_density(aes(y = y2040_values), fill = colors[1], color = colors[1], alpha = 0.6) +
      geom_hline(yintercept = y2040_mean, linetype = "dashed", color = "black", size = 0.5) +
      annotate("text", x = Inf, y = y2040_mean, label = sprintf("Mean = %.2f", y2040_mean),
           hjust = 1.1, vjust = -0.5, size = 3) +
      ylim(y_min, y_max) +
      labs(x = NULL, y = NULL, title = as.character(y2040_year)) +
      theme_minimal(base_size = 11) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5),
        axis.text.x = element_blank()
      )
    
    # 2060 density plot (now second)
    mid_values <- subset_matrix[, y2060_col]
    mid_mean <- mean(mid_values, na.rm = TRUE)
    p_mid <- ggplot() +
      geom_density(aes(y = mid_values), fill = colors[2], color = colors[2], alpha = 0.6) +
      geom_hline(yintercept = mid_mean, linetype = "dashed", color = "black", size = 0.5) +
      annotate("text", x = Inf, y = mid_mean, label = sprintf("Mean = %.2f", mid_mean),
           hjust = 1.1, vjust = -0.5, size = 3) +
      ylim(y_min, y_max) +
      labs(x = NULL, y = NULL, title = as.character(y2060_year)) +
      theme_minimal(base_size = 11) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5),
        axis.text.x = element_blank()
      )
    
    # 2080 density plot (was final year, now 2080)
    y2080_values <- subset_matrix[, y2080_col]
    y2080_mean <- mean(y2080_values, na.rm = TRUE)
    p_2080 <- ggplot() +
      geom_density(aes(y = y2080_values), fill = colors[3], color = colors[3], alpha = 0.6) +
      geom_hline(yintercept = y2080_mean, linetype = "dashed", color = "black", size = 0.5) +
      annotate("text", x = Inf, y = y2080_mean, label = sprintf("Mean = %.2f", y2080_mean),
           hjust = 1.1, vjust = -0.5, size = 3) +
      ylim(y_min, y_max) +
      labs(x = NULL, y = NULL, title = as.character(y2080_year)) +
      theme_minimal(base_size = 11) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 9, hjust = 0.5),
        axis.text.x = element_blank()
      )
    
    # Combine horizontally for this row: 2040, 2060, 2080
    p1 + p_2040 + p_mid + p_2080 + plot_layout(ncol = 4, widths = c(4, 1, 1, 1))
  })
  
  # Stack all rows vertically with mask_title in the overall title
  combined_plot <- wrap_plots(row_plots, ncol = 1) +
    plot_annotation(
      title = paste0("Emissions Trajectories and Densities by Sequential Subset\n", 
      title_suffix, " Mask: ", mask_title),
      subtitle = paste0(nrow(ems_matrix), " simulated pathways, 10 sequential subsets"),
      theme = theme(
   plot.title = element_text(size = 16, hjust = 0.5),
   plot.subtitle = element_text(size = 12, hjust = 0.5)
      )
    )
  return(combined_plot)
}

# Example usage:
# subset_density_plot <- plot_emission_trajectories_by_sequential_subsets(
#   ems, alpha = 0.003, years = 2020:2100, title_suffix = fig_suffix)
# ggsave(paste0("../results/emissions_sequential_subset_density", fig_suffix, "_", mask_title, ".png"),
#        subset_density_plot, width = 16, height = 22)
