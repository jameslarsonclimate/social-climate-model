# ---- Plot distribution of net-zero years for all runs and selected bins ----

library(ggplot2)

# Compute all net-zero years (already done above)
# all_zero_years <- apply(ems_mat, 1, function(x) { ... })

# Remove NAs for plotting
all_zero_years_nonNA <- all_zero_years[!is.na(all_zero_years)]

# ---- User selection: choose bins to overlay ----
# Example: select bins by (duration, magnitude) pairs
# User can edit this list to pick which bins to plot
selected_bins <- list(
  list(duration = 5,  magnitude = 0.2),
  list(duration = 10, magnitude = 0.4),
  list(duration = 15, magnitude = 0.6)
)

# Build a data.frame for plotting
plot_data <- data.frame(
  year = all_zero_years_nonNA,
  group = "All Runs"
)

# Store median emissions zero year for each group
median_emissions_zero_years <- data.frame(
  group = character(),
  zero_year = integer()
)

# All Runs: median emissions timeseries and zero year
med_emissions_all <- apply(ems_mat, 2, median, na.rm = TRUE)
zz_all <- which(med_emissions_all <= 0)
zero_year_all_median_emis <- if (length(zz_all) > 0) years[min(zz_all)] else NA_integer_
median_emissions_zero_years <- rbind(
  median_emissions_zero_years,
  data.frame(group = "All Runs", zero_year = zero_year_all_median_emis)
)

# For each selected bin, extract net-zero years and median emissions zero year
for (bin in selected_bins) {
  message(sprintf("Processing bin: Duration = %d, Magnitude = %.2f", bin$duration, bin$magnitude))
  dur <- bin$duration
  mag <- bin$magnitude
  bin_row <- dt_bin[duration == dur & abs(magnitude - mag) < 1e-6]
  if (nrow(bin_row) == 0) next
  end_year <- start_year + dur - 1
  idx_range <- which(years >= start_year & years <= end_year)
  std_nat   <- apply(natvar_mat[, idx_range, drop=FALSE], 1, sd, na.rm=TRUE)
  lo <- mag - half_width
  hi <- mag + half_width
  idx <- which(std_nat >= lo & std_nat < hi)
  if (length(idx) > 0) {
    bin_zero_years <- apply(ems_mat[idx, , drop=FALSE], 1, function(x) {
      zz <- which(x <= 0)
      if (length(zz) > 0) years[min(zz)] else NA_integer_
    })
    bin_zero_years <- bin_zero_years[!is.na(bin_zero_years)]
    plot_data <- rbind(
      plot_data,
      data.frame(
        year = bin_zero_years,
        group = paste0("Duration=", dur, ", SD=", mag)
      )
    )
    # Median emissions timeseries and zero year for this bin
    med_emis_bin <- apply(ems_mat[idx, , drop=FALSE], 2, median, na.rm = TRUE)
    zz_bin <- which(med_emis_bin <= 0)
    zero_year_bin_median_emis <- if (length(zz_bin) > 0) years[min(zz_bin)] else NA_integer_
    median_emissions_zero_years <- rbind(
      median_emissions_zero_years,
      data.frame(group = paste0("Duration=", dur, ", SD=", mag), zero_year = zero_year_bin_median_emis)
    )
  }
}

# ---- Plot distributions ----

p <- ggplot(plot_data, aes(x = year, color = group, fill = group)) +
  geom_density(alpha = 0.5) +
  # Add black, wider line for "All Runs" median
  geom_vline(
    data = subset(median_emissions_zero_years, group == "All Runs"),
    aes(xintercept = zero_year),
    color = "black",
    linetype = "dashed",
    size = 2
  ) +
  # Add colored lines for other groups
  geom_vline(
    data = subset(median_emissions_zero_years, group != "All Runs"),
    aes(xintercept = zero_year, color = group),
    linetype = "dashed",
    size = 1.2
  ) +
  labs(
    title = "Distribution of Net-Zero Year: All Runs and Selected Bins",
    x = "Year Net-Zero",
    y = "Density",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14)

# Save plot to file
output_dir <- "../results/heatmaps/"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output_file <- file.path(output_dir, "netzero_year_distribution_composite_heatmap_medianEmis.png")
ggsave(output_file, plot = p, width = 8, height = 5, dpi = 300)
