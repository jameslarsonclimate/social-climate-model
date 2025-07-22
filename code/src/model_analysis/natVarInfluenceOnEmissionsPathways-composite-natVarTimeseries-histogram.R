library(data.table)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

setwd('~/Documents/Research/social-climate-model/code')
data_dir <- "../results/MC Runs/MC Runs_TunedParams/"
years <- 2020:2100
start_year <- 2025

# ---- User input: choose duration and magnitude ----
plot_dur <- 5      # duration (years)
plot_mag <- -0.5   # bin center for magnitude
half_width <- 0.05

# Datasets to compare
datasets <- list(
  list(label = "Model", suffix = "_initClimSupportNormalDistribution"),
  list(label = "CESMx1", suffix = "_CESM_HR_local_natVar_multiplier1"),
  # list(label = "CESMx0.5", suffix = "_CESM_HR_local_natVar_multiplier05"),
  list(label = "ERA5", suffix = "_ERA5natVar")
)

hist_data_list <- list()

for (ds in datasets) {
  label <- ds$label
  suffix <- ds$suffix
  message("Loading: ", data_dir, "natvar", suffix, ".csv")
  natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar", suffix, ".csv")))

  # Find runs in the selected bin
  end_year <- start_year + plot_dur - 1
  idx_range <- which(years >= start_year & years <= end_year)
  avg_nat <- rowMeans(natvar_mat[, idx_range, drop=FALSE], na.rm=TRUE)
  lo <- plot_mag - half_width
  hi <- plot_mag + half_width
  idx <- which(avg_nat >= lo & avg_nat < hi)

  if (length(idx) > 0) {
    # Gather all natvar values for all runs in the bin
    all_natvar <- as.numeric(natvar_mat[idx, , drop=FALSE])
    hist_data_list[[length(hist_data_list) + 1]] <- data.table(
      natvar = all_natvar,
      dataset = label
    )
  }
}

df_hist <- rbindlist(hist_data_list)

# Set common x and y axis limits
xlims <- range(df_hist$natvar, na.rm=TRUE)
# Compute max y for all datasets' histograms
ymax <- max(sapply(split(df_hist, df_hist$dataset), function(d) {
  max(hist(d$natvar, plot=FALSE, breaks=30)$counts)
}), na.rm=TRUE)*0.7

# Plot histograms for each dataset
plots <- lapply(unique(df_hist$dataset), function(label) {
  ggplot(df_hist[dataset == label], aes(x = natvar, fill = dataset)) +
    geom_histogram(bins = 30, color = "black", alpha = 0.7) +
    scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Set1")) +
    labs(
      title = label,
      x = "Natural Variability (degC)",
      y = "Count"
    ) +
    xlim(xlims) +
    ylim(0, ymax) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
})

# Combine plots in a single row
combined_plot <- wrap_plots(plots, ncol = length(plots)) +
  plot_annotation(
    title = paste0("Histogram of Natural Variability Values in Bin\nDuration = ", plot_dur, ", Magnitude = ", plot_mag),
    theme = theme(plot.title = element_text(size = 16, hjust = 0.5))
  )

# Save plot
out_dir <- "../results/heatmaps"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(out_dir, paste0("hist_natvar_bin_dur", plot_dur, "_mag", plot_mag, ".png"))
ggsave(out_file, combined_plot, width=12, height=5)

