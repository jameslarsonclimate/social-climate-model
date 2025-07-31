library(data.table)
library(ggplot2)
library(RColorBrewer)

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

plot_list <- list()

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
  idx_bin <- which(avg_nat >= lo & avg_nat < hi)
  n_in_bin <- length(idx_bin)

  if (n_in_bin > 0) {
    # Median timeseries for runs in the bin
    median_natvar_bin <- apply(natvar_mat[idx_bin, , drop=FALSE], 2, median, na.rm=TRUE)
    # Median timeseries for randomly selected runs (same n as in bin)
    set.seed(42)
    idx_rand <- sample(seq_len(nrow(natvar_mat)), n_in_bin)
    median_natvar_rand <- apply(natvar_mat[idx_rand, , drop=FALSE], 2, median, na.rm=TRUE)

    df_plot <- data.table(
      year = rep(years, 2),
      median_natvar = c(median_natvar_bin, median_natvar_rand),
      group = rep(c("Bin", "Random"), each = length(years)),
      dataset = label,
      n_runs = n_in_bin
    )

    plot_list[[length(plot_list) + 1]] <- df_plot
  }
}

df_all <- rbindlist(plot_list)

# Create a label for legend
df_all[, legend_label := paste0(dataset, " (n=", n_runs, ")")]

p <- ggplot(df_all, aes(x = year, y = median_natvar, color = group, linetype = group)) +
  geom_line(size = 1.2) +
  facet_wrap(~ legend_label, ncol = 1) +
  labs(
    title = paste0("Median Natural Variability Timeseries\nDuration = ", plot_dur, ", Magnitude = ", plot_mag),
    x = "Year",
    y = "Median Natural Variability (degC)",
    color = "Group",
    linetype = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Save plot
out_dir <- "../results/heatmaps"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(out_dir, paste0("median_natvar_bin_vs_random_dur", plot_dur, "_mag", plot_mag, ".png"))
ggsave(out_file, p, width=8, height=10)

