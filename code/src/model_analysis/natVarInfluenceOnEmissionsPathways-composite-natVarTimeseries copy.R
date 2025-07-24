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
  list(label = "CESMx1", suffix = "_CESM_HR_local_natVar_multiplier1")
  # list(label = "CESMx0.5", suffix = "_CESM_HR_local_natVar_multiplier05"),
  # list(label = "ERA5", suffix = "_ERA5natVar")
)

median_natvar_list <- list()

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
    # Median timeseries for this bin
    median_natvar <- apply(natvar_mat[idx, , drop=FALSE], 2, median, na.rm=TRUE)
    # Average pairwise correlation
    cor_val <- NA
    if (length(idx) > 1) {
      cor_mat <- cor(t(natvar_mat[idx, , drop=FALSE]), use = "pairwise.complete.obs")
      cor_val <- mean(cor_mat[upper.tri(cor_mat)], na.rm = TRUE)
    }
    # Standard deviation across all values in the bin (all runs, all years)
    sd_val <- sd(as.numeric(natvar_mat[idx, , drop=FALSE]), na.rm=TRUE)
    median_natvar_list[[length(median_natvar_list) + 1]] <- data.frame(
      year = years,
      median_natvar = median_natvar,
      dataset = label,
      n_runs = length(idx),
      cor = cor_val,
      sd = sd_val
    )
  }
}

df_plot <- rbindlist(median_natvar_list)

# Create a new label with run count, correlation, and sd for legend
df_plot[, dataset_n := paste0(
  dataset, " (n=", n_runs, ")\n",
  "corr=", ifelse(is.na(cor), "NA", sprintf("%.2f", cor)), "\n",
  "sd=", ifelse(is.na(sd), "NA", sprintf("%.2f", sd))
)]

p <- ggplot(df_plot, aes(x = year, y = median_natvar, color = dataset_n)) +
  geom_line(size = 1.2) +
  labs(
    title = paste0("Median Natural Variability Timeseries\nDuration = ", plot_dur, ", Magnitude = ", plot_mag),
    x = "Year",
    y = "Median Natural Variability (degC)",
    color = "Dataset (n in bin)\ncorr = mean pairwise\nsd = all values"
  ) +
  theme_minimal(base_size = 14)

# Save plot
out_dir <- "../results/"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(out_dir, paste0("median_natvar_timeseries_dur", plot_dur, "_mag", plot_mag, ".png"))
ggsave(out_file, p, width=8, height=5)
