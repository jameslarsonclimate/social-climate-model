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
plot_mag <- -1.0   # bin center for magnitude
half_width <- 0.05
n_samples <- 10    # Number of random samples to plot

# Datasets to compare
datasets <- list(
  list(label = "Model", suffix = "_initClimSupportNormalDistribution"),
  list(label = "CESMx1", suffix = "_CESM_HR_local_natVar_multiplier1")
  # list(label = "CESMx0.5", suffix = "_CESM_HR_local_natVar_multiplier05"),
  # list(label = "ERA5", suffix = "_ERA5natVar")
)

# Store sampled timeseries for each dataset
sampled_list <- list()

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
    set.seed(42) # for reproducibility
    idx_sample <- if (length(idx) > n_samples) sample(idx, n_samples) else idx
    for (i in seq_along(idx_sample)) {
      sampled_list[[length(sampled_list) + 1]] <- data.table(
        year = years,
        natvar = natvar_mat[idx_sample[i], ],
        run = i,
        dataset = label,
        run_id = idx_sample[i]
      )
    }
  }
}

# Combine all samples
df_samples <- rbindlist(sampled_list)

# Now, for each run (1 to n_samples), plot all datasets' timeseries for that run index
plots <- vector("list", n_samples)
for (i in 1:n_samples) {
  df_plot <- df_samples[run == i]
  plots[[i]] <- ggplot(df_plot, aes(x = year, y = natvar, color = dataset)) +
    geom_line(size = 1.1) +
    labs(
      title = paste0("Random Sample ", i, " (run_id: ", paste(df_plot$run_id, collapse=", "), ")"),
      x = "Year",
      y = "Natural Variability (degC)"
    ) +
    coord_cartesian(ylim = c(-3, 3)) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal"
    )
}

# Combine plots in a 2-column, 5-row grid with shared legend
combined_plot <- wrap_plots(plots, ncol = 2, nrow = 5, guides = "collect") &
  theme(legend.position = "bottom") &
  guides(color = guide_legend(ncol = 3))

# Add a global title
combined_plot <- combined_plot +
  plot_annotation(
    title = paste0("Randomly Selected Natural Variability Timeseries\nDuration = ", plot_dur, ", Magnitude = ", plot_mag),
    theme = theme(plot.title = element_text(size = 16, hjust = 0.5))
  )

# Save plot
out_dir <- "../results"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(out_dir, paste0("sampled_natvar_timeseries_dur", plot_dur, "_mag", plot_mag, ".png"))
ggsave(out_file, combined_plot, width=12, height=18)
