library(data.table)
library(ggplot2)
library(viridis)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
# fig_suffix    <- "_initClimSupportNormalDistribution"
# fig_suffix = '_CESM_HR_local_natVar_multiplier1'
# fig_suffix = '_CESM_LM_local_Tambora_2030_normalDistribution'  
# fig_suffix    <- "_initClimSupportNormalDistribution" 
fig_suffix = '_CESM_HR_local_natVar_500000runs'
years         <- 2020:2100

# ---- Load data ----
message("Loading: ", data_dir, "emissions", fig_suffix, ".csv")
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
message("Loading: ", data_dir, "natvar", fig_suffix, ".csv")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Compute mean natvar over window (index 6:15) for each run ----
mean_natvar_window <- rowMeans(natvar_mat[, 6:15, drop=FALSE], na.rm=TRUE)

# ---- Define bins for mean natvar ----
bin_width <- 0.1
mag_centers <- seq(-1.5, 1.5, by=bin_width)
half_width <- bin_width / 2

# ---- Bin runs and compute median emissions trajectory for each bin ----
res <- list()
bin_counts <- data.table(mag_bin = numeric(), n_runs = integer())
k <- 1L

for (mag_center in mag_centers) {
  lo_mag <- mag_center - half_width
  hi_mag <- mag_center + half_width
  idx_bin <- which(mean_natvar_window >= lo_mag & mean_natvar_window < hi_mag)
  n_runs <- length(idx_bin)
  bin_counts <- rbind(bin_counts, data.table(mag_bin = mag_center, n_runs = n_runs))
  if (n_runs > 0) {
    med_ems <- apply(ems_mat[idx_bin, , drop=FALSE], 2, median, na.rm=TRUE)
    res[[k]] <- data.table(
      year = years,
      median_ems = med_ems,
      mag_bin = mag_center,
      n_runs = n_runs
    )
    k <- k + 1L
  }
}

dt_plot <- rbindlist(res)
# ---- Plot: median emissions trajectory for each mean natvar bin ----
message("Plotting median emissions trajectories by mean natvar bin...")

# Compute overall median emissions trajectory for all runs
overall_median_ems <- apply(ems_mat, 2, median, na.rm=TRUE)
dt_overall <- data.table(
  year = years,
  median_ems = overall_median_ems
)

p <- ggplot(dt_plot, aes(x = year, y = median_ems, group = mag_bin, color = mag_bin)) +
  geom_line(size = 1.1) +
  # Add bold line for overall median emissions trajectory (all runs)
  geom_line(
    data = dt_overall,
    aes(x = year, y = median_ems),
    color = "#0a7800", # Distinct red, complementary to viridis
    size = 2.2,
    inherit.aes = FALSE,
    alpha=0.5
  ) +
  scale_color_viridis_c(
    name = "Mean NatVar (degC)",
    option = "C",
    limits = range(mag_centers)
  ) +
  labs(
    title = paste0("Median Emissions Trajectory by Mean NatVar Bin (Window 6:15)\n", fig_suffix),
    x = "Year",
    y = "Median Emissions (GtC/yr)"
  ) +
  theme_minimal(base_size=14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

# ---- Additional scatterplot: bin counts (log scale) ----
message("Plotting sample counts per mean natvar bin (log scale)...")
p_counts <- ggplot(bin_counts, aes(x = mag_bin, y = n_runs, color = mag_bin)) +
  geom_point(size = 3) +
  scale_color_viridis_c(
    name = "Mean NatVar (degC)",
    option = "C",
    limits = range(mag_centers)
  ) +
  scale_y_log10(
    breaks = c(1, 10, 100, 1000, 10000),
    labels = scales::comma,
    limits = c(1, 12000)
  ) +
  labs(
    title = paste0("Sample Count per Mean NatVar Bin (Window 6:15, Log Scale)\n", fig_suffix),
    x = "Mean NatVar Bin Center (degC)",
    y = "Number of Samples (log scale)"
  ) +
  theme_minimal(base_size=14) 
  
# ---- Save figures ----
out_dir <- "../results"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(
  out_dir,
  paste0("median_ems_trajectories_by_meanNatVar_bin_", fig_suffix, ".png")
)
message("Saving: ", out_file)
ggsave(out_file, p, width=7, height=6, dpi=300)

out_file_counts <- file.path(
  out_dir,
  paste0("sample_count_by_meanNatVar_bin_", fig_suffix, ".png")
)
message("Saving: ", out_file_counts)
ggsave(out_file_counts, p_counts, width=8, height=6)

