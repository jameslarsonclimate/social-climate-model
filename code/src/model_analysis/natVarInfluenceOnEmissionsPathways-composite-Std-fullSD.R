library(data.table)
library(ggplot2)
library(viridis)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix    <- "_initClimSupportNormalDistribution"
fig_suffix = '_CESM_HR_local_natVar_multiplier1'
years         <- 2020:2100
 
# ---- Load data ----
message("Loading: ", data_dir, "emissions", fig_suffix, ".csv")
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
message("Loading: ", data_dir, "natvar", fig_suffix, ".csv")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Compute SD of full timeseries for each run ----
sd_full_all <- apply(natvar_mat, 1, sd, na.rm=TRUE)

# ---- Define bins for SD of full timeseries ----
bin_width <- 0.05
sd_full_centers <- seq(min(sd_full_all, na.rm=TRUE), max(sd_full_all, na.rm=TRUE), by=bin_width)
half_width <- bin_width / 2

# ---- Bin runs and compute median emissions trajectory for each bin ----
res <- list()
bin_counts <- data.table(sd_bin = numeric(), n_runs = integer())
k <- 1L

for (sd_full_center in sd_full_centers) {
  lo_full <- sd_full_center - half_width
  hi_full <- sd_full_center + half_width
  idx_bin <- which(sd_full_all >= lo_full & sd_full_all < hi_full)
  n_runs <- length(idx_bin)
  # Store bin counts for scatterplot
  bin_counts <- rbind(bin_counts, data.table(sd_bin = sd_full_center, n_runs = n_runs))
  if (n_runs > 100) {
    med_ems <- apply(ems_mat[idx_bin, , drop=FALSE], 2, median, na.rm=TRUE)
    res[[k]] <- data.table(
      year = years,
      median_ems = med_ems,
      sd_bin = sd_full_center,
      n_runs = n_runs
    )
    k <- k + 1L
  }
}

dt_plot <- rbindlist(res)

# ---- Plot: median emissions trajectory for each SD bin ----
message("Plotting median emissions trajectories by SD bin...")
p <- ggplot(dt_plot, aes(x = year, y = median_ems, group = sd_bin, color = sd_bin)) +
  geom_line(size = 1.1) +
  scale_color_viridis_c(
    name = "SD of Full Series (degC)",
    option = "C",
    guide = guide_colorbar(
      barwidth = unit(15, "lines"),
      barheight = unit(1, "lines"),
      direction = "horizontal",
      title.position = "top"
    ),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4),
    labels = c("0", "0.2", "0.4", "0.6", "0.8", "1.0", "1.2", "1.4"),
    limits = range(sd_full_centers)
  ) +
  labs(
    title = paste0("Median Emissions Trajectory by Full-Series SD Bin\n", fig_suffix),
    x = "Year",
    y = "Median Emissions (GtC/yr)"
  ) +
  theme_minimal(base_size=14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

# ---- Additional scatterplot: bin counts (log scale) ----
message("Plotting sample counts per SD bin (log scale)...")
p_counts <- ggplot(bin_counts, aes(x = sd_bin, y = n_runs, color = sd_bin)) +
  geom_point(size = 3) +
  scale_color_viridis_c(
    name = "SD of Full Series (degC)",
    option = "C",
    limits = range(sd_full_centers)
  ) +
  scale_y_log10(
    breaks = c(10, 100, 1000, 10000),
    labels = scales::comma,
    limits = c(10, 12000)
  ) +
  labs(
    title = paste0("Sample Count per Full-Series SD Bin (Log Scale)\n", fig_suffix),
    x = "SD Bin Center (degC)",
    y = "Number of Samples (log scale)"
  ) +
  theme_minimal(base_size=14) +
  theme(
    legend.position = "right"
  )
  # ---- Save figures ----
  out_dir <- "../results"
  dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
  out_file <- file.path(
    out_dir,
    paste0("median_ems_trajectories_by_fullSD_bin_", fig_suffix, ".png")
  )
  message("Saving: ", out_file)
  ggsave(out_file, p, width=7, height=6, dpi=300)

out_file_counts <- file.path(
  out_dir,
  paste0("sample_count_by_fullSD_bin_", fig_suffix, ".png")
)
message("Saving: ", out_file_counts)
ggsave(out_file_counts, p_counts, width=8, height=6)