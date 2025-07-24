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
k <- 1L

for (sd_full_center in sd_full_centers) {
  lo_full <- sd_full_center - half_width
  hi_full <- sd_full_center + half_width
  idx_bin <- which(sd_full_all >= lo_full & sd_full_all < hi_full)
  n_runs <- length(idx_bin)
  if (n_runs > 0) {
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
    option = "C"
  ) +
  labs(
    title = paste0("Median Emissions Trajectory by Full-Series SD Bin\n", fig_suffix),
    x = "Year",
    y = "Median Emissions (GtC/yr)"
  ) +
  theme_minimal(base_size=14) +
  theme(
    legend.position = "right"
  )

# ---- Save figure ----
out_dir <- "../results/heatmaps"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(
  out_dir,
  paste0("median_ems_trajectories_by_fullSD_bin_", fig_suffix, ".png")
)
message("Saving: ", out_file)
ggsave(out_file, p, width=10, height=6)
