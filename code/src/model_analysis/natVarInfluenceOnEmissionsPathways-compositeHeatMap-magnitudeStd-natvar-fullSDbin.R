library(data.table)
library(ggplot2)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix    <- "_initClimSupportNormalDistribution"
# fig_suffix    <- '_CESM_HR_local_natVar_multiplier1'
years         <- 2020:2100
start_year    <- 2025

# ---- User input: window size (years) ----
window_size <- 10  # User can change this value

# ---- Load data ----
message("Loading: ", data_dir, "emissions", fig_suffix, ".csv")
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
message("Loading: ", data_dir, "natvar", fig_suffix, ".csv")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Compute SD of full timeseries for each run ----
sd_full_all <- apply(natvar_mat, 1, sd, na.rm=TRUE)

# ---- Define bins for SD of full timeseries ----
sd_full_centers <- seq(min(sd_full_all, na.rm=TRUE), max(sd_full_all, na.rm=TRUE), by=0.05)
half_width <- 0.025

# ---- Compute SD of window for each run ----
window_start <- start_year
window_end <- start_year + window_size - 1
idx_range <- which(years >= window_start & years <= window_end)
sd_window <- apply(natvar_mat[, idx_range, drop=FALSE], 1, sd, na.rm=TRUE)

# ---- Define bins for SD of window ----
sd_window_centers <- seq(min(sd_window, na.rm=TRUE), max(sd_window, na.rm=TRUE), by=0.05)

# ---- Bin by SD of full timeseries, then by SD of window ----
res <- list()
k <- 1L

for (sd_full_center in sd_full_centers) {
  lo_full <- sd_full_center - half_width
  hi_full <- sd_full_center + half_width
  idx_full <- which(sd_full_all >= lo_full & sd_full_all < hi_full)

  for (sd_win_center in sd_window_centers) {
    lo_win <- sd_win_center - half_width
    hi_win <- sd_win_center + half_width
    idx_bin <- idx_full[which(sd_window[idx_full] >= lo_win & sd_window[idx_full] < hi_win)]
    n_runs <- length(idx_bin)
    median_cumems <- NA_real_
    if (n_runs > 0) {
      # Median cumulative emissions for this bin
      cum_ems <- rowSums(ems_mat[idx_bin, , drop=FALSE], na.rm=TRUE)
      median_cumems <- median(cum_ems, na.rm=TRUE)
    }
    res[[k]] <- list(
      sd_full = sd_full_center,
      sd_window = sd_win_center,
      median_cumems = median_cumems,
      n_runs = n_runs
    )
    k <- k + 1L
  }
}

dt_bin <- rbindlist(res)[!is.na(median_cumems)]

# ---- Set color scale limits and breaks ----
fill_limits <- range(dt_bin$median_cumems, na.rm=TRUE)
fill_breaks <- pretty(fill_limits, n = 9)

# ---- Plot heatmap of median cumulative emissions by SD bins ----
message("Plotting heatmap of median cumulative emissions by SD bins...")
p_bin <- ggplot(dt_bin, aes(x = sd_window, y = sd_full, fill = median_cumems)) +
  geom_tile() +
  scale_fill_stepsn(
    colors = brewer.pal(length(fill_breaks) - 1, "PRGn"),
    name   = "Median Cumulative\nEmissions",
    limits = fill_limits,
    breaks = fill_breaks,
    oob    = scales::oob_squish
  ) +
  guides(fill = guide_colorbar(barwidth = unit(12, "cm"),
                               barheight = unit(0.5, "cm"))) +
  labs(
    title = paste0("Median Cumulative Emissions by SD Bins\n", fig_suffix, "\nWindow size: ", window_size, " years"),
    x = "Standard Deviation in Window (degC)",
    y = "Standard Deviation of Full Series (degC)"
  ) +
  theme_minimal(base_size=14) +
  theme(
    panel.grid     = element_blank(),
    legend.position = "bottom"
  )

# ---- Save figure ----
out_dir <- "../results/heatmaps"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(
  out_dir,
  paste0("cumems_heatmap_by_fullSD_bin_", fig_suffix, "_window", window_size, ".png")
)
message("Saving: ", out_file)
ggsave(out_file, p_bin, width=8, height=6)
