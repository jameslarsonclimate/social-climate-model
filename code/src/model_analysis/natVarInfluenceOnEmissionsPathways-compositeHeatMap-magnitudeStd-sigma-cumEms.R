library(data.table)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix    <- "_initClimSupportNormalDistribution" #-natVarMultiplier10"
# fig_suffix    <- '_CESM_HR_local_natVar_multiplier1'
years         <- 2020:2100
start_year    <- 2025
max_dur       <- 20
bin_width     <- 0.2

# ---- Load data ----
message("Loading: ", data_dir, "emissions", fig_suffix, ".csv")
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
message("Loading: ", data_dir, "natvar", fig_suffix, ".csv")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Standardize natvar_mat by row (across time) ----
row_sd <- apply(natvar_mat, 1, sd, na.rm=TRUE)
natvar_mat_sigma <- natvar_mat / row_sd

# ---- Precompute cumulative emissions for each run ----
cum_ems <- rowSums(ems_mat, na.rm=TRUE)

res <- list()
k   <- 1L

for (dur in 2:max_dur) {
  print(sprintf("Processing: Duration = %d years out of %d max years", dur, max_dur))
  end_year <- start_year + dur - 1
  idx_range <- which(years >= start_year & years <= end_year)
  # Compute SD within the window for each run (on standardized natvar)
  avg_std <- apply(natvar_mat_sigma[, idx_range, drop=FALSE], 1, sd, na.rm=TRUE)

  # Define bin centers for avg_std
  std_min <- min(avg_std, na.rm=TRUE)
  std_max <- max(avg_std, na.rm=TRUE)
  bin_centers <- seq(std_min, std_max, by=bin_width)
  half_width <- bin_width / 2

  for (std_center in bin_centers) {
    lo <- std_center - half_width
    hi <- std_center + half_width
    idx <- which(avg_std >= lo & avg_std < hi)
    median_cum_ems <- NA_real_

    if (length(idx) > 0) {
      median_cum_ems <- median(cum_ems[idx], na.rm=TRUE)
    }

    res[[k]] <- list(
      duration   = dur,
      avg_std    = std_center,
      median_cum_ems = median_cum_ems,
      n_runs     = length(idx)
    )
    k <- k + 1L
  }
}

dt_bin <- rbindlist(res)[!is.na(median_cum_ems)]

# ---- Plot heatmap of median cumulative emissions by window SD bin & duration ----

fill_limits <- range(dt_bin$median_cum_ems, na.rm=TRUE)
fill_breaks <- pretty(fill_limits, n = 9)

p_bin <- ggplot(dt_bin, aes(x = avg_std, y = duration, fill = median_cum_ems)) +
  geom_tile(width = bin_width, height = 1) +
  scale_fill_viridis_c(
    option = "plasma",
    name   = "Median Cumulative Emissions",
    limits = fill_limits,
    breaks = fill_breaks,
    oob    = scales::oob_squish
  ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  guides(fill = guide_colorbar(
    barwidth  = unit(12, "cm"),
    barheight = unit(0.5, "cm"),
    title.position = "top",
    show.limits    = TRUE
  )) +
  labs(
    title = paste0("Median Cumulative Emissions by Window SD Bin and Duration (σ units)\n", fig_suffix),
    x = "Window Standard Deviation of Standardized NatVar (σ)",
    y = "Duration (yrs)"
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
  paste0("medianCumEms_heatmap_windowStd_sigma", fig_suffix, ".png")
)
message("Saving: ", out_file)
ggsave(out_file, p_bin, width=8, height=6)
