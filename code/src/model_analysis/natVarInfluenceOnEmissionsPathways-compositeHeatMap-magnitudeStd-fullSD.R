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
bin_centers   <- seq(0, 1.5, by=0.1)
half_width    <- 0.05


# ---- Load data ----
message("Loading: ", data_dir, "emissions", fig_suffix, ".csv")
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
message("Loading: ", data_dir, "natvar", fig_suffix, ".csv")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Precompute SD of full timeseries for each run ----
sd_full_natvar <- apply(natvar_mat, 1, sd, na.rm=TRUE)

res <- list()
k   <- 1L

for (dur in 1:max_dur) {
  print(sprintf("Processing: Duration = %d years out of %d max years", dur, max_dur))
  if (dur < 2) next  # Skip duration 1, SD not defined

  end_year <- start_year + dur - 1
  idx_range <- which(years >= start_year & years <= end_year)
  # Compute SD within the window for each run
  window_sd <- apply(natvar_mat[, idx_range, drop=FALSE], 1, sd, na.rm=TRUE)

  # Only proceed if there are finite values
  if (all(is.na(window_sd)) || length(window_sd[is.finite(window_sd)]) == 0) next

  # half_width <- bin_width / 2

  for (mag in bin_centers) {
    lo <- mag - half_width
    hi <- mag + half_width
    idx <- which(window_sd >= lo & window_sd < hi)
    median_sd_full <- NA_real_

    if (length(idx) > 0) {
      median_sd_full <- median(sd_full_natvar[idx], na.rm=TRUE)
    }

    res[[k]] <- list(
      duration   = dur,
      window_sd  = sd_center,
      median_sd_full = median_sd_full,
      n_runs    = length(idx)
    )
    k <- k + 1L
  }
}

dt_bin <- rbindlist(res)[!is.na(median_sd_full)]

# ---- Plot heatmap of median SD of full natvar by window SD bin & duration ----

fill_limits <- range(dt_bin$median_sd_full, na.rm=TRUE)
fill_breaks <- pretty(fill_limits, n = 9)

p_bin <- ggplot(dt_bin, aes(x = window_sd, y = duration, fill = median_sd_full)) +
  geom_tile(width = bin_width, height = 1) +  # Fix: set tile width/height to fill bins
  scale_fill_viridis_c(
    option = "plasma",
    name   = "Median SD\n(full natvar)",
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
    title = paste0("Median SD of Full NatVar by Window SD Bin and Duration\n", fig_suffix),
    x = "Window SD of Natural Variability (degC)",
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
  paste0("medianSD_heatmap_windowSDbin", fig_suffix, ".png")
)
message("Saving: ", out_file)
ggsave(out_file, p_bin, width=8, height=6)
