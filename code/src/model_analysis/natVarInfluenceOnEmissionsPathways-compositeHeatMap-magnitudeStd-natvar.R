library(data.table)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
# fig_suffix    <- '_CESM_HR_local_natVar_multiplier1'
fig_suffix    <- "_initClimSupportNormalDistribution" #-natVarMultiplier10"

years         <- 2020:2100
start_year    <- 2025
max_dur       <- 20
bin_centers   <- seq(-1.5, 1.5, by=0.1)
half_width    <- 0.05

# ---- Load data ----
message("Loading: ", data_dir, "emissions", fig_suffix, ".csv")
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
message("Loading: ", data_dir, "natvar", fig_suffix, ".csv")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Loop over duration & magnitude bins, compute full-series SD for each bin ----
res <- list()
k   <- 1L

for (dur in 1:max_dur) {
  print(sprintf("Processing: Duration = %d years out of %d max years", dur, max_dur))
  end_year <- start_year + dur - 1
  idx_range <- which(years >= start_year & years <= end_year)
  std_nat_window <- apply(natvar_mat[, idx_range, drop=FALSE], 1, sd, na.rm=TRUE)

  for (mag in bin_centers) {
    lo <- mag - half_width
    hi <- mag + half_width
    idx <- which(std_nat_window >= lo & std_nat_window < hi)

    sd_full <- NA_real_
    n_runs <- length(idx)
    if (n_runs > 0) {
      # Compute the standard deviation over the full time series for all runs in the bin
      sd_full <- mean(apply(natvar_mat[idx, , drop=FALSE], 1, sd, na.rm=TRUE), na.rm=TRUE)
    }

    res[[k]] <- list(
      duration   = dur,
      magnitude  = mag,
      sd_full    = sd_full,
      n_runs     = n_runs
    )
    k <- k + 1L
  }
}

dt_bin <- rbindlist(res)[!is.na(sd_full)]

# ---- Plot heatmap of full-series SD by bin & duration ----
message("Plotting heatmap of full-series SD by bin and duration...")
p_bin <- ggplot(dt_bin, aes(x = magnitude, y = duration, fill = sd_full)) +
  geom_tile() +
  scale_fill_viridis_c(
    name   = "Mean SD\n(full series)",
    option = "C"
  ) +
  labs(
    title = paste0("Mean Standard Deviation of Full NatVar Series by Bin\n", fig_suffix),
    x = "Standard Deviation in Window (degC)",
    y = "Duration (yrs)"
  ) +
  guides(fill = guide_colorbar(barwidth = unit(12, "cm"), 
                               barheight = unit(0.5, "cm"))) +
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
  paste0("fullseriesSD_heatmap_bin_stdNatVarBin_", fig_suffix, ".png")
)
message("Saving: ", out_file)
ggsave(out_file, p_bin, width=8, height=6)


# ---- Compute ratio of full-series SD to window SD for each bin ----
dt_bin[, ratio_sd := sd_full / magnitude]

# ---- Plot heatmap of ratio by bin & duration with log-diverging colorbar ----
# User can set colorbar range here:
ratio_clim <- c(0.5, 2)  # <-- User: set min/max for colorbar

# Create log-spaced breaks centered at 1
log_breaks <- round(exp(seq(log(ratio_clim[1]), log(ratio_clim[2]), length.out = 10)), 2)

message("Plotting heatmap of SD ratio (full series / window) by bin and duration with log-diverging colorbar...")
p_ratio <- ggplot(dt_bin, aes(x = magnitude, y = duration, fill = ratio_sd)) +
  geom_tile() +
  scale_fill_stepsn(
    colors = brewer.pal(length(log_breaks) - 1, "PRGn"),
    name   = "SD Ratio\n(full/window)",
    limits = ratio_clim,
    breaks = log_breaks,
    trans = "log",
    oob = scales::oob_squish
  ) +
  labs(
    title = paste0("Ratio of Full-Series SD to Window SD by Bin\n", fig_suffix),
    x = "Standard Deviation in Window (degC)",
    y = "Duration (yrs)"
  ) +
  guides(fill = guide_colorbar(barwidth = unit(12, "cm"), 
                               barheight = unit(0.5, "cm"))) +
  theme_minimal(base_size=14) +
  theme(
    panel.grid     = element_blank(),
    legend.position = "bottom"
  )

# ---- Save ratio figure ----
out_file_ratio <- file.path(
  out_dir,
  paste0("fullseriesSD_ratio_heatmap_bin_stdNatVarBin_", fig_suffix, ".png")
)
message("Saving: ", out_file_ratio)
ggsave(out_file_ratio, p_ratio, width=8, height=6)
