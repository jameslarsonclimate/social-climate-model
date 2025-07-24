library(data.table)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix    <- '_CESM_HR_local_natVar_multiplier1'
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

# ---- Standardize natvar_mat by row (across time) ----
row_sd <- apply(natvar_mat, 1, sd, na.rm=TRUE)
natvar_mat_sigma <- natvar_mat / row_sd

res <- list()
k   <- 1L

for (dur in 1:max_dur) {
  print(sprintf("Processing: Duration = %d years out of %d max years", dur, max_dur))
  end_year <- start_year + dur - 1
  idx_range <- which(years >= start_year & years <= end_year)
  avg_nat   <- rowMeans(natvar_mat_sigma[, idx_range, drop=FALSE], na.rm=TRUE)

  for (mag in bin_centers) {
    lo <- mag - half_width
    hi <- mag + half_width
    idx <- which(avg_nat >= lo & avg_nat < hi)
    zero_year <- NA_integer_

    if (length(idx) > 0) {
      med_traj  <- apply(ems_mat[idx, , drop=FALSE], 2, median, na.rm=TRUE)
      zz        <- which(med_traj <= 0)
      zero_year <- if (length(zz)>0) years[min(zz)] else NA_integer_
    }

    res[[k]] <- list(
      duration   = dur,
      magnitude  = mag,
      zero_year  = zero_year,
      n_runs     = length(idx)
    )
    k <- k + 1L
  }
}

dt_bin <- rbindlist(res)[!is.na(zero_year)]

# ---- Plot heatmap of net-zero year by standardized natvar bin & duration ----

fill_breaks <- seq(2076,2098,2)
fill_limits <- c(min(fill_breaks), max(fill_breaks))

p_bin <- ggplot(dt_bin, aes(x = magnitude, y = duration, fill = zero_year)) +
  geom_tile() +
  scale_fill_stepsn(
    colors = brewer.pal(length(fill_breaks) - 1, "PRGn"),
    name   = "Year Net-Zero",
    limits = fill_limits,
    breaks = fill_breaks,
    oob    = scales::oob_squish
  ) +
  guides(fill = guide_colorbar(
    barwidth  = unit(12, "cm"),
    barheight = unit(0.5, "cm"),
    title.position = "top",
    show.limits    = TRUE
  )) +
  labs(
    title = paste0("Net-Zero Year by Standardized NatVar Bin and Duration (σ units)\n", fig_suffix),
    x = "Average Standardized Natural Variability (σ)",
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
  paste0("netzero_heatmap_bin_sigma", fig_suffix, ".png")
)
message("Saving: ", out_file)
ggsave(out_file, p_bin, width=8, height=6)
