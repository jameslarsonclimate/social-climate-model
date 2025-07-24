library(data.table)
library(ggplot2)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix    <- "_initClimSupportNormalDistribution" #-natVarMultiplier10"
# fig_suffix    <- '_CESM_HR_local_natVar_multiplier1'
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

# ---- Compute global median net‐zero year and cumulative emissions ----
med_all       <- apply(ems_mat, 2, median, na.rm=TRUE)
zz_all        <- which(med_all <= 0)
zero_year_all <- if (length(zz_all)>0) years[min(zz_all)] else NA_integer_
cum_ems_all   <- median(rowSums(ems_mat, na.rm=TRUE), na.rm=TRUE)

# ---- Bin by SD of full timeseries ----
res <- list()
k <- 1L

for (sd_full_center in sd_full_centers) {
  lo_full <- sd_full_center - half_width
  hi_full <- sd_full_center + half_width
  idx_bin <- which(sd_full_all >= lo_full & sd_full_all < hi_full)
  n_runs <- length(idx_bin)
  zero_year <- NA_real_
  median_cumems <- NA_real_
  if (n_runs > 0) {
    # Median net-zero year for this bin
    med_traj  <- apply(ems_mat[idx_bin, , drop=FALSE], 2, median, na.rm=TRUE)
    zz        <- which(med_traj <= 0)
    zero_year <- if (length(zz)>0) years[min(zz)] else NA_integer_
    # Median cumulative emissions for this bin
    cum_ems <- rowSums(ems_mat[idx_bin, , drop=FALSE], na.rm=TRUE)
    median_cumems <- median(cum_ems, na.rm=TRUE)
  }
  res[[k]] <- list(
    sd_full = sd_full_center,
    zero_year = zero_year,
    median_cumems = median_cumems,
    n_runs = n_runs
  )
  k <- k + 1L
}

dt_bin <- rbindlist(res)[n_runs > 0 & !is.na(zero_year)]

# ---- Bar plot: median net-zero year by SD bin ----
p_zero <- ggplot(dt_bin, aes(x = sd_full, y = zero_year)) +
  geom_col(fill = RColorBrewer::brewer.pal(9, "PRGn")[6], width = bin_width * 0.9) +
  labs(
    title = paste0("Median Net-Zero Year by Full-Series SD Bin\n", fig_suffix),
    x = "Standard Deviation of Full Series (degC)",
    y = "Median Year Net-Zero"
  ) +
  ylim(2065, 2101) +
  theme_minimal(base_size=14)

# ---- Bar plot: median cumulative emissions by SD bin ----
p_cumems <- ggplot(dt_bin, aes(x = sd_full, y = median_cumems)) +
  geom_col(fill = RColorBrewer::brewer.pal(9, "PRGn")[4], width = bin_width * 0.9) +
  labs(
    title = paste0("Median Cumulative Emissions by Full-Series SD Bin\n", fig_suffix),
    x = "Standard Deviation of Full Series (degC)",
    y = "Median Cumulative Emissions"
  ) +
  theme_minimal(base_size=14)

# ---- Save figures ----
out_dir <- "../results/heatmaps"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)

out_file_zero <- file.path(
  out_dir,
  paste0("netzero_barplot_by_fullSD_bin_", fig_suffix, ".png")
)
message("Saving: ", out_file_zero)
ggsave(out_file_zero, p_zero, width=8, height=5)

out_file_cumems <- file.path(
  out_dir,
  paste0("cumems_barplot_by_fullSD_bin_", fig_suffix, ".png")
)
message("Saving: ", out_file_cumems)
ggsave(out_file_cumems, p_cumems, width=8, height=5)
