library(data.table)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
# fig_suffix    <- "_initClimSupportNormalDistribution"
fig_suffix = '_initClimSupportNormalDistribution-500Kruns'


years         <- 2020:2100
start_year    <- 2025
# max_dur       <- length(years) - (start_year - min(years))
max_dur       <- 20
bin_centers   <- seq(-1.5, 1.5, by=0.1)
half_width    <- 0.05

# ---- Load data ----
message("Loading: ", data_dir, "emissions", fig_suffix, ".csv")
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
message("Loading: ", data_dir, "natvar", fig_suffix, ".csv")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Load and append data from all suffixes ----

# print reading messages and the suffixes being loaded

# fig_suffixes  <- c(
#   "_initClimSupportNormalDistribution",
#   "_initClimSupportNormalDistribution-resample1",
#   "_initClimSupportNormalDistribution-resample2",
#   "_initClimSupportNormalDistribution-resample3"
# )

# message("Loading data for suffixes: \n", paste(fig_suffixes, collapse=",\n"))

# ems_list    <- list()
# natvar_list <- list()
# for (suffix in fig_suffixes) {
#   message("Loading: ", data_dir, "emissions", suffix, ".csv")
#   ems_list[[suffix]]    <- as.matrix(fread(paste0(data_dir, "emissions", suffix, ".csv")))
#   message("Loading: ", data_dir, "natvar", suffix, ".csv")
#   natvar_list[[suffix]] <- as.matrix(fread(paste0(data_dir, "natvar", suffix, ".csv")))
# }
# ems_mat    <- do.call(rbind, ems_list)
# natvar_mat <- do.call(rbind, natvar_list)

# message("Data loaded successfully.")
# fig_suffix    <- "_initClimSupportNormalDistribution-appended" # Update suffix for output


# ---- User options for hatching ----
# If hatch_by_percentile = FALSE, hatching requires p-value criterion AND at least hatch_min_n samples.
# If hatch_by_percentile = TRUE, hatching applies to the lowest hatch_percentile fraction of grid boxes by sample count
hatch_min_n       <- 1        # default absolute minimum samples required for hatching when not using percentiles
hatch_by_percentile <- FALSE     # when TRUE, use the lowest-percentile rule below instead of hatch_min_n
hatch_percentile  <- 0.20        # lowest 20% of grid boxes by sample count (only used if hatch_by_percentile == TRUE)
hatch_p_alpha     <- 0.01        # p-value threshold for "non-significant" (keep existing semantics)

# ---- Loop over duration & magnitude bins (store also n_runs) ----
res <- list()
k   <- 1L
for (dur in 1:max_dur) {
  print(sprintf("Processing: Duration = %d years out of %d max years", dur, max_dur))

  end_year <- start_year + dur - 1
  idx_range <- which(years >= start_year & years <= end_year)
  avg_nat   <- rowMeans(natvar_mat[, idx_range, drop=FALSE], na.rm=TRUE)

  for (mag in bin_centers) {
    lo <- mag - half_width
    hi <- mag + half_width
    idx <- which(avg_nat >= lo & avg_nat < hi)

    n_runs <- length(idx)
    if (n_runs==0) {
      zero_year <- NA_integer_
    } else {
      med_traj  <- apply(ems_mat[idx, , drop=FALSE], 2, median, na.rm=TRUE)
      zz        <- which(med_traj <= 0)
      zero_year <- if (length(zz)>0) years[min(zz)] else NA_integer_
    }
    res[[k]] <- list(
      duration   = dur,
      magnitude  = mag,
      zero_year  = zero_year, 
      n_runs     = n_runs
    )
    k <- k + 1L
  }
}

# build full table (include bins with NA zero_year for percentile calculations)
dt_allbins <- rbindlist(res)
dt_bin     <- dt_allbins[!is.na(zero_year)]

# ---- Compute global median net‐zero year ----
med_all       <- apply(ems_mat, 2, median, na.rm=TRUE)
zz_all        <- which(med_all <= 0)
zero_year_all <- if (length(zz_all)>0) years[min(zz_all)] else NA_integer_

# ---- Statistical significance testing using the Wilcox Test ----
message("Computing statistical significance of net-zero years by bin (Wilcox Test)...")
dt_bin[, p_value := NA_real_]

all_zero_years <- apply(ems_mat, 1, function(x) {
  zz <- which(x <= 0)
  if (length(zz) > 0) years[min(zz)] else NA_integer_
})

for (i in seq_len(nrow(dt_bin))) {
  if (i %% 20 == 1) message(sprintf("Processing bin %d of %d", i, nrow(dt_bin)))
  dur <- dt_bin$duration[i]
  mag <- dt_bin$magnitude[i]
  end_year <- start_year + dur - 1
  idx_range <- which(years >= start_year & years <= end_year)
  avg_nat   <- rowMeans(natvar_mat[, idx_range, drop=FALSE], na.rm=TRUE)
  lo <- mag - half_width
  hi <- mag + half_width
  idx <- which(avg_nat >= lo & avg_nat < hi)
  if (length(idx) > 0) {
    bin_zero_years <- apply(ems_mat[idx, , drop=FALSE], 1, function(x) {
      zz <- which(x <= 0)
      if (length(zz) > 0) years[min(zz)] else NA_integer_
    })
    bin_zero_years <- bin_zero_years[!is.na(bin_zero_years)]
    if (length(bin_zero_years) > 1 && !is.na(zero_year_all)) {
      dt_bin$p_value[i] <- wilcox.test(bin_zero_years, all_zero_years, alternative = "two.sided")$p.value
    }
  }
}

# ---- Determine which bins receive hatching ----
if (hatch_by_percentile) {
  # compute cutoff using non-zero bin counts only (ignore the many zero-count bins)
  nonzero_counts <- dt_allbins$n_runs[dt_allbins$n_runs > 0]
  if (length(nonzero_counts) == 0) {
    cutoff_n <- 0
    message("No non-zero bins found; percentile-based hatching disabled.")
    hatch_mask <- rep(FALSE, nrow(dt_bin))
  } else {
    cutoff_n <- as.numeric(quantile(nonzero_counts, probs = hatch_percentile, na.rm = TRUE))
    message("Hatching lowest ", hatch_percentile * 100, "% of non-zero bins by sample count; cutoff n_runs = ", cutoff_n)
    hatch_mask <- dt_bin$n_runs <= cutoff_n
  }
} else {
  cutoff_n <- hatch_min_n
  message("Hatching non-significant bins with at least ", cutoff_n, " samples (p > ", hatch_p_alpha, ")")
  hatch_mask <- dt_bin$n_runs >= cutoff_n
}
# apply both p-value criterion and sample-count mask
hatch_dt <- dt_bin[is.na(p_value) | p_value > hatch_p_alpha]# | hatch_mask]

# ---- Plot heatmap of zero‐year by bin & duration ----

# Set color scale limits and breaks
center_year <- zero_year_all
range_years <- 10
# fill_limits <- c(center_year - range_years, center_year + range_years)
# fill_breaks <- seq(center_year - range_years, center_year + range_years, by = 2)
fill_breaks <- seq(2076,2098,2)
fill_limits <- c(min(fill_breaks), max(fill_breaks))

# compute midpoints between break boundaries for tick positions and labels
fill_mid <- (head(fill_breaks, -1) + tail(fill_breaks, -1)) / 2
fill_mid_labels <- as.integer(round(fill_mid))  # or format as you prefer

p_bin <- ggplot(dt_bin, aes(x = magnitude, y = duration, fill = zero_year)) +
  geom_tile() +
  scale_x_continuous(
    breaks = seq(-1, 1, by = 0.5),
    labels = function(x) {
      ifelse(abs(x - round(x)) < 1e-8, as.character(as.integer(round(x))), sprintf("%.1f", x))
    }
  ) +
  scale_fill_stepsn(
    colors = brewer.pal(length(fill_breaks) - 1, "PRGn"),
    name   = "Year of net-zero",
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
    title = paste0("Net-Zero Year by Natural Variability Bin and Duration\n",
                   fig_suffix, "\n",
                   "Median Year of Net Zero for All Runs is ", zero_year_all),
    x = "Anomaly magnitude (degC)",
    y = "Anomaly duration (years)"
  ) +
  theme_minimal(base_size=14) +
  theme(
    panel.grid     = element_blank(),
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white", colour = NA)
  ) +
  # Add hatching (diagonal lines) for selected bins
  geom_segment(
    data = hatch_dt,
    aes(
      x = magnitude - half_width,
      xend = magnitude + half_width,
      y = duration - 0.5,
      yend = duration + 0.5
    ),
    color = "black", size = 0.35, alpha = 0.8, inherit.aes = FALSE
  )

# ---- Save figure ----
out_dir <- "../results/heatmaps"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(
  out_dir,
  paste0("netzero_heatmap_bin_wilcox", fig_suffix, ".png")
)
message("Saving: ", out_file)
ggsave(out_file, p_bin, width=8, height=6)


