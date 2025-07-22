library(data.table)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix    <- "_initClimSupportNormalDistribution" #-natVarMultiplier10"
# fig_suffix = '_ERA5natVar'
years         <- 2020:2100
start_year    <- 2025
# max_dur       <- length(years) - (start_year - min(years))
max_dur       <- 20
bin_centers   <- seq(-1.5, 1.5, by=0.1)
half_width    <- 0.05

# ---- Load data ----
# message("Loading: ", data_dir, "emissions", fig_suffix, ".csv")
# ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
# message("Loading: ", data_dir, "natvar", fig_suffix, ".csv")
# natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Load and append data from all suffixes ----

# print reading messages and the suffixes being loaded

fig_suffixes  <- c(
  "_initClimSupportNormalDistribution",
  "_initClimSupportNormalDistribution-resample",
  "_initClimSupportNormalDistribution-resample2",
  "_initClimSupportNormalDistribution-resample3"
)

message("Loading data for suffixes: \n", paste(fig_suffixes, collapse=",\n"))

ems_list    <- list()
natvar_list <- list()
for (suffix in fig_suffixes) {
  message("Loading: ", data_dir, "emissions", suffix, ".csv")
  ems_list[[suffix]]    <- as.matrix(fread(paste0(data_dir, "emissions", suffix, ".csv")))
  message("Loading: ", data_dir, "natvar", suffix, ".csv")
  natvar_list[[suffix]] <- as.matrix(fread(paste0(data_dir, "natvar", suffix, ".csv")))
}
ems_mat    <- do.call(rbind, ems_list)
natvar_mat <- do.call(rbind, natvar_list)

message("Data loaded successfully.")
fig_suffix    <- "_initClimSupportNormalDistribution-resampleAppended" # Update suffix for output


# ---- Heatmap: Median of Net-Zero Years for Each Run in Each Bin ----

# For each run, find the net-zero year (first year emissions <= 0)
run_zero_years <- apply(ems_mat, 1, function(x) {
  zz <- which(x <= 0)
  if (length(zz) > 0) years[min(zz)] else NA_integer_
})

# Prepare binning results
res_median <- list()
k <- 1L
for (dur in 1:max_dur) {
  end_year <- start_year + dur - 1
  idx_range <- which(years >= start_year & years <= end_year)
  avg_nat   <- rowMeans(natvar_mat[, idx_range, drop=FALSE], na.rm=TRUE)
  for (mag in bin_centers) {
    lo <- mag - half_width
    hi <- mag + half_width
    idx <- which(avg_nat >= lo & avg_nat < hi)
    if (length(idx) == 0) {
      median_zero_year <- NA_integer_
    } else {
      # Use the net-zero year for each run in the bin
      bin_zero_years <- run_zero_years[idx]
      bin_zero_years <- bin_zero_years[!is.na(bin_zero_years)]
      median_zero_year <- if (length(bin_zero_years) > 0) median(bin_zero_years) else NA_integer_
    }
    res_median[[k]] <- list(
      duration   = dur,
      magnitude  = mag,
      median_zero_year = median_zero_year
    )
    k <- k + 1L
  }
}
dt_bin_median <- rbindlist(res_median)[!is.na(median_zero_year)]

# Compute overall population median of net-zero years
overall_median_zero_year <- median(run_zero_years[!is.na(run_zero_years)])

# Set color scale limits and breaks
center_year_median <- overall_median_zero_year
range_years_median <- 10
fill_breaks_median <- seq(center_year_median - range_years_median, center_year_median + range_years_median, by = 2)
fill_limits_median <- c(min(fill_breaks_median), max(fill_breaks_median))
fill_breaks <- seq(overall_median_zero_year-11, overall_median_zero_year+11,2)
fill_limits <- c(min(fill_breaks), max(fill_breaks))


# Plot heatmap
p_bin_median <- ggplot(dt_bin_median, aes(x = magnitude, y = duration, fill = median_zero_year)) +
  geom_tile() +
  scale_fill_stepsn(
    colors = brewer.pal(length(fill_breaks) - 1, "PRGn"),
    name   = "Median Net-Zero Year\n(of Run Net-Zero Years)",
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
    title = paste0("Median of Run Net-Zero Years by Natural Variability Bin and Duration\n",
                   fig_suffix, "\n",
                   "Overall Median Net-Zero Year (all runs): ", overall_median_zero_year),
    x = "Average Natural Variability Magnitude (degC)",
    y = "Duration (yrs)"
  ) +
  theme_minimal(base_size=14) +
  theme(
    panel.grid     = element_blank(),
    legend.position = "bottom"
  )

# ---- Save figure ----
out_file_median <- file.path(
  out_dir,
  paste0("netzero_heatmap_bin_medianOfRunNetZeroYears", fig_suffix, ".png")
)
message("Saving median-of-run-net-zero-years heatmap to: ", out_file_median)
ggsave(out_file_median, p_bin_median, width=8, height=6)

