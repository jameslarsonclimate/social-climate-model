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


# ---- Loop over duration & magnitude bins ----
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
    if (length(idx)==0) {
      zero_year <- NA_integer_
    } else {
      med_traj  <- apply(ems_mat[idx, , drop=FALSE], 2, median, na.rm=TRUE)
      zz        <- which(med_traj <= 0)
      zero_year <- if (length(zz)>0) years[min(zz)] else NA_integer_
    }
    res[[k]] <- list(
      duration   = dur,
      magnitude  = mag,
      zero_year  = zero_year
    )
    k <- k + 1L
  }
}

dt_bin <- rbindlist(res)[!is.na(zero_year)]

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

# ---- Plot heatmap of zero‐year by bin & duration ----

# Set color scale limits and breaks
center_year <- zero_year_all
range_years <- 10
# fill_limits <- c(center_year - range_years, center_year + range_years)
# fill_breaks <- seq(center_year - range_years, center_year + range_years, by = 2)
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
    title = paste0("Net-Zero Year by Natural Variability Bin and Duration\n",
                   fig_suffix, "\n",
                   "Median Year of Net Zero for All Runs is ", zero_year_all),
    x = "Average Natural Variability Magnitude (degC)",
    y = "Duration (yrs)"
  ) +
  theme_minimal(base_size=14) +
  theme(
    panel.grid     = element_blank(),
    legend.position = "bottom"
  ) +
  # Add hatching (diagonal lines) for non-significant bins
  geom_segment(
    data = dt_bin[is.na(p_value) | p_value > 0.05],
    aes(
      x = magnitude - half_width,
      xend = magnitude + half_width,
      y = duration - 0.5,
      yend = duration + 0.5
    ),
    color = "black", size = 0.45, alpha = 0.8, inherit.aes = FALSE
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


