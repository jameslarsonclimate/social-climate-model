library(data.table)
library(ggplot2)
library(viridis)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir    <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix  <- "_initClimSupportNormalDistribution"
years       <- 2020:2100
start_year  <- 2025
# max_dur     <- length(years) - (start_year - min(years))
max_dur     <- 40 # Maximum duration to consider

threshold   <- 0.34  # Threshold for a "hot year" anomaly
durations   <- 1:max_dur

# ---- Load data ----
message("Loading: ", data_dir, "emissions", fig_suffix, ".csv")
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))

message("Loading: ", data_dir, "natvar", fig_suffix, ".csv")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar", fig_suffix, ".csv")))

# ---- Heat Exposure Index Analysis ----
res <- list()
k   <- 1L

for (dur in durations) {
  print(sprintf("Processing: Duration = %d years", dur))

  end_year <- start_year + dur - 1
  idx_range <- which(years >= start_year & years <= end_year)

  # Calculate the heat exposure index for each simulation:
  # Sum of anomalies above the threshold across the period
  # heat_exposure <- rowSums(
  #   pmax(0, natvar_mat[, idx_range, drop = FALSE] - threshold),
  #   na.rm = TRUE
  # )
  mat <- natvar_mat[, idx_range, drop = FALSE] - threshold
  heat_exposure <- apply(mat, 1, function(x) sum(pmax(x, 0), na.rm = TRUE))

  # Bin simulations into heat exposure deciles (or choose your own binning)
# n_bins <- 10
# min_ex <- min(heat_exposure, na.rm=TRUE)
# max_ex <- max(heat_exposure, na.rm=TRUE)

# # create 11 break‐points from min to max
# my_breaks <- seq(min_ex, max_ex, length.out = n_bins + 1)

my_breaks <- 0:10

# now cut into equal‐width bins
bins <- cut(
  heat_exposure,
  breaks = my_breaks,
  include.lowest = TRUE,
  labels = FALSE
)

  for (bin in sort(unique(bins))) {
    idx <- which(bins == bin)
    if (length(idx) == 0) {
      zero_year <- NA_integer_
    } else {
      med_traj  <- apply(ems_mat[idx, , drop = FALSE], 2, median, na.rm = TRUE)
      zz        <- which(med_traj <= 0)
      zero_year <- if (length(zz) > 0) years[min(zz)] else NA_integer_
    }

    res[[k]] <- list(
      duration     = dur,
      exposure_bin = bin,
      zero_year    = zero_year
    )
    k <- k + 1L
  }
}

dt_hex <- rbindlist(res)[!is.na(zero_year)]

# ---- Plot heatmap of net-zero year by exposure bin and duration ----
p_hex <- ggplot(dt_hex, aes(x = exposure_bin, y = duration, fill = zero_year)) +
  geom_tile() +
    scale_x_continuous(
      name   = "Cumulative Heat Exposure (°C)",
      breaks = 1:n_bins,
      labels = sprintf("%.2f", my_breaks[1:n_bins]),
      expand = c(0,0)
    ) +

  scale_fill_viridis(name = "Year Net-Zero", na.value = "grey80", option = "C",
    limits = c(2084, 2091),
    oob = scales::oob_squish  # Allow colors outside the range to be squished

) +
  labs(
    title = paste0("Net-Zero Year by Heat Exposure Bin and Duration\n", fig_suffix),
    y = "Duration (yrs)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  guides(fill = guide_colorbar(
    barwidth = unit(6, "cm"),
    barheight = unit(0.5, "cm"))
  )

# ---- Save heatmap ----
out_dir <- "../results/heatmaps"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, paste0("netzero_heatmap_hex", fig_suffix, ".png"))
message("Saving: ", out_file)
ggsave(out_file, p_hex, width = 8, height = 6)
