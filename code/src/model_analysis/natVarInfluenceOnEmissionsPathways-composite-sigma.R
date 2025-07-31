library(data.table)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir  <- "../results/MC Runs/MC Runs_TunedParams/"
# fig_suffix <- ""
# fig_suffix = '_initClimSupportNormalDistribution'
# fig_suffix = 'volcanicCooling_2030_-1_seed2090'  # Change the seed!
fig_suffix = '_CESM_HR_local_natVar_multiplier1'
# fig_suffix = '_CESM_HR_local_natVar_500000runs'
# fig_suffix = '_CESM_HR_local_natVar_multiplier05'
# fig_suffix = '_ERA5natVar'
# fig_suffix = '_ERA5natVar0.5'
# fig_suffix = '_CESM_HR_local_natVar_scale_sd'


years     <- 2020:2100

# user‐adjustable analysis window and percentile threshold:
analysis_years <- c(2025, 2034)   # e.g. c(2030, 2039) or c(2070, 2079)
pct_threshold  <- 0.10            # e.g. 0.10 for 10%, 0.05 for 5%

# dynamically build labels for titles and file names:
analysis_label <- paste0(analysis_years[1], "-", analysis_years[2])
pct_label      <- paste0(pct_threshold * 100, "pct")
fig_title      <- paste0(
  "Impact of Natural Variability Extremes (", analysis_label, ") at ",
  pct_threshold * 100, "% Threshold of Natural Variability\n",
  fig_suffix
)

# ---- Load data ----
ems    <- fread(paste0(data_dir, "emissions",   fig_suffix, ".csv"))
clim   <- fread(paste0(data_dir, "temperature", fig_suffix, ".csv"))
natvar <- fread(paste0(data_dir, "natvar",      fig_suffix, ".csv"))

ems_mat    <- as.matrix(ems)
clim_mat   <- as.matrix(clim)
natvar_mat <- as.matrix(natvar)

# ---- Standardize natvar_mat by row (across time) ----
row_sd <- apply(natvar_mat, 1, sd, na.rm=TRUE)
natvar_mat_sigma <- natvar_mat / row_sd

# ---- Identify bottom/top 10% runs by avg natvar in 2030–2039 ----
idx_range <- which(years >= analysis_years[1] & years <= analysis_years[2])
avg_nat   <- rowMeans(natvar_mat_sigma[, idx_range], na.rm=TRUE)
q_vals    <- c(pct_threshold, 1 - pct_threshold)
q_thresh  <- quantile(avg_nat, q_vals, na.rm=TRUE)
bottom_idx <- which(avg_nat <= q_thresh[1])
top_idx    <- which(avg_nat >= q_thresh[2])

# ---- Subsets ----
subs <- list(
  "Coldest 10%" = list(ems=ems_mat[bottom_idx, ], temp=clim_mat[bottom_idx, ]),
  "Hottest 10%"    = list(ems=ems_mat[top_idx, ],    temp=clim_mat[top_idx, ]),
  "Default"    = list(ems=ems_mat,               temp=clim_mat)
)

colors   <- c("Coldest 10%"="#0072B2", "Hottest 10%"="#D55E00","Default"="#009E73")

# ---- Emissions plot ----
dt_long_ems <- rbindlist(lapply(names(subs), function(name) {
  mat <- subs[[name]]$ems
  data.table(
    Experiment = name,
    year       = years,
    median     = apply(mat, 2, median,   na.rm=TRUE),
    q05        = apply(mat, 2, quantile, probs=0.05, na.rm=TRUE),
    q95        = apply(mat, 2, quantile, probs=0.95, na.rm=TRUE)
  )
}))

p_all_ems <- ggplot(dt_long_ems, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Emissions (GtC/yr)", title="Median Emissions (GtC/yr)") +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom", legend.box="horizontal")

# ---- Temperature plot ----
dt_long_temp <- rbindlist(lapply(names(subs), function(name) {
  mat <- subs[[name]]$temp
  data.table(
    Experiment = name,
    year       = years,
    median     = apply(mat, 2, median,   na.rm=TRUE),
    q05        = apply(mat, 2, quantile, probs=0.05, na.rm=TRUE),
    q95        = apply(mat, 2, quantile, probs=0.95, na.rm=TRUE)
  )
}))

p_all_temp <- ggplot(dt_long_temp, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Temperature (°C)", title="Median Temperature (°C)") +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom", legend.box="horizontal")

# ---- Climate Supporters plot ----
dist_file <- paste0(data_dir, "distributions", fig_suffix, ".Rdata")
load(dist_file)   # loads 'dist' array [run, year, group]
supporters_mat <- dist[,,3]

subs_sup <- list(
  "Coldest 10%" = list(sup = supporters_mat[bottom_idx, ]),
  "Hottest 10%"    = list(sup = supporters_mat[top_idx, ]),
  "Default"    = list(sup = supporters_mat)
)

dt_long_sup <- rbindlist(lapply(names(subs_sup), function(name) {
  mat <- subs_sup[[name]]$sup
  data.table(
    Experiment = name,
    year       = years,
    median     = apply(mat, 2, median,   na.rm=TRUE),
    q05        = apply(mat, 2, quantile, probs=0.05, na.rm=TRUE),
    q95        = apply(mat, 2, quantile, probs=0.95, na.rm=TRUE)
  )
}))

p_all_sup <- ggplot(dt_long_sup, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="Year", y="Fraction of Climate Supporters", title="Median Fraction of Climate Supporters") +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom", legend.box="horizontal")

# ---- Combine plots as a 3-panel figure ----
fig_combined <- p_all_ems / p_all_temp / p_all_sup +
  plot_layout(guides="collect") &
  plot_annotation(title=fig_title) &
  theme(legend.position="bottom", legend.box="horizontal") &
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2))

ggsave(
  paste0("../results/natvar_sigma_extremes_3panel", fig_suffix, "_", analysis_label, "_", pct_label, ".png"),
  fig_combined, width=8, height=15
)
