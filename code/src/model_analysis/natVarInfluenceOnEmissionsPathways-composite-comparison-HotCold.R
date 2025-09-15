library(data.table)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir  <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffixes <- c('_CESM_HR_local_natVar_multiplier1', '_CESM_LM_local_Tambora_2030_normalDistribution', 
                  '_CESM_LM_global_member10_Tambora_2030_normalDistribution_multiplier1', "_CESM_HR_local_natVar_defaultSupporterInitialDistribution")
years     <- 2020:2100

# User option: whether to plot coldest/hottest 10% or just median of all runs
plot_extremes <- FALSE  # Set to FALSE to plot only the median of all runs

# user‐adjustable analysis window and percentile threshold:
analysis_years <- c(2025, 2034)
pct_threshold  <- 0.10

analysis_label <- paste0(analysis_years[1], "-", analysis_years[2])
pct_label      <- paste0(pct_threshold * 100, "pct")
fig_title      <- paste0(
  "Impact of Natural Variability Extremes (", analysis_label, ") at ",
  pct_threshold * 100, "% Threshold\n",
  paste(fig_suffixes, collapse=" vs ")
)

# ---- Load and process datasets ----
all_dt_long_ems <- list()
all_dt_long_temp <- list()
all_dt_long_sup <- list()
all_colors <- c("Coldest 10%"="#0072B2", "Hottest 10%"="#D55E00", "Median of All Runs"="#009E73")

for (suffix in fig_suffixes) {
  # Load data
  ems    <- fread(paste0(data_dir, "emissions",   suffix, ".csv"))
  clim   <- fread(paste0(data_dir, "temperature", suffix, ".csv"))
  natvar <- fread(paste0(data_dir, "natvar",      suffix, ".csv"))
  ems_mat    <- as.matrix(ems)
  clim_mat   <- as.matrix(clim)
  natvar_mat <- as.matrix(natvar)

  if (plot_extremes) {
    # Identify bottom/top 10% runs by avg natvar in analysis window
    idx_range <- which(years >= analysis_years[1] & years <= analysis_years[2])
    avg_nat   <- rowMeans(natvar_mat[, idx_range], na.rm=TRUE)
    q_vals    <- c(pct_threshold, 1 - pct_threshold)
    q_thresh  <- quantile(avg_nat, q_vals, na.rm=TRUE)
    bottom_idx <- which(avg_nat <= q_thresh[1])
    top_idx    <- which(avg_nat >= q_thresh[2])

    # Subsets
    subs <- list(
      "Coldest 10%" = list(ems=ems_mat[bottom_idx, ], temp=clim_mat[bottom_idx, ]),
      "Hottest 10%" = list(ems=ems_mat[top_idx, ],    temp=clim_mat[top_idx, ]),
      "Median of All Runs" = list(ems=ems_mat,        temp=clim_mat)
    )
    plot_groups <- c("Coldest 10%", "Hottest 10%", "Median of All Runs")
  } else {
    # Only median of all runs
    subs <- list(
      "Median of All Runs" = list(ems=ems_mat, temp=clim_mat)
    )
    plot_groups <- c("Median of All Runs")
  }

  # Emissions plot data
  dt_long_ems <- rbindlist(lapply(plot_groups, function(name) {
    mat <- subs[[name]]$ems
    data.table(
      Dataset    = suffix,
      Experiment = name,
      year       = years,
      median     = apply(mat, 2, median, na.rm=TRUE)
    )
  }))
  all_dt_long_ems[[suffix]] <- dt_long_ems

  # Temperature plot data
  dt_long_temp <- rbindlist(lapply(plot_groups, function(name) {
    mat <- subs[[name]]$temp
    data.table(
      Dataset    = suffix,
      Experiment = name,
      year       = years,
      median     = apply(mat, 2, median, na.rm=TRUE)
    )
  }))
  all_dt_long_temp[[suffix]] <- dt_long_temp

  # Climate Supporters plot data
  dist_file <- paste0(data_dir, "distributions", suffix, ".Rdata")
  load(dist_file)   # loads 'dist' array [run, year, group]
  supporters_mat <- dist[,,3]
  if (plot_extremes) {
    subs_sup <- list(
      "Coldest 10%" = list(sup = supporters_mat[bottom_idx, ]),
      "Hottest 10%" = list(sup = supporters_mat[top_idx, ]),
      "Median of All Runs" = list(sup = supporters_mat)
    )
  } else {
    subs_sup <- list(
      "Median of All Runs" = list(sup = supporters_mat)
    )
  }
  dt_long_sup <- rbindlist(lapply(plot_groups, function(name) {
    mat <- subs_sup[[name]]$sup
    data.table(
      Dataset    = suffix,
      Experiment = name,
      year       = years,
      median     = apply(mat, 2, median, na.rm=TRUE)
    )
  }))
  all_dt_long_sup[[suffix]] <- dt_long_sup
}

# Combine datasets for plotting
dt_long_ems <- rbindlist(all_dt_long_ems)
dt_long_temp <- rbindlist(all_dt_long_temp)
dt_long_sup <- rbindlist(all_dt_long_sup)

# ---- Emissions plot ----
p_all_ems <- ggplot(dt_long_ems, aes(year, median, color=Experiment, linetype=Dataset)) +
  geom_line(size=1.1) +
  scale_color_manual(values=all_colors[plot_groups]) +
  labs(x="", y="Emissions (GtC/yr)", title="Median Emissions (GtC/yr)") +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom", legend.box="horizontal")

# ---- Temperature plot ----
p_all_temp <- ggplot(dt_long_temp, aes(year, median, color=Experiment, linetype=Dataset)) +
  geom_line(size=1.1) +
  scale_color_manual(values=all_colors[plot_groups]) +
  labs(x="", y="Temperature (°C)", title="Median Temperature (°C)") +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom", legend.box="horizontal")

# ---- Climate Supporters plot ----
p_all_sup <- ggplot(dt_long_sup, aes(year, median, color=Experiment, linetype=Dataset)) +
  geom_line(size=1.1) +
  scale_color_manual(values=all_colors[plot_groups]) +
  labs(x="Year", y="Fraction of Climate Supporters", title="Median Fraction of Climate Supporters") +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom", legend.box="horizontal")

# ---- Combine plots as a 3-panel figure ----
fig_combined <- p_all_ems / p_all_temp / p_all_sup +
  plot_layout(guides="collect") &
  plot_annotation(title=fig_title) &
  theme(legend.position="bottom", legend.box="horizontal") &
  guides(color=guide_legend(ncol=1), linetype=guide_legend(ncol=1))

ggsave(
  paste0("../results/natvar_extremes_3panel_compare_", analysis_label, "_", pct_label, ".png"),
  fig_combined, width=8, height=15
)
