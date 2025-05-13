library(data.table)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir  <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix <- "_varyInitialDistribution"
years     <- 2020:2100

# ---- Load data ----
ems    <- fread(paste0(data_dir, "emissions",   fig_suffix, ".csv"))
clim   <- fread(paste0(data_dir, "temperature", fig_suffix, ".csv"))
natvar <- fread(paste0(data_dir, "natvar",      fig_suffix, ".csv"))

ems_mat    <- as.matrix(ems)
clim_mat   <- as.matrix(clim)
natvar_mat <- as.matrix(natvar)

# ---- Identify bottom/top 10% runs by avg natvar in 2030–2039 ----
idx_range <- 11:20
avg_nat   <- rowMeans(natvar_mat[, idx_range], na.rm=TRUE)
q10_90    <- quantile(avg_nat, c(0.1, 0.9), na.rm=TRUE)
bottom_idx <- which(avg_nat <=  q10_90[1])
top_idx    <- which(avg_nat >=  q10_90[2])

# ---- Subsets ----
subs <- list(
  "Bottom 10%" = list(ems=ems_mat[bottom_idx, ], temp=clim_mat[bottom_idx, ]),
  "Top 10%"    = list(ems=ems_mat[top_idx, ],    temp=clim_mat[top_idx, ]),
  "Default"    = list(ems=ems_mat,               temp=clim_mat)
)

# ---- Common settings ----
colors   <- c("Bottom 10%"="#0072B2","Top 10%"="#D55E00","Default"="#009E73")
fig_title <- paste0(
  "Impact of Natural Variability Extremes (2030–2039) on Pathways:\n",
  "Comparing Top and Bottom 10% of Runs by Avg. Natural Variability\n",
  fig_suffix
)

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

# ---- Differences relative to Default median ----
# 1. pull out the Default median by year
base_default <- dt_long_ems[Experiment == "Default", .(year, median_default = median)]

# 2. merge only Bottom and Top runs with the Default baseline
dt_diff_ems <- merge(
  dt_long_ems[Experiment %in% c("Bottom 10%", "Top 10%")],
  base_default,
  by = "year"
)

# 3. compute the median difference
dt_diff_ems[, median_diff := median - median_default]

p_all_ems <- ggplot(dt_long_ems, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Emissions (GtC/yr)", title="Median & 5-95% Quantiles: Emissions (GtC/yr)") +
  theme_minimal(base_size=14)

p_diff_ems <- ggplot(dt_diff_ems, aes(year, median_diff, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors["Top 10%"]) +
  scale_fill_manual(values=colors["Top 10%"]) +
  labs(
    x="Year",
    y="Diff vs Bottom 10% (GtC/yr)",
    title=paste0(
      "Differences vs Bottom 10%: Emissions (GtC/yr)\n",
      "Cumulative emissions difference (hot decade): ",
      round(sum(dt_diff_ems[Experiment == "Top 10%"]$median_diff),2)," GtC"
    )
  ) +
  theme_minimal(base_size=14)

fig_ems <- (p_all_ems / p_diff_ems) +
  plot_layout(guides="collect") &
  plot_annotation(title=fig_title) &
  theme(legend.position="bottom", legend.box="horizontal") &
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2))

ggsave(paste0("../results/emissions_natvar_extremes", fig_suffix, ".png"), fig_ems, width=8, height=10)


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

# ---- Differences relative to Default median for Temperature ----
# 1. pull out the Default median by year
base_default_temp <- dt_long_temp[Experiment == "Default", .(year, median_default = median)]

# 2. merge only Bottom and Top runs with the Default baseline
dt_diff_temp <- merge(
  dt_long_temp[Experiment %in% c("Bottom 10%", "Top 10%")],
  base_default_temp,
  by = "year"
)

# 3. compute the median difference
dt_diff_temp[, median_diff := median - median_default]

# (you can drop q05/q95 columns here if you only need the median_diff)
p_all_temp <- ggplot(dt_long_temp, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Temperature (°C)", title="Median & 5-95% Quantiles: Temperature (°C)") +
  theme_minimal(base_size=14)

p_diff_temp <- ggplot(dt_diff_temp, aes(year, median_diff, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors["Top 10%"]) +
  scale_fill_manual(values=colors["Top 10%"]) +
  labs(
    x="Year",
    y="Diff vs Bottom 10% (°C)",
    title=paste0(
      "Differences vs Bottom 10%: Temperature (°C)"
    )
  ) +
  theme_minimal(base_size=14)

fig_temp <- (p_all_temp / p_diff_temp) +
  plot_layout(guides="collect") &
  plot_annotation(title=fig_title) &
  theme(legend.position="bottom", legend.box="horizontal") &
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2))

ggsave(paste0("../results/temperature_natvar_extremes", fig_suffix, ".png"), fig_temp, width=8, height=10)


# ---- Climate Supporters plot ----
dist_file <- paste0(data_dir, "distributions", fig_suffix, ".Rdata")
load(dist_file)   # loads 'dist' array [run, year, group]
supporters_mat <- dist[,,3]

subs_sup <- list(
  "Bottom 10%" = list(sup = supporters_mat[bottom_idx, ]),
  "Top 10%"    = list(sup = supporters_mat[top_idx, ]),
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

# ---- Differences relative to Default median for Supporters ----
# 1. pull out the Default median by year
base_default_sup <- dt_long_sup[Experiment == "Default", .(year, median_default = median)]

# 2. merge only Bottom and Top runs with the Default baseline
dt_diff_sup <- merge(
  dt_long_sup[Experiment %in% c("Bottom 10%", "Top 10%")],
  base_default_sup,
  by = "year"
)

# 3. compute the median difference
dt_diff_sup[, median_diff := median - median_default]

p_all_sup <- ggplot(dt_long_sup, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Fraction of Climate Supporters", title="Median & 5-95% Quantiles: Fraction of Climate Supporters") +
  theme_minimal(base_size=14)

p_diff_sup <- ggplot(dt_diff_sup, aes(year, median_diff, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors["Top 10%"]) +
  scale_fill_manual(values=colors["Top 10%"]) +
  labs(
    x="Year",
    y="Diff vs Bottom 10% (Fraction)",
    title=paste0(
      "Differences vs Bottom 10%: Fraction of Climate Supporters"
    )
  ) +
  theme_minimal(base_size=14)

fig_sup <- (p_all_sup / p_diff_sup) +
  plot_layout(guides="collect") &
  plot_annotation(title=fig_title) &
  theme(legend.position="bottom", legend.box="horizontal") &
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2))

ggsave(paste0("../results/supporters_natvar_extremes", fig_suffix, ".png"), fig_sup, width=8, height=10)