library(data.table)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir    <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix  <- '_initClimSupportNormalDistribution'
years       <- 2020:2100

# ---- Load data ----
ems    <- fread(paste0(data_dir, "emissions",   fig_suffix, ".csv"))
clim   <- fread(paste0(data_dir, "temperature", fig_suffix, ".csv"))
dist   <- get(load(paste0(data_dir, "distributions", fig_suffix, ".Rdata")))
# 'dist' array [run, year, group]; group 3 == supporters

ems_mat    <- as.matrix(ems)
clim_mat   <- as.matrix(clim)
sup_mat    <- dist[,,3]

# ---- Identify Net-Zero vs Non-Zero runs ----
ems_vec   <- rowSums(ems_mat, na.rm=TRUE)
final_ems <- ems_mat[, ncol(ems_mat)]
zero_idx     <- which(final_ems <= 0)
positive_idx <- which(final_ems >  0)

# ---- Subsets ----
subs <- list(
  "Zero by Year End"     = list(ems = ems_mat[zero_idx, ],    temp = clim_mat[zero_idx, ],    sup = sup_mat[zero_idx, ]),
  "Positive at Year End" = list(ems = ems_mat[positive_idx, ], temp = clim_mat[positive_idx, ], sup = sup_mat[positive_idx, ]),
  "All Runs"             = list(ems = ems_mat,                temp = clim_mat,                sup = sup_mat)
)

colors   <- c("Zero by Year End"="#0072B2", "Positive at Year End"="#D55E00", "All Runs"="#009E73")
fig_title <- "Composite: Net‐Zero vs Non‐Zero Emissions by Year End"

# ---- Helper to build long tables ----
make_dt <- function(mat, label) {
  data.table(
    Experiment = label,
    year       = years,
    median     = apply(mat, 2, median,   na.rm=TRUE),
    q05        = apply(mat, 2, quantile, probs=0.05, na.rm=TRUE),
    q95        = apply(mat, 2, quantile, probs=0.95, na.rm=TRUE)
  )
}

# ---- Build & plot Emissions ----
dt_ems      <- rbindlist(lapply(names(subs), function(n) make_dt(subs[[n]]$ems, n)))
dt_diff_ems <- merge(
  dt_ems[Experiment != "All Runs", .(Experiment, year, median)],
  dt_ems[Experiment == "All Runs", .(year, median_default = median)],
  by = "year"
)[, median_diff := median - median_default]

p_all_ems  <- ggplot(dt_ems,      aes(year, median,      color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(y="Emissions (GtC/yr)", x="", title="Emissions: Median & 5–95% Quantiles") +
  theme_minimal(base_size=14)

p_diff_ems <- ggplot(dt_diff_ems, aes(year, median_diff, color=Experiment)) +
  geom_line(size=1.1, show.legend=FALSE) +
  scale_color_manual(values=colors) +
  labs(y="Δ Emissions (GtC/yr)", x="Year", title="Difference vs All Runs") +
  theme_minimal(base_size=14)

fig_ems <- (p_all_ems / p_diff_ems) +
  plot_layout(guides="collect") &
  plot_annotation(title=fig_title) &
  theme(legend.position="bottom", legend.box="horizontal") &
  guides(color=guide_legend(ncol=3), fill=guide_legend(ncol=3))

ggsave(paste0("../results/composites/netzero_emissions", fig_suffix, ".png"),
       fig_ems, width=8, height=10)

# ---- Build & plot Temperature ----
dt_temp      <- rbindlist(lapply(names(subs), function(n) make_dt(subs[[n]]$temp, n)))
dt_diff_temp <- merge(
  dt_temp[Experiment != "All Runs", .(Experiment, year, median)],
  dt_temp[Experiment == "All Runs", .(year, median_default = median)],
  by = "year"
)[, median_diff := median - median_default]

p_all_temp  <- ggplot(dt_temp,      aes(year, median,      color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(y="Temperature (°C)", x="", title="Temperature: Median & 5–95% Quantiles") +
  theme_minimal(base_size=14)

p_diff_temp <- ggplot(dt_diff_temp, aes(year, median_diff, color=Experiment)) +
  geom_line(size=1.1, show.legend=FALSE) +
  scale_color_manual(values=colors) +
  labs(y="Δ Temperature (°C)", x="Year", title="Difference vs All Runs") +
  theme_minimal(base_size=14)

fig_temp <- (p_all_temp / p_diff_temp) +
  plot_layout(guides="collect") &
  plot_annotation(title=fig_title) &
  theme(legend.position="bottom", legend.box="horizontal") &
  guides(color=guide_legend(ncol=3), fill=guide_legend(ncol=3))

ggsave(paste0("../results/composites/netzero_temperature", fig_suffix, ".png"),
       fig_temp, width=8, height=10)

# ---- Build & plot Supporters ----
dt_sup      <- rbindlist(lapply(names(subs), function(n) make_dt(subs[[n]]$sup, n)))
dt_diff_sup <- merge(
  dt_sup[Experiment != "All Runs", .(Experiment, year, median)],
  dt_sup[Experiment == "All Runs", .(year, median_default = median)],
  by = "year"
)[, median_diff := median - median_default]

p_all_sup  <- ggplot(dt_sup,      aes(year, median,      color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(y="Fraction of Supporters", x="", title="Supporters: Median & 5–95% Quantiles") +
  theme_minimal(base_size=14)

p_diff_sup <- ggplot(dt_diff_sup, aes(year, median_diff, color=Experiment)) +
  geom_line(size=1.1, show.legend=FALSE) +
  scale_color_manual(values=colors) +
  labs(y="Δ Fraction", x="Year", title="Difference vs All Runs") +
  theme_minimal(base_size=14)

fig_sup <- (p_all_sup / p_diff_sup) +
  plot_layout(guides="collect") &
  plot_annotation(title=fig_title) &
  theme(legend.position="bottom", legend.box="horizontal") &
  guides(color=guide_legend(ncol=3), fill=guide_legend(ncol=3))

ggsave(paste0("../results/composites/netzero_supporters", fig_suffix, ".png"),
       fig_sup, width=8, height=10)