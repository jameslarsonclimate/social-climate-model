library(data.table)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir       <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix     <- "_initClimSupport40percent"
# fig_suffix <- ""
years          <- 2020:2100

# user‐adjustable analysis window and percentile threshold
analysis_years <- c(2020, 2039)
pct_threshold  <- 0.10  # e.g. 0.10 for 10%, 0.05 for 5%

# labels for titles and filenames
analysis_label <- paste0(analysis_years[1], "-", analysis_years[2])
pct_label      <- paste0(pct_threshold * 100, "pct")
fig_title_base <- paste0(
  "Variance-Based Composite: Natural Variability (", analysis_label, 
  ") at ", pct_label
)

# ---- Load data ----
ems_df    <- fread(paste0(data_dir, "emissions",   fig_suffix, ".csv"))
temp_df   <- fread(paste0(data_dir, "temperature", fig_suffix, ".csv"))
natvar_df <- fread(paste0(data_dir, "natvar",      fig_suffix, ".csv"))

ems_mat    <- as.matrix(ems_df)
temp_mat   <- as.matrix(temp_df)
natvar_mat <- as.matrix(natvar_df)

# ---- Identify bottom/top runs by natvar standard deviation ----
idx_range <- which(years >= analysis_years[1] & years <= analysis_years[2])
sd_nat    <- apply(natvar_mat[, idx_range, drop=FALSE], 1, sd, na.rm=TRUE)
q_vals    <- c(pct_threshold, 1 - pct_threshold)
q_thresh  <- quantile(sd_nat, q_vals, na.rm=TRUE)

bottom_idx <- which(sd_nat <= q_thresh[1])
top_idx    <- which(sd_nat >= q_thresh[2])

# ---- Build subsets ----
subs <- list(
  "Bottom 10%" = list(ems = ems_mat[bottom_idx, , drop=FALSE],
                      temp = temp_mat[bottom_idx, , drop=FALSE]),
  "Top 10%"    = list(ems = ems_mat[top_idx,    , drop=FALSE],
                      temp = temp_mat[top_idx,    , drop=FALSE]),
  "Default"    = list(ems = ems_mat,   temp = temp_mat)
)

# color palette
colors <- c("Bottom 10%"="#0072B2","Top 10%"="#D55E00","Default"="#009E73")

# ---- Composite: Emissions ----
dt_ems <- rbindlist(lapply(names(subs), function(x) {
  mat <- subs[[x]]$ems
  data.table(
    Experiment = x,
    year       = years,
    median     = apply(mat, 2, median,   na.rm=TRUE),
    q05        = apply(mat, 2, quantile, probs=0.05, na.rm=TRUE),
    q95        = apply(mat, 2, quantile, probs=0.95, na.rm=TRUE)
  )
}))

# difference vs Default
base_ems   <- dt_ems[Experiment=="Default", .(year, median_def=median)]
dt_diff_ems <- merge(
  dt_ems[Experiment!="Default"], base_ems, by="year"
)[, median_diff := median - median_def]

p1 <- ggplot(dt_ems, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1) +
  # geom_ribbon(aes(ymin=q05, ymax=q95), alpha=0.2) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(title = paste0(fig_title_base, " — Emissions"),
       y = "Emissions (GtC/yr)", x = NULL) +
  theme_minimal()

p2 <- ggplot(dt_diff_ems, aes(year, median_diff, color=Experiment)) +
  geom_line(size=1, show.legend=FALSE) +
  scale_color_manual(values=colors) +
  labs(title = "Difference vs Default: Emissions",
       y = "Δ Emissions (GtC/yr)", x = "Year") +
  theme_minimal()

fig_ems <- p1 / p2 + 
  plot_annotation(title = fig_title_base) &
  theme(legend.position="bottom")

ggsave(
  paste0("../results/composites/variance_emissions_", fig_suffix, "_", 
         analysis_label, "_", pct_label, ".png"),
  fig_ems, width=8, height=10
)

# ---- Composite: Temperature ----
dt_temp <- rbindlist(lapply(names(subs), function(x) {
  mat <- subs[[x]]$temp
  data.table(
    Experiment = x,
    year       = years,
    median     = apply(mat, 2, median,   na.rm=TRUE),
    q05        = apply(mat, 2, quantile, probs=0.05, na.rm=TRUE),
    q95        = apply(mat, 2, quantile, probs=0.95, na.rm=TRUE)
  )
}))

base_temp    <- dt_temp[Experiment=="Default", .(year, median_def=median)]
dt_diff_temp <- merge(
  dt_temp[Experiment!="Default"], base_temp, by="year"
)[, median_diff := median - median_def]

p3 <- ggplot(dt_temp, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1) +
  # geom_ribbon(aes(ymin=q05, ymax=q95), alpha=0.2) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(title = paste0(fig_title_base, " — Temperature"),
       y = "Temperature (°C)", x = NULL) +
  theme_minimal()

p4 <- ggplot(dt_diff_temp, aes(year, median_diff, color=Experiment)) +
  geom_line(size=1, show.legend=FALSE) +
  scale_color_manual(values=colors) +
  labs(title = "Difference vs Default: Temperature",
       y = "Δ Temperature (°C)", x = "Year") +
  theme_minimal()

fig_temp <- p3 / p4 + 
  plot_annotation(title = fig_title_base) &
  theme(legend.position="bottom")

ggsave(
  paste0("../results/composites/variance_temperature_", fig_suffix, "_", 
         analysis_label, "_", pct_label, ".png"),
  fig_temp, width=8, height=10
)