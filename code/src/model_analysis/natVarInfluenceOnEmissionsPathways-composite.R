library(data.table)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# ---- Load data ----
data_dir <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix <- ""  # or your suffix

ems    <- fread(paste0(data_dir, "emissions",    fig_suffix, ".csv"))
clim   <- fread(paste0(data_dir, "temperature",  fig_suffix, ".csv"))
natvar <- fread(paste0(data_dir, "natvar",       fig_suffix, ".csv"))

# to matrices
ems_mat    <- as.matrix(ems)
clim_mat   <- as.matrix(clim)
natvar_mat <- as.matrix(natvar)

years <- 2020:2100

# ---- Identify bottom/top 10% runs by average natvar in 2030–2039 ----
idx_range <- 11:20   # columns for 2030–2039
avg_nat   <- rowMeans(natvar_mat[, idx_range], na.rm = TRUE)
q10_90    <- quantile(avg_nat, c(0.1, 0.9), na.rm = TRUE)

bottom_idx <- which(avg_nat <=  q10_90[1])
top_idx    <- which(avg_nat >=  q10_90[2])

# # save indexes
# fwrite(data.table(Index = bottom_idx), "../results/bottom10_natvar_idx.csv")
# fwrite(data.table(Index = top_idx),    "../results/top10_natvar_idx.csv")

# ---- Subsets list ----
subs <- list(
  "Bottom 10%" = list(ems = ems_mat[bottom_idx, ], temp = clim_mat[bottom_idx, ]),
  "Top 10%"    = list(ems = ems_mat[top_idx, ],    temp = clim_mat[top_idx, ])
)

# ---- Function to build two‐panel quantile + diff plot ----
make_two_panel <- function(var_list, y_label, diff_label_unit) {
  # prepare long format
  dt_long <- rbindlist(lapply(names(var_list), function(name) {
    mat <- var_list[[name]]
    data.table(
      Experiment = name,
      year       = years,
      median     = apply(mat, 2, median,   na.rm = TRUE),
      q05        = apply(mat, 2, quantile, probs = 0.05, na.rm = TRUE),
      q95        = apply(mat, 2, quantile, probs = 0.95, na.rm = TRUE)
    )
  }))
  # baseline = Bottom 10%
  base <- dt_long[Experiment == "Bottom 10%", .(year, median, q05, q95)]
  dt_diff <- merge(dt_long, base, by="year", suffixes=c("", "_base"))
  dt_diff[, `:=`(
    median_diff = median - median_base,
    q05_diff    = q05    - q05_base,
    q95_diff    = q95    - q95_base
  )]
  dt_diff <- dt_diff[Experiment != "Bottom 10%"]
  
  colors  <- setNames(brewer.pal(2, "Set1"), names(var_list))
  
  p_all <- ggplot(dt_long, aes(year, median, color=Experiment, fill=Experiment)) +
    geom_ribbon(aes(ymin=q05, ymax=q95), alpha=0.2, color=NA) +
    geom_line(size=1.1) +
    scale_color_manual(values=colors) +
    scale_fill_manual(values=colors) +
    labs(x="", y=y_label, title=paste0("Median & 5–95% Quantiles: ", y_label)) +
    theme_minimal(base_size=14)
  
  p_diff <- ggplot(dt_diff, aes(year, median_diff, color=Experiment, fill=Experiment)) +
    geom_ribbon(aes(ymin=q05_diff, ymax=q95_diff), alpha=0.2, color=NA) +
    geom_line(size=1.1) +
    scale_color_manual(values=colors[-1]) +
    scale_fill_manual(values=colors[-1]) +
    labs(x="Year",
         y=paste0("Diff vs Bottom 10% (", diff_label_unit, ")"),
         title=paste0("Differences vs Bottom 10%: ", y_label)) +
    theme_minimal(base_size=14)
  
  return((p_all / p_diff) +
           plot_layout(guides="collect") &
           theme(legend.position="bottom", legend.box="horizontal") &
           guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=2)))
}

# ---- Create & save plots ----
fig_ems  <- make_two_panel(lapply(subs, `[[`, "ems"),  "Emissions (GtC/yr)", "GtC/yr")
fig_temp <- make_two_panel(lapply(subs, `[[`, "temp"), "Temperature (°C)",   "°C")

ggsave("../results/emissions_natvar_extremes.pdf", fig_ems,  width=8, height=10)
ggsave("../results/temperature_natvar_extremes.pdf", fig_temp, width=8, height=10)