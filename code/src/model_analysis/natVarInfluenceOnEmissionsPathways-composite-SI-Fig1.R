library(data.table)
library(ggplot2)
library(patchwork)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir    <- "../results/MC Runs/MC Runs_TunedParams/"
years       <- 2020:2100
analysis_years <- c(2025, 2034)
pct_threshold  <- 0.10

# Specify experiments (one or more suffixes). Provide matching friendly labels if desired.
fig_suffixes <- c(
  '',
  "_initClimSupportNormalDistribution"
)
exp_labels <- c(
  "Default Parameters",
  "Updated Initialization of Climate Supporters"
)
# linetypes (one per experiment)
linestyles <- c("longdash", "solid", "dotted", "dotdash", "twodash", "dashed")
linestyles <- linestyles[seq_len(length(fig_suffixes))]
names(linestyles) <- exp_labels

# color scheme (same across experiments) for groups
group_levels <- c("Coldest 10%", "Median of All Runs", "Hottest 10%")
group_colors <- c("Coldest 10%" = "#005AB5", "Median of All Runs" = "#000000", "Hottest 10%" = "#DC3220") 

# ---- Load and process each experiment ----
dt_sup_list <- list()
dt_ems_list <- list()

for (i in seq_along(fig_suffixes)) {
  suffix <- fig_suffixes[i]
  explab <- exp_labels[i]

  # build file paths
  fname_ems  <- file.path(data_dir, paste0("emissions",   suffix, ".csv"))
  # fname_temp <- file.path(data_dir, paste0("temperature", suffix, ".csv"))
  fname_nat  <- file.path(data_dir, paste0("natvar",      suffix, ".csv"))
  fname_dist <- file.path(data_dir, paste0("distributions", suffix, ".Rdata"))

  # basic existence checks
  if (!file.exists(fname_ems))  stop("Missing: ", fname_ems)
  # if (!file.exists(fname_temp)) stop("Missing: ", fname_temp)
  if (!file.exists(fname_nat))  stop("Missing: ", fname_nat)
  if (!file.exists(fname_dist)) stop("Missing: ", fname_dist)

  ems_mat    <- as.matrix(fread(file = fname_ems))
  # temp_mat   <- as.matrix(fread(file = fname_temp))
  natvar_mat <- as.matrix(fread(file = fname_nat))
  load(fname_dist)   # loads 'dist' array [run, year, group]
  supporters_mat <- dist[,,3]

  # compute average natvar over analysis window and pick tails
  idx_range <- which(years >= analysis_years[1] & years <= analysis_years[2])
  avg_nat   <- rowMeans(natvar_mat[, idx_range, drop=FALSE], na.rm=TRUE)
  q_vals    <- c(pct_threshold, 1 - pct_threshold)
  q_thresh  <- quantile(avg_nat, q_vals, na.rm=TRUE)
  bottom_idx <- which(avg_nat <= q_thresh[1])
  top_idx    <- which(avg_nat >= q_thresh[2])

  subs_local <- list(
    "Coldest 10%" = list(ems = ems_mat[bottom_idx, , drop=FALSE],
                         sup = supporters_mat[bottom_idx, , drop=FALSE]),
    "Hottest 10%" = list(ems = ems_mat[top_idx, , drop=FALSE],
                         sup = supporters_mat[top_idx, , drop=FALSE]),
    "Median of All Runs" = list(ems = ems_mat, sup = supporters_mat)
  )

  # build data.tables for this experiment
  for (grp in names(subs_local)) {
    mat_ems <- subs_local[[grp]]$ems
    mat_sup <- subs_local[[grp]]$sup

    dt_ems <- data.table(
      Group = grp,
      Experiment = explab,
      year = years,
      median = apply(mat_ems, 2, median, na.rm = TRUE)
    )
    dt_sup <- data.table(
      Group = grp,
      Experiment = explab,
      year = years,
      median = apply(mat_sup, 2, median, na.rm = TRUE)
    )

    dt_ems_list[[length(dt_ems_list) + 1]] <- dt_ems
    dt_sup_list[[length(dt_sup_list) + 1]] <- dt_sup
  }
}

dt_long_ems <- rbindlist(dt_ems_list)
dt_long_sup <- rbindlist(dt_sup_list)

# ordering factors
dt_long_ems$Group <- factor(dt_long_ems$Group, levels = group_levels)
dt_long_sup$Group <- factor(dt_long_sup$Group, levels = group_levels)
dt_long_ems$Experiment <- factor(dt_long_ems$Experiment, levels = exp_labels)
dt_long_sup$Experiment <- factor(dt_long_sup$Experiment, levels = exp_labels)

# ---- Plot: top = supporters, bottom = emissions; color = Group, linetype = Experiment ----
p_sup <- ggplot(dt_long_sup, aes(x = year, y = median, color = Group, linetype = Experiment)) +
  geom_line(size = 1.05) +
  scale_color_manual(name = "Temperature Bin", values = group_colors) +
  scale_linetype_manual(values = linestyles) +
  labs(title = "Median Fraction of Climate Supporters", x = "", y = "Fraction Supporters") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.box = "horizontal")

p_ems <- ggplot(dt_long_ems, aes(x = year, y = median, color = Group, linetype = Experiment)) +
  geom_line(size = 1.05) +
  scale_color_manual(name = "Temperature Bin", values = group_colors) +
  scale_linetype_manual(values = linestyles) +
  labs(title = "Median Emissions (GtC/yr)", x = "Year", y = "Emissions (GtC/yr)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", legend.box = "horizontal")

fig_combined <- p_sup / p_ems +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.key.width = unit(1.5, "cm")   # make legend keys wider so dashes are visible
  ) &
  guides(
    color = guide_legend(ncol = 1),
    linetype = guide_legend(
      ncol = 1,
      keywidth = unit(1.5, "cm"),
      # override.aes = list(size = 1.4)    # draw thicker lines in legend (only), preserves plot linetypes
    )
  )

# ---- Save ----
out_file <- file.path("../results", paste0("multiExp_supporters_top_emissions_bottom",
                                           paste0(fig_suffixes, collapse = "_"), ".png"))
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
ggsave(out_file, fig_combined, width = 7, height = 10, dpi = 300)

message("Saved: ", out_file)
