library(data.table)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
# fig_suffix    <- "_initClimSupportNormalDistribution"
fig_suffix = '_ERA5natVar'

pct_threshold <- 0.10
years         <- 2020:2100
max_dur       <- length(years) - 1

# ---- Load data ----
cat("Loading emissions:", data_dir, "emissions", fig_suffix, ".csv\n")
ems_df     <- fread(paste0(data_dir, "emissions",   fig_suffix, ".csv"))
cat("Loading natvar:   ", data_dir, "natvar",      fig_suffix, ".csv\n")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Compute integrated emissions per run ----
ems_vec <- rowSums(as.matrix(ems_df), na.rm = TRUE)

# ---- Prepare storage ----
res <- vector("list", max_dur * length(years) * 2)  # pre-allocate approx.
k   <- 1L

# ---- Nested loops: duration & start year ----
for (dur in seq_len(max_dur)) {
  if (dur %% 10 == 0) cat("Working on duration", dur, "of", max_dur, "\n")
  for (start_year in years) {
    end_year <- start_year + dur - 1
    if (end_year > max(years)) break

    idx_range <- which(years >= start_year & years <= end_year)
    avg_nat   <- rowMeans(natvar_mat[, idx_range, drop = FALSE], na.rm = TRUE)

    # compute percentile thresholds
    qv <- quantile(avg_nat, c(pct_threshold, 1 - pct_threshold), na.rm = TRUE)
    labs <- c(
      paste0("Coldest ", pct_threshold * 100, "%"),
      paste0("Hottest ", pct_threshold * 100, "%")
    )
    subsets <- setNames(
      list(which(avg_nat <= qv[1]), which(avg_nat >= qv[2])),
      labs
    )

    # for each subset compute median integrated emissions
    for (lbl in names(subsets)) {
      idx <- subsets[[lbl]]
      med_int <- if (length(idx) == 0) NA_real_ else median(ems_vec[idx], na.rm = TRUE)
      res[[k]] <- list(
        start_year = start_year,
        duration   = dur,
        subset     = lbl,
        med_int    = med_int
      )
      k <- k + 1L
    }
  }
}

dt_int <- rbindlist(res[1:(k-1)])[!is.na(med_int)]



# ---- Plot heatmap: median integrated emissions ----
p_int <- ggplot(dt_int, aes(x = start_year, y = duration, fill = med_int)) +
  geom_tile() +
  facet_wrap(~ subset, ncol = 1) +
  scale_fill_viridis(
    name   = "Median Integrated\nEmissions (GtC)",
    option = "C"
  ) +
  guides(fill = guide_colorbar(barwidth = unit(6, "cm"), barheight = unit(0.4, "cm"))) +
  labs(
    title = paste0(
      "Median Integrated Emissions by NatVar Extremes\n",
      fig_suffix, " @ ", pct_threshold*100, "% threshold"
    ),
    x = "Starting Year of Extreme Period",
    y = "Duration of Extreme Period (years)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid     = element_blank(),
    legend.position= "bottom"
  ) 
  # coord_fixed()

# ---- Save heatmap ----
out_dir <- "../results/heatmaps"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(
  out_dir,
  paste0("intEmissions_heatmap", fig_suffix, "_", pct_threshold*100, "pct.png")
)
cat("Saving heatmap to:", out_file, "\n")
ggsave(out_file, p_int, width = 8, height = 6)



# ---- Difference heatmap: median vs all runs ----
med_int_all <- median(ems_vec, na.rm = TRUE)
dt_int_diff <- dt_int[, diff_int := med_int - med_int_all]

lim_int <- max(abs(dt_int_diff$diff_int), na.rm = TRUE)

p_int_diff <- ggplot(dt_int_diff, aes(x = start_year, y = duration, fill = diff_int)) +
  geom_tile() +
  facet_wrap(~ subset, ncol = 1) +
  scale_fill_stepsn(
    colors = brewer.pal(9, "RdBu"), # rev(RColorBrewer::brewer.pal(9,"RdBu"))
    limits = c(-55, 55),
    breaks = seq(-55, 55, by = 10),
    name   = "Subset - All Runs\n(GtC)"
  ) +
  guides(fill = guide_colorbar(barwidth = unit(7, "cm"),  # increase width
                               barheight = unit(0.5, "cm"))) +
  labs(
    title = paste0(
      "Difference in Median Integrated Emissions vs All Runs\n",
      fig_suffix, " @ ", pct_threshold*100, "% threshold"
    ),
    x = "Starting Year of Extreme Period",
    y = "Duration of Extreme Period (years)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid     = element_blank(),
    legend.position= "bottom"
  ) 

out_file_diff <- file.path(
  out_dir,
  paste0("intEmissions_heatmap_diff", fig_suffix, "_", pct_threshold*100, "pct.png")
)
cat("Saving difference heatmap to:", out_file_diff, "\n")
ggsave(out_file_diff, p_int_diff, width = 8, height = 6)

