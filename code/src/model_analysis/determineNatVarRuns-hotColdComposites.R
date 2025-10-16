library(data.table)
library(ggplot2)

# ---- Settings ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir  <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix <- "_initClimSupportNormalDistribution-500Kruns"
years <- 2020:2100

# Sampling / plotting options
pct_threshold <- 0.05    # hottest / coldest fraction (e.g. 0.05 = 5%)
n_plot <- 4             # number of runs to plot from each tail (ignored if prescribing indexes)
sample_seed <- 42        # for reproducibility

# Optional: prescribe specific run indexes (absolute row numbers in natvar file).
# If NULL the script will randomly sample from the tail. If provided, these exact
# run indices will be used (warnings issued if out of range or not in the computed tail).
prescribed_cold_idxs <- c(283483) # NULL   # e.g. c(10, 20, 33)
prescribed_hot_idxs  <- c(241) # NULL   # e.g. c(5, 123, 205)

# ---- Load natvar ----
# ...existing code...
fname_nat <- file.path(data_dir, paste0("natvar", fig_suffix, ".csv"))
if (!file.exists(fname_nat)) stop("natvar file not found: ", fname_nat)
natvar_dt <- fread(fname_nat)
natvar_mat <- as.matrix(natvar_dt)      # rows = runs, cols = years
if (ncol(natvar_mat) != length(years)) warning("column count != length(years)")

# ---- Compute average natvar over 2025:2034 (indexes 6:15) and select tails ----
idx_range <- which(years >= 2025 & years <= 2034)  # should be 6:15
avg_nat <- rowMeans(natvar_mat[, idx_range, drop = FALSE], na.rm = TRUE)

q_vals <- quantile(avg_nat, probs = c(pct_threshold, 1 - pct_threshold), na.rm = TRUE)
cold_idx_all <- which(avg_nat <= q_vals[1])
hot_idx_all  <- which(avg_nat >= q_vals[2])

if (length(cold_idx_all) == 0 || length(hot_idx_all) == 0) {
  stop("No runs identified in one of the tails; check natvar or pct_threshold.")
}

# ---- Randomly sample runs from each tail (or use prescribed indexes) ----
# determine default sample sizes
default_n_cold <- min(n_plot, length(cold_idx_all))
default_n_hot  <- min(n_plot, length(hot_idx_all))

# Cold
if (!is.null(prescribed_cold_idxs)) {
  # validate range
  valid_mask <- prescribed_cold_idxs %in% seq_len(nrow(natvar_mat))
  if (!all(valid_mask)) {
    warning("Some prescribed_cold_idxs are out of range and will be ignored: ",
            paste(setdiff(prescribed_cold_idxs, seq_len(nrow(natvar_mat))), collapse = ", "))
  }
  chosen_cold <- prescribed_cold_idxs[valid_mask]
  if (length(chosen_cold) == 0) stop("No valid prescribed_cold_idxs remain after validation.")
  not_in_tail <- setdiff(chosen_cold, cold_idx_all)
  if (length(not_in_tail) > 0) {
    warning("Some prescribed_cold_idxs are not in the cold tail (they will still be used): ",
            paste(not_in_tail, collapse = ", "))
  }
  cold_sample_idx <- chosen_cold
  n_cold <- length(cold_sample_idx)
} else {
  set.seed(sample_seed)
  cold_sample_idx <- sample(cold_idx_all, default_n_cold)
  n_cold <- length(cold_sample_idx)
}

# Hot
if (!is.null(prescribed_hot_idxs)) {
  valid_mask <- prescribed_hot_idxs %in% seq_len(nrow(natvar_mat))
  if (!all(valid_mask)) {
    warning("Some prescribed_hot_idxs are out of range and will be ignored: ",
            paste(setdiff(prescribed_hot_idxs, seq_len(nrow(natvar_mat))), collapse = ", "))
  }
  chosen_hot <- prescribed_hot_idxs[valid_mask]
  if (length(chosen_hot) == 0) stop("No valid prescribed_hot_idxs remain after validation.")
  not_in_tail <- setdiff(chosen_hot, hot_idx_all)
  if (length(not_in_tail) > 0) {
    warning("Some prescribed_hot_idxs are not in the hot tail (they will still be used): ",
            paste(not_in_tail, collapse = ", "))
  }
  hot_sample_idx <- chosen_hot
  n_hot <- length(hot_sample_idx)
} else {
  set.seed(sample_seed)
  hot_sample_idx <- sample(hot_idx_all, default_n_hot)
  n_hot <- length(hot_sample_idx)
}

# ---- Prepare long table for plotting ----
# ...existing code...
make_dt_for_idx <- function(idxs, prefix) {
  submat <- natvar_mat[idxs, , drop = FALSE]
  run_ids <- seq_len(nrow(submat))
  dt_list <- lapply(seq_len(nrow(submat)), function(i) {
    data.table(
      run_idx = idxs[i],
      run_label = paste0(prefix, "_", i),
      year = years,
      value = as.numeric(submat[i, ])
    )
  })
  rbindlist(dt_list)
}

dt_cold <- make_dt_for_idx(cold_sample_idx, "cold")
dt_hot  <- make_dt_for_idx(hot_sample_idx,  "hot")
dt_plot <- rbindlist(list(dt_cold, dt_hot))
dt_plot[, Group := ifelse(grepl("^cold_", run_label), "Cold (lowest 10%)", "Hot (highest 10%)")]

# ---- Color palettes (shades of blue for cold, red for hot) ----
cold_colors <- colorRampPalette(c("#deebf7", "#3182bd"))(n_cold)
hot_colors  <- colorRampPalette(c("#fcbba1", "#cb181d"))(n_hot)
run_labels <- unique(dt_plot$run_label)
color_map <- setNames(c(cold_colors, hot_colors), c(paste0("cold_", seq_len(n_cold)), paste0("hot_", seq_len(n_hot))))

# ---- Plot ----
# ...existing code...
p <- ggplot(dt_plot, aes(x = year, y = value, group = run_label, color = run_label)) +
  # thick grey zero line behind the colored lines
  geom_hline(yintercept = 0, color = "grey60", size = 1.2, linetype = "solid") +
  geom_line(size = 0.9, alpha = 0.95) +
  scale_color_manual(name = "Sample runs", values = color_map) +
  facet_wrap(~Group, ncol = 1, scales = "free_y") +
  labs(
    title = paste0("Example natvar realizations: coldest vs hottest ", pct_threshold*100, "% (", fig_suffix, ")"),
    x = "Year",
    y = "Natural variability (°C, local 2m)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    strip.background = element_rect(fill = "grey95", color = NA)
  )

# ---- Save ----
# ...existing code...
out_dir <- "../results/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, paste0("natvar_example_cold_hot_", gsub("^_", "", fig_suffix), ".png"))
ggsave(out_file, p, width = 9, height = 6, dpi = 300)

message("Saved plot to: ", out_file)
