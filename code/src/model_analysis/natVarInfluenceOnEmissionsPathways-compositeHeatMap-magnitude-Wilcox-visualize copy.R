# ---- Visualize Wilcoxon Test and Histogram for a Bin Across Multiple Datasets ----

library(data.table)
library(ggplot2)

# --- Parameters for bin selection ---
mag <- 0.8
dur <- 5
half_width <- 0.05
start_year <- 2025
years <- 2020:2100

data_dir <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffixes  <- c(
  "_initClimSupportNormalDistribution",
  "_initClimSupportNormalDistribution-resample1",
  "_initClimSupportNormalDistribution-resample2",
  "_initClimSupportNormalDistribution-resample3"
)
dataset_labels <- c("Original", "Resample 1", "Resample 2", "Resample 3")

# --- Load and process each dataset ---
all_densities <- list()
all_histograms <- list()
all_medians <- data.frame()
all_bin_medians <- data.frame()
all_pvals <- data.frame()

for (i in seq_along(fig_suffixes)) {
  suffix <- fig_suffixes[i]
  label <- dataset_labels[i]
  message("Processing: ", label, " (", suffix, ")")

  # Load data
  ems_mat <- as.matrix(fread(paste0(data_dir, "emissions", suffix, ".csv")))
  natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar", suffix, ".csv")))

  # Select runs in the bin
  end_year <- start_year + dur - 1
  idx_range <- which(years >= start_year & years <= end_year)
  avg_nat   <- rowMeans(natvar_mat[, idx_range, drop=FALSE], na.rm=TRUE)
  lo <- mag - half_width
  hi <- mag + half_width
  idx <- which(avg_nat >= lo & avg_nat < hi)

  # Get net-zero years for runs in this bin
  bin_zero_years <- apply(ems_mat[idx, , drop=FALSE], 1, function(x) {
    zz <- which(x <= 0)
    if (length(zz) > 0) years[min(zz)] else NA_integer_
  })
  bin_zero_years <- bin_zero_years[!is.na(bin_zero_years)]

  # Get net-zero years for all runs
  all_zero_years <- apply(ems_mat, 1, function(x) {
    zz <- which(x <= 0)
    if (length(zz) > 0) years[min(zz)] else NA_integer_
  })
  all_zero_years <- all_zero_years[!is.na(all_zero_years)]

  # Wilcoxon test
  pval <- if (length(bin_zero_years) > 1 && length(all_zero_years) > 1) {
    wilcox.test(bin_zero_years, all_zero_years, alternative = "two.sided")$p.value
  } else {
    NA_real_
  }

  # Store for density plot
  all_densities[[label]] <- rbind(
    data.frame(NetZeroYear = all_zero_years, Group = paste0(label, " (All Runs)")),
    data.frame(NetZeroYear = bin_zero_years, Group = paste0(label, " (Bin)"))
  )

  # Store for histogram plot
  all_histograms[[label]] <- data.frame(NetZeroYear = bin_zero_years, Dataset = label)

  # Store medians
  all_medians <- rbind(
    all_medians,
    data.frame(Dataset = label, Group = "All Runs", Median = median(all_zero_years, na.rm = TRUE)),
    data.frame(Dataset = label, Group = "Bin", Median = median(bin_zero_years, na.rm = TRUE))
  )
  all_bin_medians <- rbind(
    all_bin_medians,
    data.frame(Dataset = label, Median = median(bin_zero_years, na.rm = TRUE))
  )
  all_pvals <- rbind(
    all_pvals,
    data.frame(Dataset = label, PValue = pval)
  )
}

# --- Combine for plotting ---
df_density <- do.call(rbind, all_densities)
df_hist <- do.call(rbind, all_histograms)

# --- Density plot with Wilcoxon p-values ---
p_density <- ggplot(df_density, aes(x = NetZeroYear, fill = Group, color = Group)) +
  geom_density(alpha = 0.4, adjust = 1.2) +
  geom_vline(
    data = all_medians,
    aes(xintercept = Median, color = paste(Dataset, Group)),
    linetype = "solid", size = 1
  ) +
  facet_wrap(~ sub(" \\(.*", "", Group), ncol = 2) +
  labs(
    title = paste0(
      "Wilcoxon Test: Net-Zero Years (mag = ", mag, ", dur = ", dur, ")\n",
      paste0(dataset_labels, ": p = ", signif(all_pvals$PValue, 3), collapse = " | ")
    ),
    x = "Net-Zero Year",
    y = "Density",
    fill = "Group",
    color = "Group"
  ) +
  theme_minimal(base_size = 14)

# --- Histogram plot for bin_zero_years with medians ---
p_hist <- ggplot(df_hist, aes(x = NetZeroYear, fill = Dataset)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7, boundary = 0, closed = "left", position = "identity") +
  geom_vline(
    data = all_bin_medians,
    aes(xintercept = Median, color = Dataset, size = Dataset),
    linetype = "solid"
  ) +
  scale_size_manual(
    values = c("Original" = 2.4, "Resample 1" = 1.2, "Resample 2" = 1.2, "Resample 3" = 1.2),
    guide = "none"
  ) +
  labs(
    title = paste0("Histogram of Net-Zero Years for Bin (mag = ", mag, ", dur = ", dur, ")"),
    subtitle = "Solid lines: Bin medians for each dataset",
    x = "Net-Zero Year",
    y = "Count",
    fill = "Dataset",
    color = "Dataset"
  ) +
  theme_minimal(base_size = 14)

# --- Save plots ---
out_dir <- "../results/heatmaps"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file_density <- file.path(out_dir, paste0("wilcox_test_net_zero_years_multiset_mag", mag, "_dur", dur, ".png"))
out_file_hist <- file.path(out_dir, paste0("histogram_bin_net_zero_years_multiset_mag", mag, "_dur", dur, ".png"))
ggsave(out_file_density, p_density, width = 10, height = 6, dpi = 300)
ggsave(out_file_hist, p_hist, width = 10, height = 6, dpi = 300)

