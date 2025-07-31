# ---- Visualize Wilcoxon Test for a Single Bin (mag = 0.8, dur = 5) ----

library(data.table)
library(ggplot2)

# --- Parameters for bin selection ---
mag <- 0.8
dur <- 5
half_width <- 0.05
start_year <- 2025
years <- 2020:2100

# --- Load emissions and natvar data (use same files as main analysis) ---
# data_dir <- "../results/MC Runs/MC Runs_TunedParams/"
# fig_suffix <- "_initClimSupportNormalDistribution" # adjust if needed

# ems_mat <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
# natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar", fig_suffix, ".csv")))

fig_suffixes  <- c(
  "_initClimSupportNormalDistribution",
  "_initClimSupportNormalDistribution-resample1",
  "_initClimSupportNormalDistribution-resample2",
  "_initClimSupportNormalDistribution-resample3"
)

message("Loading data for suffixes: \n", paste(fig_suffixes, collapse=",\n"))

ems_list    <- list()
natvar_list <- list()
for (suffix in fig_suffixes) {
  message("Loading: ", data_dir, "emissions", suffix, ".csv")
  ems_list[[suffix]]    <- as.matrix(fread(paste0(data_dir, "emissions", suffix, ".csv")))
  message("Loading: ", data_dir, "natvar", suffix, ".csv")
  natvar_list[[suffix]] <- as.matrix(fread(paste0(data_dir, "natvar", suffix, ".csv")))
}
ems_mat    <- do.call(rbind, ems_list)
natvar_mat <- do.call(rbind, natvar_list)

message("Data loaded successfully.")
fig_suffix    <- "_initClimSupportNormalDistribution-appended" # Update suffix for output


# --- Select runs in the bin ---
end_year <- start_year + dur - 1
idx_range <- which(years >= start_year & years <= end_year)
avg_nat   <- rowMeans(natvar_mat[, idx_range, drop=FALSE], na.rm=TRUE)
lo <- mag - half_width
hi <- mag + half_width
idx <- which(avg_nat >= lo & avg_nat < hi)

# --- Get net-zero years for runs in this bin ---
bin_zero_years <- apply(ems_mat[idx, , drop=FALSE], 1, function(x) {
  zz <- which(x <= 0)
  if (length(zz) > 0) years[min(zz)] else NA_integer_
})
bin_zero_years <- bin_zero_years[!is.na(bin_zero_years)]

# --- Get net-zero years for all runs ---
all_zero_years <- apply(ems_mat, 1, function(x) {
  zz <- which(x <= 0)
  if (length(zz) > 0) years[min(zz)] else NA_integer_
})
all_zero_years <- all_zero_years[!is.na(all_zero_years)]

# --- Wilcoxon test ---
pval <- if (length(bin_zero_years) > 1 && length(all_zero_years) > 1) {
  wilcox.test(bin_zero_years, all_zero_years, alternative = "two.sided")$p.value
} else {
  NA_real_
}

# --- Prepare data for plotting ---
df <- rbind(
  data.frame(NetZeroYear = all_zero_years, Group = "All Runs"),
  data.frame(NetZeroYear = bin_zero_years, Group = paste0("Bin: mag=", mag, ", dur=", dur))
)

# --- Plot ---
p <- ggplot(df, aes(x = NetZeroYear, fill = Group, color = Group)) +
  geom_density(alpha = 0.4, adjust = 1.2) +
  geom_vline(
    data = aggregate(NetZeroYear ~ Group, df, median),
    aes(xintercept = NetZeroYear, color = Group),
    linetype = "solid", size = 1
  ) +
  labs(
    title = paste0(
      "Wilcoxon Test: Net-Zero Years (mag = ", mag, ", dur = ", dur, ")\n",
      "p-value = ", signif(pval, 3),
      ifelse(!is.na(pval) && pval < 0.05, " (Significant)", " (Not Significant)")
    ),
    x = "Net-Zero Year",
    y = "Density",
    fill = "Group",
    color = "Group"
  ) +
  theme_minimal(base_size = 14)

# Save plot
out_dir <- "../results/heatmaps"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, paste0("wilcox_test_net_zero_years_mag", mag, "_dur", dur, ".png"))
ggsave(out_file, p, width = 8, height = 5, dpi = 300)


# --- Plot histogram of bin_zero_years with medians for bin and all runs ---

# Calculate medians
median_bin <- median(bin_zero_years, na.rm = TRUE)
median_all <- median(all_zero_years, na.rm = TRUE)

p_hist <- ggplot(data.frame(NetZeroYear = bin_zero_years), aes(x = NetZeroYear)) +
  geom_histogram(binwidth = 1, fill = "#619CFF", color = "black", alpha = 0.7, boundary = 0, closed = "left") +
  geom_vline(xintercept = median_bin, color = "#619CFF", linetype = "solid", size = 1.2) +
  geom_vline(xintercept = median_all, color = "black", linetype = "dashed", size = 1.2) +
  labs(
    title = paste0("Histogram of Net-Zero Years for Bin (mag = ", mag, ", dur = ", dur, ")"),
    subtitle = paste0("Solid blue: Bin median (", median_bin, "), Dashed black: All runs median (", median_all, ")"),
    x = "Net-Zero Year",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

# Save plot
out_file_hist <- file.path(out_dir, paste0("histogram_bin_net_zero_years_mag", mag, "_dur", dur, ".png"))
ggsave(out_file_hist, p_hist, width = 8, height = 5, dpi = 300)
