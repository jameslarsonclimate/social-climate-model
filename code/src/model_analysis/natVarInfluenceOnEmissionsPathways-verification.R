# Demonstration of the Sign Test with Real Data: mag = 0.6, dur = 5

library(data.table)
library(ggplot2)

# --- Parameters for bin selection ---
mag <- 0.8
dur <- 5
half_width <- 0.05
start_year <- 2025
years <- 2020:2100

# --- Load emissions and natvar data ---
data_dir <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix <- "_initClimSupportNormalDistribution" # adjust if needed

ems_mat <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar", fig_suffix, ".csv")))

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

# --- Get overall median net-zero year ---
med_all <- apply(ems_mat, 2, median, na.rm=TRUE)
zz_all <- which(med_all <= 0)
zero_year_all <- if (length(zz_all) > 0) years[min(zz_all)] else NA_integer_

# --- Sign test calculation ---
above <- bin_zero_years[bin_zero_years > zero_year_all]
below <- bin_zero_years[bin_zero_years < zero_year_all]
equal <- bin_zero_years[bin_zero_years == zero_year_all]
n_pos <- length(above)
n_neg <- length(below)
n <- n_pos + n_neg
pval <- if (n == 0) 1 else 2 * pbinom(min(n_pos, n_neg), n, 0.5)
pval <- min(pval, 1)

# --- Plot ---
df <- data.frame(
  NetZeroYear = bin_zero_years,
  Category = factor(
    ifelse(bin_zero_years < zero_year_all, "Below Reference",
    ifelse(bin_zero_years > zero_year_all, "Above Reference", "Equal to Reference")),
    levels = c("Below Reference", "Equal to Reference", "Above Reference")
  )
)

p <- ggplot(df, aes(x = NetZeroYear, fill = Category)) +
  geom_histogram(binwidth = 1, color = "black", boundary = 0, closed = "left") +
  geom_vline(xintercept = zero_year_all, linetype = "dashed", color = "red", size = 1.2) +
  scale_fill_manual(values = c("#1b9e77", "#d3d3d3", "#d95f02")) +
  labs(
    title = paste0(
      "Sign Test: Net-Zero Years (mag = ", mag, ", dur = ", dur, ")\n",
      "p-value = ", signif(pval, 2),
      ifelse(pval < 0.01, " (Significant)", " (Not Significant)")
    ),
    x = "Net-Zero Year",
    y = "Count",
    fill = "Category"
  ) +
  annotate("text", x = zero_year_all, y = max(table(bin_zero_years)) + 1,
           label = paste("Reference Median\n", zero_year_all), color = "red", vjust = 0) +
  theme_minimal(base_size = 14)

ggsave(
  filename = "../results/heatmaps/sign_test_net_zero_years_histogram_realdata.png",
  plot = p,
  width = 8,
  height = 5,
  dpi = 300
)
