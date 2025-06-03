library(data.table)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# ---- Load data ----
data_dir <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix <- ""  # or your suffix
fig_suffix <-"_varyInitialDistribution"
print(paste0(data_dir, "emissions",    fig_suffix, ".csv"))

ems    <- fread(paste0(data_dir, "emissions",    fig_suffix, ".csv"))
clim   <- fread(paste0(data_dir, "temperature",  fig_suffix, ".csv"))
natvar <- fread(paste0(data_dir, "natvar",       fig_suffix, ".csv"))

# to matrices
ems_mat    <- as.matrix(ems)
clim_mat   <- as.matrix(clim)
natvar_mat <- as.matrix(natvar)

years <- 2020:2100

# Draw 100,000 valid (frac_opp + frac_neut) sums and plot 1 - s
n <- 100000
s_vals <- replicate(n, {
  repeat {
    frac_opp  <- runif(1, 0.1, 0.8)
    frac_neut <- runif(1, 0.1, 0.8)
    s <- frac_opp + frac_neut
    if (s >= 0.2 && s <= 0.8) break
  }
  s
})
one_minus_s <- 1 - s_vals

# Plot distribution of (1 - s)
library(ggplot2)
df <- data.frame(remaining = one_minus_s)
library(scales)  # for comma formatting
ggplot(df, aes(x = remaining)) +
  # geom_histogram(bins = 50, fill = "#0072B2", color = "white", alpha = 0.8) +
  geom_density(aes(y = ..count..), color = "#D55E00", size = 1) +
  scale_y_continuous(
    breaks = c(0, 100000, 200000, 300000),
    labels = comma
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribution of 1 - (frac_opp + frac_neut)",
    x     = "1 - s",
    y     = "Count"
  )

  # Save the last plot to file
  ggsave(
    filename = paste0("../results/hist_remaining", fig_suffix, ".png"),
    plot     = last_plot(),
    width    = 8,
    height   = 6,
    dpi      = 300
  )