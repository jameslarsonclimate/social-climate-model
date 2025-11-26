library(data.table)
library(ggplot2)
library(viridis)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
# fig_suffix    <- "_initClimSupportNormalDistribution"
# fig_suffix = '_CESM_HR_local_natVar_multiplier1'
fig_suffix = '_CESM_HR_local_natVar_500000runs'
# fig_suffix = '_CESM_HR_local_natVar_multiplier1'
# fig_suffix = '_CESM_HR_local_natVar-totalGDPweighted'
# fig_suffix = '_CESM_HR_local_natVar-popWeighted'

years         <- 2020:2100
 
# ---- Load data ----
message("Loading: ", data_dir, "emissions", fig_suffix, ".csv")
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
message("Loading: ", data_dir, "natvar", fig_suffix, ".csv")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))
message("Loading: ", data_dir, "temperature", fig_suffix, ".csv")
clim_mat <- as.matrix(fread(paste0(data_dir, "temperature", fig_suffix, ".csv")))
# Now load fraction of climate supporters
message("Loading: ", data_dir, "params", fig_suffix, ".csv")
dist_file <- paste0(data_dir, "distributions", fig_suffix, ".Rdata")
load(dist_file)   # loads 'dist' array [run, year, group]
supporters_mat <- dist[,,3]

# ---- Compute SD of full timeseries for each run ----
sd_full_all <- apply(natvar_mat, 1, sd, na.rm=TRUE)

# ---- Define bins for SD of full timeseries ----
bin_width <- 0.05
sd_full_centers <- seq(min(sd_full_all, na.rm=TRUE), max(sd_full_all, na.rm=TRUE), by=bin_width)
half_width <- bin_width / 2

# ---- Bin runs and compute median trajectories for each bin (ems, supporters, temp) ----
res_ems     <- list()
res_support <- list()
res_temp    <- list()
bin_counts  <- data.table(sd_bin = numeric(), n_runs = integer())
k_ems <- k_sup <- k_temp <- 1L

for (sd_full_center in sd_full_centers) {
  lo_full <- sd_full_center - half_width
  hi_full <- sd_full_center + half_width
  idx_bin <- which(sd_full_all >= lo_full & sd_full_all < hi_full)
  n_runs <- length(idx_bin)

  # Store bin counts for scatterplot
  bin_counts <- rbind(bin_counts, data.table(sd_bin = sd_full_center, n_runs = n_runs))

  if (n_runs > 500) {
    med_ems <- apply(ems_mat[idx_bin, , drop=FALSE], 2, median, na.rm=TRUE)
    med_sup <- apply(supporters_mat[idx_bin, , drop=FALSE], 2, median, na.rm=TRUE)
    med_tmp <- apply(clim_mat[idx_bin, , drop=FALSE], 2, median, na.rm=TRUE)

    res_ems[[k_ems]] <- data.table(
      year = years,
      median_ems = med_ems,
      sd_bin = sd_full_center,
      n_runs = n_runs
    ); k_ems <- k_ems + 1L

    res_support[[k_sup]] <- data.table(
      year = years,
      median_support = med_sup,
      sd_bin = sd_full_center,
      n_runs = n_runs
    ); k_sup <- k_sup + 1L

    res_temp[[k_temp]] <- data.table(
      year = years,
      median_temp = med_tmp,
      sd_bin = sd_full_center,
      n_runs = n_runs
    ); k_temp <- k_temp + 1L
  }
}

dt_ems     <- rbindlist(res_ems)
dt_support <- rbindlist(res_support)
dt_temp    <- rbindlist(res_temp)

# ---- Plot: median emissions trajectory for each SD bin ----
message("Plotting median emissions, supporters and temperature trajectories by SD bin...")
p_ems <- ggplot(dt_ems, aes(x = year, y = median_ems, group = sd_bin, color = sd_bin)) +
  geom_line(size = 1.0) +
  scale_color_viridis_c(
    name = "SD of Full Series (°C)",
    option = "C",
    guide = guide_colorbar(
      barwidth = unit(15, "lines"),
      barheight = unit(1, "lines"),
      direction = "horizontal",
      title.position = "top"
    )
  ) +
  labs(
    title = paste0("Median Emissions Trajectory by Full-Series SD Bin"),
    x = "Year",
    y = "Median Emissions (GtC yr^-1)"
  ) +
  theme_minimal(base_size=14) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  )

p_support <- ggplot(dt_support, aes(x = year, y = median_support, group = sd_bin, color = sd_bin)) +
  geom_line(size = 1.0) +
  scale_color_viridis_c(option = "C", name = "SD of Full Series (°C)") +
  labs(
    title = paste0("Median Fraction of Climate Supporters"),
    x = "Year",
    y = "Median Fraction Supporters"
  ) +
  theme_minimal(base_size=14) +
  theme(
    legend.position = "none"
  )

p_temp <- ggplot(dt_temp, aes(x = year, y = median_temp, group = sd_bin, color = sd_bin)) +
  geom_line(size = 1.0) +
  scale_color_viridis_c(option = "C", name = "SD of Full Series (°C)") +
  labs(
    title = paste0("Median Temperature"),
    x = "Year",
    y = "Median Temperature (°C)"
  ) +
  theme_minimal(base_size=14) +
  theme(
    legend.position = "none"
  )

# ---- Combine panels and save ----
combined <-  p_support / p_ems / p_temp + plot_layout(guides = "collect") & theme(legend.position = "bottom", legend.direction = "horizontal")
out_dir <- "../results"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(out_dir, paste0("median_ems_support_temp_by_fullSD_bin_", fig_suffix, ".png"))
message("Saving: ", out_file)
ggsave(out_file, combined, width = 8, height = 15, dpi = 300)
