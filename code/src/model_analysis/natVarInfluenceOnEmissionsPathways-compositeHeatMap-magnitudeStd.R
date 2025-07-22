library(data.table)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir      <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix    <- "_initClimSupportNormalDistribution" #-natVarMultiplier10"
# fig_suffix = '_ERA5natVar'
years         <- 2020:2100
start_year    <- 2025
# max_dur       <- length(years) - (start_year - min(years))
max_dur       <- 20
bin_centers   <- seq(-1.5, 1.5, by=0.1)
half_width    <- 0.05

# ---- Load data ----
message("Loading: ", data_dir, "emissions", fig_suffix, ".csv")
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
message("Loading: ", data_dir, "natvar", fig_suffix, ".csv")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Load and append data from all suffixes ----

# print reading messages and the suffixes being loaded
# fig_suffixes  <- c(
#   "_initClimSupportNormalDistribution",
#   "_initClimSupportNormalDistribution-natVarMultiplier10",
#   "_initClimSupportNormalDistribution-natVarMultiplier12"
# )

# message("Loading data for suffixes: \n", paste(fig_suffixes, collapse=",\n"))

# ems_list    <- list()
# natvar_list <- list()
# for (suffix in fig_suffixes) {
#   message("Loading: ", data_dir, "emissions", suffix, ".csv")
#   ems_list[[suffix]]    <- as.matrix(fread(paste0(data_dir, "emissions", suffix, ".csv")))
#   message("Loading: ", data_dir, "natvar", suffix, ".csv")
#   natvar_list[[suffix]] <- as.matrix(fread(paste0(data_dir, "natvar", suffix, ".csv")))
# }
# ems_mat    <- do.call(rbind, ems_list)
# natvar_mat <- do.call(rbind, natvar_list)

# message("Data loaded successfully.")
# fig_suffix    <- "_initClimSupportNormalDistribution-appended" # Update suffix for output


# ---- Loop over duration & magnitude bins, compute net-zero year and p-value together ----
res <- list()
k   <- 1L

# ---- Compute global median net‐zero year ----
med_all       <- apply(ems_mat, 2, median, na.rm=TRUE)
zz_all        <- which(med_all <= 0)
zero_year_all <- if (length(zz_all)>0) years[min(zz_all)] else NA_integer_

# Precompute all runs' net-zero years for the sign test
all_zero_years <- apply(ems_mat, 1, function(x) {
  zz <- which(x <= 0)
  if (length(zz) > 0) years[min(zz)] else NA_integer_
})

sign_test <- function(x, mu) {
  x <- x[!is.na(x)]
  n_pos <- sum(x > mu)
  n_neg <- sum(x < mu)
  n <- n_pos + n_neg
  if (n == 0) return(1)
  pval <- 2 * pbinom(min(n_pos, n_neg), n, 0.5)
  pval <- min(pval, 1)
  return(pval)
}

for (dur in 1:max_dur) {
  print(sprintf("Processing: Duration = %d years out of %d max years", dur, max_dur))
  end_year <- start_year + dur - 1
  idx_range <- which(years >= start_year & years <= end_year)
  std_nat   <- apply(natvar_mat[, idx_range, drop=FALSE], 1, sd, na.rm=TRUE)

  for (mag in bin_centers) {
    lo <- mag - half_width
    hi <- mag + half_width
    idx <- which(std_nat >= lo & std_nat < hi)
    zero_year <- NA_integer_
    p_value <- NA_real_

    if (length(idx) > 0) {
      med_traj  <- apply(ems_mat[idx, , drop=FALSE], 2, median, na.rm=TRUE)
      zz        <- which(med_traj <= 0)
      zero_year <- if (length(zz)>0) years[min(zz)] else NA_integer_

      # Net-zero years for this bin
      bin_zero_years <- apply(ems_mat[idx, , drop=FALSE], 1, function(x) {
        zz <- which(x <= 0)
        if (length(zz) > 0) years[min(zz)] else NA_integer_
      })
      bin_zero_years <- bin_zero_years[!is.na(bin_zero_years)]
      # Sign Test: is the bin median different from the population median?
      if (length(bin_zero_years) > 1 && !is.na(zero_year_all)) {
        p_value <- sign_test(bin_zero_years, zero_year_all)
      }
    }

    res[[k]] <- list(
      duration   = dur,
      magnitude  = mag,
      zero_year  = zero_year,
      p_value    = p_value
    )
    k <- k + 1L
  }
}

dt_bin <- rbindlist(res)[!is.na(zero_year)]

# Set color scale limits and breaks
center_year <- zero_year_all
range_years <- 10
# fill_limits <- c(center_year - range_years, center_year + range_years)
# fill_breaks <- seq(center_year - range_years, center_year + range_years, by = 2)
fill_breaks <- seq(2076,2098,2)
fill_limits <- c(min(fill_breaks), max(fill_breaks))

# ---- Plot heatmap of zero‐year by bin & duration ----
message("Plotting heatmap of net-zero year by bin and duration...")
p_bin <- ggplot(dt_bin, aes(x = magnitude, y = duration, fill = zero_year)) +
  geom_tile() +
  scale_fill_stepsn(
    colors = brewer.pal(length(fill_breaks) - 1, "PRGn"),
    name   = "Year Net-Zero",
    # na.value = "grey80",
    limits = fill_limits,
    breaks = fill_breaks,
    oob    = scales::oob_squish
  ) +
  guides(fill = guide_colorbar(barwidth = unit(12, "cm"), 
                               barheight = unit(0.5, "cm"))) +
  labs(
    title = paste0("Net-Zero Year by Natural Variability Bin and Duration\n",
                   fig_suffix, "\n",
                   "Median Year of Net Zero for All Runs is ", zero_year_all),
    x = "Standard Deviation of Natural Variability (degC)",
    y = "Duration (yrs)"
  ) +
  theme_minimal(base_size=14) +
  theme(
    panel.grid     = element_blank(),
    legend.position = "bottom"
  ) +  # Add points for statistical significance
  geom_point(
    data = dt_bin[is.na(p_value) | p_value > 0.05],
    aes(x = magnitude, y = duration),
    shape = 8, color = "black", size = 1.5, alpha = 0.7, inherit.aes = FALSE
  )

# ---- Save figure ----
out_dir <- "../results/heatmaps"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(
  out_dir,
  paste0("netzero_heatmap_bin_stdNatVarBin_", fig_suffix, ".png")
)
message("Saving: ", out_file)
ggsave(out_file, p_bin, width=8, height=6)


# # ---- Build difference table: per‐bin minus overall ----
# dt_diff <- copy(dt_bin)[
#   , diff := zero_year - zero_year_all
# ]

# # symmetric color‐scale limits
# lim <- max(abs(dt_diff$diff), na.rm=TRUE)

# # ---- Plot difference heatmap ----
# p_diff <- ggplot(dt_diff, aes(x = magnitude, y = duration, fill = diff)) +
#   geom_tile() +
#   scale_fill_stepsn(
#     colors = brewer.pal(9, "RdBu"),
#     limits = c(-5, 5),
#     breaks = c(-5, -4 ,-3 ,-2 ,-1, 1, 2, 3, 4, 5),
#     name   = "Subset - All Runs\n(Net-Zero Years)"
#   ) +
#   guides(fill = guide_colorbar(
#     barwidth  = unit(6, "cm"),
#     barheight = unit(0.5, "cm")
#   )) +
#   labs(
#     title = paste0(
#       "Difference in Net-Zero Year by NatVar Bin vs All Runs\n",
#       fig_suffix
#     ),
#     x = "Standard Deviation of Natural Variability (degC)",
#     y = "Duration (yrs)"
#   ) +
#   theme_minimal(base_size=14) +
#   theme(
#     panel.grid     = element_blank(),
#     legend.position= "bottom"
#   ) 

# # ---- Save difference figure ----
# out_file_diff <- file.path(
#   out_dir,
#   paste0("netzero_heatmap_bin_stdNatVarBin_diff", fig_suffix, ".png")
# )
# message("Saving difference heatmap to: ", out_file_diff)
# ggsave(out_file_diff, p_diff, width=8, height=6)
