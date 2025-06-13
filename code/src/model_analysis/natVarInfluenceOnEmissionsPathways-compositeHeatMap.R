library(data.table)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir        <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix      <- "_initClimSupportNormalDistribution"
pct_threshold   <- 0.1
years           <- 2020:2100
max_dur         <- length(years) - 1
start_years_all <- years

# ---- Load data ----
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions",   fig_suffix, ".csv")))
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",      fig_suffix, ".csv")))

# Print out the directory and filename seperately
cat("\nData directory:", data_dir, "\n")
cat("Emissions file:", paste0("emissions", fig_suffix, ".csv"), "\n\n")

# ---- Prepare storage ----
res <- vector("list", max_dur * length(start_years_all))
k   <- 1L

# ---- Nested loops over duration & start year ----
for (dur in 1:max_dur) {
  for (start_year in years) {
    end_year <- start_year + dur - 1
    if (end_year > max(years)) next

    # Print progress
    if (start_year == 2020) cat(sprintf("Processing: Duration = %d years out of %d max years\n", dur, max_dur))

    idx_range <- which(years >= start_year & years <= end_year)
    avg_nat   <- rowMeans(natvar_mat[, idx_range, drop=FALSE], na.rm=TRUE)
    qv        <- quantile(avg_nat, c(pct_threshold, 1 - pct_threshold), na.rm=TRUE)
    subsets   <- list(
      "Coldest 10%" = which(avg_nat <= qv[1]),
      "Hottest 10%"    = which(avg_nat >= qv[2])
    )

    for (lbl in names(subsets)) {
      idx    <- subsets[[lbl]]
      if (length(idx)==0) {
        zero_year <- NA_integer_
      } else {
        med_traj  <- apply(ems_mat[idx, , drop=FALSE], 2, median, na.rm=TRUE)
        zz        <- which(med_traj <= 0)
        zero_year <- if (length(zz)>0) years[min(zz)] else NA_integer_
      }
      res[[k]] <- list(
        start_year = start_year,
        duration   = dur,
        subset     = lbl,
        zero_year  = zero_year
      )
      k <- k + 1L
    }
  }
}

dt_heat <- rbindlist(res)[!is.na(zero_year)]



# ---- Compute net‐zero year for median of all runs ----
med_all <- apply(ems_mat, 2, median, na.rm=TRUE)
zz_all  <- which(med_all <= 0)
zero_year_all <- if (length(zz_all)>0) years[min(zz_all)] else NA_integer_


# ---- Plot heatmap ----
p <- ggplot(dt_heat, aes(x = start_year, y = duration, fill = zero_year)) +
  geom_tile() +
  scale_fill_viridis(name="Net-Zero Year", na.value="grey80", option = "C") +
  guides(fill = guide_colorbar(barwidth = unit(5, "cm"),  # increase width
                               barheight = unit(0.5, "cm"))) +
  facet_wrap(~ subset, ncol = 1) +
  labs(
    title = paste0("Heatmap of Median Emissions Net-Zero Year\n",
                   fig_suffix, " at ", pct_threshold*100, "% extremes\n",
                   "Median Year of Net Zero for All Runs is ", zero_year_all
                   ),
    x = "Starting Year of Extreme Period",
    y = "Duration of Extreme Period (years)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom"
    # aspect.ratio = 1
  )

# ---- Save figure ----
out_dir <- "../results/heatmaps"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
ggsave(
  filename = file.path(out_dir,
                       paste0("netzero_heatmap", fig_suffix, "_", pct_threshold*100, "pct.png")),
  plot   = p,
  width  = 8,
  height = 6
)




# ---- Build difference table ----
dt_diffzero <- copy(dt_heat)[
  , diff := zero_year - zero_year_all
]

# find symmetric limits
lim <- max(abs(dt_diffzero$diff), na.rm=TRUE)

# ---- Plot difference heatmap ----
p_diff <- ggplot(dt_diffzero, aes(x = start_year, y = duration, fill = diff)) +
  geom_tile() +
  facet_wrap(~ subset, ncol = 1) +
  scale_fill_stepsn(
    colors = brewer.pal(9, "RdBu"),
    limits = c(-5, 5),
    breaks = c(-5, -4 ,-3 ,-2 ,-1, 1, 2, 3, 4, 5),
    name   = "Subset - All Runs\n(Net-Zero Years)"
  ) +
  guides(fill = guide_colorbar(barwidth = unit(5, "cm"),  # increase width
                               barheight = unit(0.5, "cm"))) +
  labs(
    title = paste0(
      "Difference in Net-Zero Year of Subsets vs Median All Runs\n",
      fig_suffix, " at ", pct_threshold*100, "% extremes\n",
      "Median Year of Net Zero for All Runs is ", zero_year_all
    ),
    x = "Starting Year of Extreme Period",
    y = "Duration of Extreme Period (years)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid     = element_blank(),
    legend.position= "bottom"
    # aspect.ratio = 1
  )

# ---- Save difference figure ----
out_file_diff <- file.path(
  out_dir,
  paste0("netzero_heatmap_diff", fig_suffix, "_", pct_threshold*100, "pct.png")
)
message("Saving difference heatmap to: ", out_file_diff)
ggsave(out_file_diff, p_diff, width = 8, height = 6)
