#           natVarInfluenceOnEmissionsPathways-compositeHeatMap-numHotYears.R

library(data.table)
library(ggplot2)
library(viridis)

# ---- Setup ----
data_dir    <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix  <- "_initClimSupportNormalDistribution"
start_year  <- 2020
years       <- 2020:2100
max_dur     <- length(years) - (start_year - min(years))
threshold   <- 0.2

# ---- Load data ----
ems_mat    <- as.matrix(fread(
  paste0(data_dir, "emissions", fig_suffix, ".csv")
))
natvar_mat <- as.matrix(fread(
  paste0(data_dir, "natvar",    fig_suffix, ".csv")
))

# ---- Compute per‐run net‐zero year ----
# first year when emissions ≤ 0
zero_run <- apply(ems_mat, 1, function(em){
  zz <- which(em <= 0)
  if(length(zz)>0) years[min(zz)] else NA_integer_
})

# ---- Build table of median net‐zero by count of hot/cold years ----
res <- list()
k   <- 1L
for(dur in seq_len(max_dur)) {
  idx_cols <- which(
    years >= start_year &
    years <= start_year + dur - 1
  )
  # count hot years (> threshold) and cold years (< -threshold)
  hot_count  <- rowSums(natvar_mat[, idx_cols, drop=FALSE] >  threshold,
                        na.rm=TRUE)
  cold_count <- rowSums(natvar_mat[, idx_cols, drop=FALSE] < -threshold,
                        na.rm=TRUE)
  for(sub in c("Hot Years","Cold Years")) {
    cnt_vec <- if(sub=="Hot Years") hot_count else cold_count
    # iterate all possible counts 0:dur
    for(cnt in 0:dur) {
      runs <- which(cnt_vec==cnt)
      med_z <- if(length(runs)>0) median(zero_run[runs],
                                         na.rm=TRUE) else NA_integer_
      res[[k]] <- list(
        duration  = dur,
        count     = cnt,
        subset    = sub,
        zero_year = med_z
      )
      k <- k + 1L
    }
  }
}

dt_heat <- rbindlist(res)

# ---- Plot heatmap ----
p <- ggplot(dt_heat,
            aes(x = count, y = duration, fill = zero_year)) +
  geom_tile() +
  facet_wrap(~ subset, ncol=2) +
  guides(fill = guide_colorbar(barwidth = unit(5, "cm"),  # increase width
                               barheight = unit(0.5, "cm"))) +
  scale_fill_viridis(name="Net-Zero Year", limits = c(2084, 2091), oob = scales::oob_squish,  # Allow colors outside the range to be squished
                     option="C", na.value="grey90") +
  labs(
    title = paste0(
      "Median Net-Zero Year by Number of Hot/Cold Years\n",
      fig_suffix, "\nThreshold = ", threshold, " °C"
    ),
    x = "Number of Years over Threshold",
    y = "Duration of Period (years)"
  ) +
  theme_minimal(base_size=12) +
  theme(
    panel.grid     = element_blank(),
    legend.position= "bottom"
  ) 

# ---- Save plot ----
out_dir  <- "../results/heatmaps"
dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)
out_file <- file.path(
  out_dir,
  paste0("netzero_heatmap_numHotColdYears",
         fig_suffix, "_thr", threshold, ".png")
)
message("Saving heatmap to: ", out_file)
ggsave(out_file, p, width=8, height=6)
