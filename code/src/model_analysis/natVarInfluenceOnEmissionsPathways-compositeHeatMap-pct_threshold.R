library(data.table)
library(ggplot2)
library(viridis)
library(RColorBrewer)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir    <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix  <- "_initClimSupportNormalDistribution"
years       <- 2020:2100
start_year  <- 2020
max_dur     <- length(years) - (start_year - min(years))
pct_values  <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5)

# ---- Load data ----
message("Loading emissions…")
ems_mat    <- as.matrix(fread(paste0(data_dir, "emissions", fig_suffix, ".csv")))
message("Loading natvar…")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Compute global net‐zero year ----
med_all  <- apply(ems_mat, 2, median, na.rm=TRUE)
zz_all   <- which(med_all <= 0)
zero_all <- if (length(zz_all)>0) years[min(zz_all)] else NA_integer_

# ---- Pre‐allocate storage matrices ----
n_t <- length(pct_values)
n_d <- max_dur
bottom_zero <- matrix(NA_integer_, nrow=n_t, ncol=n_d,
                      dimnames=list(NULL, 1:n_d))
top_zero    <- matrix(NA_integer_, nrow=n_t, ncol=n_d,
                      dimnames=list(NULL, 1:n_d))

# ---- Fill matrices ----
for (j in seq_len(n_d)) {
  dur      <- j
  end_year <- start_year + dur - 1
  if (end_year > max(years)) break
  idx_range <- which(years>=start_year & years<=end_year)
  avg_nat   <- rowMeans(natvar_mat[, idx_range, drop=FALSE], na.rm=TRUE)
  for (i in seq_along(pct_values)) {
    pct <- pct_values[i]
    qv  <- quantile(avg_nat, c(pct, 1-pct), na.rm=TRUE)
    # bottom subset
    idx_b <- which(avg_nat <= qv[1])
    if (length(idx_b)>0) {
      med_traj <- apply(ems_mat[idx_b, , drop=FALSE], 2, median, na.rm=TRUE)
      zz       <- which(med_traj <= 0)
      bottom_zero[i,j] <- if (length(zz)>0) years[min(zz)] else NA_integer_
    }
    # top subset
    idx_t <- which(avg_nat >= qv[2])
    if (length(idx_t)>0) {
      med_traj <- apply(ems_mat[idx_t, , drop=FALSE], 2, median, na.rm=TRUE)
      zz       <- which(med_traj <= 0)
      top_zero[i,j]    <- if (length(zz)>0) years[min(zz)] else NA_integer_
    }
  }
}

# ---- Melt into long table ----
dt_heat <- rbind(
  data.table(
    pct_thresh = pct_values,
    as.data.table(bottom_zero)
  )[,   melt(.SD, id.vars="pct_thresh", variable.name="duration",
            value.name="zero_year")[, subset:="Coldest"]],
  data.table(
    pct_thresh = pct_values,
    as.data.table(top_zero)
  )[,   melt(.SD, id.vars="pct_thresh", variable.name="duration",
            value.name="zero_year")[, subset:="Hottest"]]
)
dt_heat[, duration := as.integer(duration)]
dt_heat[, pct_thresh := factor(pct_thresh, levels=pct_values)]

# ---- Plot net‐zero year heatmap ----
p_thr <- ggplot(dt_heat,
                aes(x=pct_thresh, y=duration, fill=zero_year)) +
  geom_tile() +
  facet_wrap(~ subset, ncol=1) +
  scale_fill_viridis(name="Net-Zero Year", option="C", na.value="grey80") +
  guides(fill=guide_colorbar(barwidth=unit(5,"cm"),
                             barheight=unit(0.5,"cm"))) +
  labs(
    title = paste0(
      "Net-Zero Year by Percentile Threshold & Duration\n",
      fig_suffix, "\n",
      "Median Year of Net Zero for All Runs is ", zero_all
    ),
    x = "Percentile Threshold",
    y = "Duration (years)"
  ) +
  theme_minimal(base_size=12) +
  theme(
    panel.grid     = element_blank(),
    legend.position= "bottom",
    axis.text.x    = element_text(angle=45, hjust=1)
  ) 

# ---- Save heatmap ----
out_dir    <- "../results/heatmaps"
dir.create(out_dir, showWarnings=FALSE, recursive=TRUE)
out_file   <- file.path(out_dir,
                        paste0("netzero_heatmap_thr", fig_suffix, ".png"))
message("Saving net-zero heatmap: ", out_file)
ggsave(out_file, p_thr, width=8, height=6)

# ---- Build and plot difference heatmap ----
dt_diff <- copy(dt_heat)[, diff := zero_year - zero_all]
lim     <- max(abs(dt_diff$diff), na.rm=TRUE)

p_diff <- ggplot(dt_diff,
                 aes(x=pct_thresh, y=duration, fill=diff)) +
  geom_tile() +
  facet_wrap(~ subset, ncol=1) +
  scale_fill_stepsn(
    colors = rev(brewer.pal(9,"RdBu")),
    limits = c(-lim, lim),
    breaks = seq(-lim, lim, length.out=9),
    name   = "Subset − All Runs\n(Net-Zero Years)"
  ) +
  guides(fill=guide_colorbar(barwidth=unit(5,"cm"),
                             barheight=unit(0.5,"cm"))) +
  labs(
    title = paste0(
      "Difference in Net-Zero Year vs All Runs\n",
      fig_suffix
    ),
    x = "Percentile Threshold",
    y = "Duration (years)"
  ) +
  theme_minimal(base_size=12) +
  theme(
    panel.grid     = element_blank(),
    legend.position= "bottom",
    axis.text.x    = element_text(angle=45, hjust=1)
  ) 

out_file_diff <- file.path(out_dir,
                           paste0("netzero_heatmap_thr_diff", fig_suffix, ".png"))
message("Saving difference heatmap: ", out_file_diff)
ggsave(out_file_diff, p_diff, width=8, height=6)