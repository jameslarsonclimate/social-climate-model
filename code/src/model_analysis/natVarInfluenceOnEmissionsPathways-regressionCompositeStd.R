library(data.table)
library(ggplot2)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir         <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix = '_initClimSupportNormalDistribution'
fig_suffix = '_CESM_HR_local_natVar_multiplier1'
years            <- 2020:2100

# Optional standardization flags
standardize_params <- TRUE
standardize_ems    <- FALSE

# Analysis window and percentile threshold
pct_threshold  <- 0.1

# Labels for titles and filenames
pct_label      <- paste0(pct_threshold * 100, "pct")

# ---- Load data ----
params_dt <- fread(paste0(data_dir, "params",    fig_suffix, ".csv"))  # 100k × 22

# if both frac_neut_01 and frac_opp_01 exist, compute support as the remainder
if ( all(c("frac_neut_01","frac_opp_01") %in% names(params_dt)) ) {
  params_dt[, frac_supp_01 := 1 - (frac_neut_01 + frac_opp_01)]
}

ems_df    <- fread(paste0(data_dir, "emissions", fig_suffix, ".csv"))  # 100k × years
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",   fig_suffix, ".csv")))

# Integrated emissions per run
ems_vec <- rowSums(as.matrix(ems_df), na.rm=TRUE)
ems_dt  <- data.table(ems = ems_vec)

# ---- Identify bottom/top runs by SD of full timeseries ----
sd_natvar <- apply(natvar_mat, 1, sd, na.rm=TRUE)
q_vals    <- c(pct_threshold, 1 - pct_threshold)
q_thresh  <- quantile(sd_natvar, q_vals, na.rm=TRUE)

bottom_idx <- which(sd_natvar <= q_thresh[1])
top_idx    <- which(sd_natvar >= q_thresh[2])

# ---- Create Bottom & Top datasets ----
bottom_params_dt <- params_dt[bottom_idx, ]
bottom_ems_dt    <- data.table(ems = ems_vec[bottom_idx])

top_params_dt <- params_dt[top_idx, ]
top_ems_dt    <- data.table(ems = ems_vec[top_idx])

# ---- Univariate regression function ----
run_univariate <- function(p_dt, e_dt) {
  # Optional standardization
  if (standardize_params) p_dt <- as.data.table(lapply(p_dt, scale))
  if (standardize_ems)    e_dt[, ems := as.numeric(scale(ems))]
  # Prepare data
  y <- e_dt$ems
  X <- as.matrix(p_dt)
  n <- ncol(X)
  # Storage
  res <- data.table(
    Parameter = names(p_dt),
    Estimate  = numeric(n),
    R2        = numeric(n),
    p_value   = numeric(n)
  )
  # Loop through each parameter
  for (i in seq_len(n)) {
    sm <- summary(lm(y ~ X[, i]))
    cm <- sm$coefficients
    res[i, `:=`(
      Estimate = cm[2, 1],
      R2        = sm$r.squared,
      p_value   = cm[2, 4]
    )]
  }
  res
}

# ---- Run regressions for subsets ----
results_bottom <- run_univariate(bottom_params_dt, bottom_ems_dt)[, Subset := "Lowest 10% SD"]
results_top    <- run_univariate(top_params_dt,    top_ems_dt)[   , Subset := "Highest 10% SD"]

# Combine and set factor order
both_results <- rbind(results_bottom, results_top)

# ---- Plot Bottom vs Top slopes ----
p <- ggplot(both_results, aes(x = Parameter, y = Estimate, fill = Subset)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("Lowest 10% SD" = "#0072B2",   # blue
               "Highest 10% SD" = "#D55E00")  # red
  ) +
  coord_flip() +
  labs(
    title    = "Univariate Regression Slopes: Lowest vs Highest 10% SD",
    subtitle = paste0(
      "Integrated Emissions ~ Each Parameter\n",
      "Natural variability extremes (SD): ", pct_label
    ),
    x    = "Model Parameter",
    y    = "Slope Estimate",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  ylim(-150, 150)

# ---- Save plot ----
ggsave(
  filename = paste0("../results/regressions/paramRegression_LowVsHighSD", fig_suffix, ".png"),
  plot     = p,
  width    = 10,
  height   = 8
)
