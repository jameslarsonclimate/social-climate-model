library(data.table)
library(ggplot2)

# ---- Setup ----
setwd('~/Documents/Research/social-climate-model/code')
data_dir         <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix       <- '_initClimSupportNormalDistribution'
years            <- 2020:2100

standardize_params <- TRUE
standardize_ems    <- FALSE

# ---- Load data ----
params_dt <- fread(paste0(data_dir, "params",    fig_suffix, ".csv"))
if (all(c("frac_neut_01","frac_opp_01") %in% names(params_dt))) {
  params_dt[, frac_supp_01 := 1 - (frac_neut_01 + frac_opp_01)]
}
ems_df  <- fread(paste0(data_dir, "emissions", fig_suffix, ".csv"))
ems_mat <- as.matrix(ems_df)

# Integrated & final‐year emissions
ems_vec   <- rowSums(ems_mat, na.rm=TRUE)
final_ems <- ems_mat[, ncol(ems_mat)]

# ---- Identify Net-Zero vs Non-Zero runs ----
zero_idx     <- which(final_ems <= 0)
positive_idx <- which(final_ems >  0)

zero_params_dt     <- params_dt[zero_idx]
zero_ems_dt        <- data.table(ems = ems_vec[zero_idx])
positive_params_dt <- params_dt[positive_idx]
positive_ems_dt    <- data.table(ems = ems_vec[positive_idx])

# ---- Univariate regression function ----
run_univariate <- function(p_dt, e_dt) {
  if (standardize_params) p_dt <- as.data.table(lapply(p_dt, scale))
  if (standardize_ems)    e_dt[, ems := as.numeric(scale(ems))]
  y  <- e_dt$ems
  X  <- as.matrix(p_dt)
  n  <- ncol(X)
  res<- data.table(
    Parameter = names(p_dt),
    Estimate  = numeric(n),
    R2        = numeric(n),
    p_value   = numeric(n)
  )
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

# ---- Run regressions for each subset ----
res_zero <- run_univariate(zero_params_dt,     zero_ems_dt)[, Subset := "Zero by Year End"]
res_pos  <- run_univariate(positive_params_dt, positive_ems_dt)[, Subset := "Positive at Year End"]

both_results <- rbind(res_zero, res_pos)
# both_results[, Parameter := factor(Parameter, levels = names(params_dt))]

# ---- Plot Zero vs Positive slopes ----
p <- ggplot(both_results, aes(x = Parameter, y = Estimate, fill = Subset)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("Zero by Year End"     = "#0072B2",
               "Positive at Year End" = "#D55E00")
  ) +
  coord_flip() +
  labs(
    title    = "Univariate Regression Slopes: Net-Zero vs Non-Zero by Year End",
    subtitle = "Integrated Emissions ~ Each Parameter\nGroups defined by final-year emissions",
    x        = "Model Parameter",
    y        = "Slope Estimate",
    fill     = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  ylim(-150, 150)

# ---- Save plot ----
ggsave(
  filename = paste0("../results/regressions/paramRegression_NetZeroVsNonZero", fig_suffix, ".png"),
  plot     = p,
  width    = 10,
  height   = 8
)