library(data.table)
library(ggplot2)

# ---- Setup ----
# fig_suffix         <- "_initClimSupport40percent"
# fig_suffix = ''
# fig_suffix = '_pulseTempAnom_2K_2030-2040'
# fig_suffix = '_noNatVar'
# fig_suffix = '_varyInitialDistribution'
fig_suffix = '_initClimSupportNormalDistribution'




data_dir           <- "../results/MC Runs/MC Runs_TunedParams/"
title_suffix       <- fig_suffix
params_file        <- paste0(data_dir, "params",   fig_suffix, ".csv")
ems_file           <- paste0(data_dir, "emissions",fig_suffix, ".csv")
natvar_mat <- as.matrix(fread(paste0(data_dir, "natvar",    fig_suffix, ".csv")))

# ---- Natural variability ----
natvar_window <- natvar_mat[, 6:25]  # years 2025:2035
natvar_sd  <- apply(natvar_window, 1, sd, na.rm = TRUE)   # (100000,)
natvar_avg <- rowMeans(natvar_window, na.rm = TRUE)       # (100000,)


print(params_file)
print(ems_file)

# ---- Load data ----
params_dt <- fread(params_file)            # 100000 × 22 (or x 24)

# if both frac_neut_01 and frac_opp_01 exist, compute support as the remainder
if ( all(c("frac_neut_01","frac_opp_01") %in% names(params_dt)) ) {
  params_dt[, frac_supp_01 := 1 - (frac_neut_01 + frac_opp_01)]
}

# ---- Add natural variability metrics to params_dt ----
params_dt[, natvar_sd_2025_2035  := natvar_sd]
params_dt[, natvar_avg_2025_2035 := natvar_avg]

ems       <- fread(ems_file)               # single-column of length 100000
ems_dt <- data.table(ems = rowSums(as.matrix(fread(ems_file)), na.rm=TRUE))
setnames(ems_dt, names(ems_dt), "ems")     

# ---- Optional standardization ----
standardize_params <- TRUE  # standardize model parameters
standardize_ems    <- FALSE  # standardize emissions

if (standardize_params) params_dt <- as.data.table(lapply(params_dt, scale))
if (standardize_ems)    ems_dt[, ems := as.numeric(scale(ems))]

# ---- Combine for regression ----
# extract response vector and predictor matrix
y <- ems_dt$ems
X <- as.matrix(params_dt)

# ---- Univariate regressions by column index ----
n_params <- ncol(X)
results <- data.table(
  Index     = seq_len(n_params),
  Estimate  = numeric(n_params),
  R2        = numeric(n_params),
  p_value   = numeric(n_params)
)

for (i in seq_len(n_params)) {
  fit <- lm(y ~ X[, i])
  sm  <- summary(fit)
  # row 2 of coefficients is the slope for X[,i]
  coef_mat     <- sm$coefficients
  results[i, `:=`(
    Estimate = coef_mat[2, 1],
    R2        = sm$r.squared,
    p_value   = coef_mat[2, 4]
  )]
}

# map numeric Index → actual parameter names
results[, Parameter := names(params_dt)[Index]]

# # ---- Plot coefficients from univariate regressions ----
# # 'results' must contain Parameter (factor) and Estimate
# p <- ggplot(results, aes(x = Parameter, y = Estimate)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(
#     title = paste0(
#       if (standardize_params && standardize_ems) "Standardized coefficients\n" else
#       if (standardize_params)    "Coefficients (standardized params)\n" else
#       if (standardize_ems)       "Coefficients (standardized ems)\n" else
#                                   "Univariate regression slopes\n",
#       "Sum of Emissions ~ Each Parameter"
#     ),
#     x = "Model Parameter",
#     y = "Slope Estimate (GtC/σ)"
#   ) +
#   theme_minimal(base_size = 14) +
#   ylim(-165, 150)


# # ---- Save plot ----
# ggsave(filename = paste0("../results/regressions/paramRegression", fig_suffix, ".jpg"), plot = p, width = 8, height = 6)



# # ---- Scatterplot: Integrated Emissions vs Evidence (param column 4) ----
# # extract raw x, y
# x <- params_dt[[2]]    # Evidence is the 4th column
# y <- ems_dt$ems

# # apply optional standardization
# if (standardize_params) x <- as.numeric(scale(x))
# if (standardize_ems)    y <- as.numeric(scale(y))

# # fit univariate regression
# fit_evd <- lm(y ~ x)
# slope   <- coef(fit_evd)[2]

# # assemble data.frame
# df_evd <- data.table(Evidence = x, Emissions = y)

# # plot scatter + regression line
# p_evd <- ggplot(df_evd, aes(x = Evidence, y = Emissions)) +
#   geom_point(alpha = 0.03) +
#   geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 1) +
#   labs(
#     title = paste0(
#       "Emissions vs Evidence (col 4) — slope = ",
#       round(slope, 3)
#     ),
#     x = paste0(if (standardize_params) "Std. " else "", "Evidence"),
#     y = paste0(if (standardize_ems)    "Std. " else "", "Integrated Emissions")
#   ) +
#   theme_minimal(base_size = 14)

# # save plot
# ggsave(
#   filename = paste0("../results/regressions/ems_vs_evidence", fig_suffix, ".png"),
#   plot     = p_evd,
#   width    = 6,
#   height   = 5
# )


# ---- Prepare correlation labels ----
# compute signed correlation coefficient from R²
results[, corr := sign(Estimate) * sqrt(R2)]
# horizontal justification: put labels just outside bar ends
results[, hjust := ifelse(Estimate >= 0, -0.1, 1.1)]

# ---- Plot coefficients with R correlation labels ----
p <- ggplot(results, aes(x = Parameter, y = Estimate)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = sprintf("%.2f", corr), hjust = hjust),
    size = 3,
    color = "black"
  ) +
  coord_flip() +
  labs(
    title = paste0(
      if (standardize_params && standardize_ems) "Standardized coefficients\n" else
      if (standardize_params)    "Coefficients (standardized params)\n" else
      if (standardize_ems)       "Coefficients (standardized ems)\n" else
                                  "Univariate regression slopes\n",
      "Sum of Emissions ~ Each Parameter"
    ),
    x = "Model Parameter",
    y = "Slope Estimate (GtC/σ)"
  ) +
  theme_minimal(base_size = 14) + 
  ylim(-150, 150)


# ---- Save plot ----
ggsave(
  filename = paste0("../results/regressions/paramRegressionWithRcoef", fig_suffix, ".png"),
  plot     = p,
  width    = 8,
  height   = 6
)



# ---- Scatterplots & linear fits for all parameters ----
# assume y <- ems_dt$ems and params_dt already loaded & optionally standardized

# 1. combine emissions and all 22 parameters into one data.table
dt_all <- cbind(
  Emissions = y,
  params_dt
)

# 2. melt to long format for faceting
dt_long <- melt(
  dt_all,
  id.vars       = "Emissions",
  measure.vars  = names(params_dt),
  variable.name = "Parameter",
  value.name    = "Value"
)

setDT(dt_long)   # now it’s a data.table

# 3. make Parameter a factor in the original order
dt_long[, Parameter := factor(Parameter, levels = names(params_dt))]

# 4. plot with one panel per parameter
p_all <- ggplot(dt_long, aes(x = Value, y = Emissions)) +
  geom_point(alpha = 0.01) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 0.8) +
  facet_wrap(~ Parameter, scales = "free_x", ncol = 4) +
  labs(
    title = "Integrated Emissions vs Each Model Parameter",
    x     = "Parameter Value",
    y     = "Integrated Emissions"
  ) +
  theme_minimal(base_size = 12)

# 5. save figure
ggsave(
  filename = paste0("../results/regressions/ems_vs_all_params", fig_suffix, ".png"),
  plot     = p_all,
  width    = 12,
  height   = 10
)