library(data.table)
library(randomForest)
library(ggplot2)
library(ranger)

# ---- Setup ----
data_dir   <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix <- "_initClimSupportNormalDistribution"
fig_suffix = '_CESM_HR_local_natVar_multiplier1'
years      <- 2020:2100
pct_threshold <- 0.1

# ---- Load data ----
params_dt <- fread(file.path(data_dir, paste0("params",   fig_suffix, ".csv")))
if ( all(c("frac_neut_01","frac_opp_01") %in% names(params_dt)) ) {
  params_dt[, frac_supp_01 := 1 - (frac_neut_01 + frac_opp_01)]
}
ems_mat   <- as.matrix(fread(file.path(data_dir, paste0("emissions", fig_suffix, ".csv"))))
ems_vec   <- rowSums(ems_mat, na.rm = TRUE)

# ---- Load natvar and compute SD ----
natvar_mat <- as.matrix(fread(file.path(data_dir, paste0("natvar", fig_suffix, ".csv"))))
sd_natvar <- apply(natvar_mat, 1, sd, na.rm=TRUE)
q_vals    <- c(pct_threshold, 1 - pct_threshold)
q_thresh  <- quantile(sd_natvar, q_vals, na.rm=TRUE)
bottom_idx <- which(sd_natvar <= q_thresh[1])
top_idx    <- which(sd_natvar >= q_thresh[2])

# ---- Prepare data subsets ----
df_rf_bottom <- cbind(data.table(ems = ems_vec[bottom_idx]), params_dt[bottom_idx, ])
df_rf_top    <- cbind(data.table(ems = ems_vec[top_idx]),    params_dt[top_idx,    ])

names(df_rf_bottom) <- make.names(names(df_rf_bottom), unique = TRUE)
names(df_rf_top)    <- make.names(names(df_rf_top),    unique = TRUE)

nt <- parallel::detectCores(logical = TRUE) - 1
print(paste("Using", nt, "threads for Random Forest"))

# ---- Fit Random Forests ----
set.seed(123)
rf_bottom <- ranger(
  ems ~ .,
  data         = df_rf_bottom,
  num.trees    = 500,
  importance   = "permutation",
  num.threads  = nt
)
rf_top <- ranger(
  ems ~ .,
  data         = df_rf_top,
  num.trees    = 500,
  importance   = "permutation",
  num.threads  = nt
)
print("Random Forest models fitted successfully.")

# ---- Extract importance ----
imp_bottom <- importance(rf_bottom, type = "permutation")
imp_top    <- importance(rf_top,    type = "permutation")

imp_df_bottom <- data.table(
  Parameter  = names(imp_bottom),
  Importance = as.numeric(imp_bottom),
  Subset     = "Lowest 10% SD"
)
imp_df_top <- data.table(
  Parameter  = names(imp_top),
  Importance = as.numeric(imp_top),
  Subset     = "Highest 10% SD"
)

imp_df <- rbind(imp_df_bottom, imp_df_top)

# Reorder Parameter factor for plotting
imp_df[, Parameter := factor(Parameter, levels = rev(sort(unique(Parameter))))]

# ---- Plot barplot comparing importance ----
p_imp <- ggplot(imp_df, aes(x = Parameter, y = Importance, fill = Subset)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(
    values = c("Lowest 10% SD" = "#0072B2",   # blue
               "Highest 10% SD" = "#D55E00")  # red
  ) +
  coord_flip() +
  labs(
    title    = "Random Forest Variable Importance: Lowest vs Highest 10% SD",
    subtitle = paste0("Predicting Summed Emissions\nNatural variability extremes (SD): ", pct_threshold * 100, "%"),
    x        = "Parameter",
    y        = "Permutation Importance",
    fill     = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")  
  # xlim(0, 50000)

# ---- Save plot ----
out_dir    <- "../results/randomForest"
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(
  out_dir,
  paste0("ems_param_importance_LowVsHighSD", fig_suffix, ".png")
)
message("Saving importance plot to: ", out_file)
ggsave(out_file, p_imp, width = 10, height = 8)
