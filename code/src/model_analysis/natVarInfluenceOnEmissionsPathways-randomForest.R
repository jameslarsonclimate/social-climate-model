library(data.table)
library(randomForest)
library(ggplot2)
library(ranger) 

# ---- Setup ----
data_dir   <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix <- "_initClimSupportNormalDistribution"

# ---- Load data ----
params_dt <- fread(file.path(data_dir, paste0("params",   fig_suffix, ".csv")))

# if both frac_neut_01 and frac_opp_01 exist, compute support as the remainder
if ( all(c("frac_neut_01","frac_opp_01") %in% names(params_dt)) ) {
  params_dt[, frac_supp_01 := 1 - (frac_neut_01 + frac_opp_01)]
}

ems_mat   <- as.matrix(fread(file.path(data_dir, paste0("emissions", fig_suffix, ".csv"))))
# sum emissions across years
ems_dt    <- data.table(ems = rowSums(ems_mat, na.rm = TRUE))

# ---- Combine predictors and response ----
df_rf <- cbind(ems_dt, params_dt)

names(df_rf) <- make.names(names(df_rf), unique = TRUE)

# ---- Fit Random Forest ----
set.seed(123)
# rf_model <- randomForest(
#   ems ~ .,
#   data       = df_rf,
#   importance = TRUE,
#   ntree      = 500
# )

nt <- parallel::detectCores(logical = TRUE) - 1
print(paste("Using", nt, "threads for Random Forest"))

rf_model <- ranger(
  ems ~ .,
  data         = df_rf,
  num.trees    = 500,
  importance   = "permutation",
  num.threads  = nt
)
print("Random Forest model fitted successfully.")

# # ---- Extract and format importance ----
# imp_mat <- importance(rf_model, type = 1)  # Mean decrease in accuracy
# imp_df <- data.table(
#   Parameter  = rownames(imp_mat),
#   Importance = imp_mat[, 1]
# )

# ---- Extract importance ----
imp <- importance(rf_model, type = "permutation")
imp_df <- data.table(
  Parameter  = names(imp),
  Importance = as.numeric(imp)
)#[order(-Importance)]

# # sort descending
# imp_df <- imp_df[order(-Importance)]
# reorder Parameter factor so that when flipped, labels run in reverse alpha
imp_df[, Parameter := factor(
  Parameter,
  levels = rev(sort(unique(Parameter)))
)]

# ---- Plot top 10 parameters ----
n_top <- min(10, nrow(imp_df))
p_imp <- ggplot(imp_df, aes( #[1:n_top], aes(
  x = reorder(Parameter, Importance),
  y = Importance
)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title    = "Random Forest Variable Importance",
    subtitle = paste0("Predicting Summed Emissions", fig_suffix),
    x        = "Parameter",
    y        = "Permutation Importance"
  ) +
  theme_minimal(base_size = 12)

# ---- Save plot ----
out_dir    <- "../results/randomForest"
out_file <- file.path(
  out_dir,
  paste0("ems_param_importance", fig_suffix, ".png")
)
message("Saving importance plot to: ", out_file)
ggsave(out_file, p_imp, width = 8, height = 6)

