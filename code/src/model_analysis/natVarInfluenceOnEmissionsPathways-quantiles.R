library(data.table)
library(ggplot2)
library(patchwork)

setwd('~/Documents/Research/social-climate-model/code')

# List your experiment suffixes here (add empty string for default run)
experiment_suffixes <- c(
  "",  # This will be labeled as "default run" in the results table
  "_noNatVar",
  "_pulseTempAnom_2K_2030-2040",
  "_pulseTempAnom_2K_2070-2080",
  "_fixedNatVar-lackOfClimateSupport",
  "_fixedNatVar-mediumClimateSupport",
  "_fixedNatVar-highClimateSupport"
)


# Replace blank experiment names with "default run"
experiment_names <- ifelse(experiment_suffixes == "", "default run", experiment_suffixes)

# Years of interest and their indices (assuming years = 2020:2100)
years <- 2020:2100
year_indices <- c(
  "2040" = which(years == 2040),
  "2060" = which(years == 2060),
  "2080" = which(years == 2080)
)

mask <- params$Evidence > 0.2 & params$"Shifting Baselines" != 0
mask_title = "Evidence>0.2_and_ShiftingBaselines!=0"

rm(mask)
rm(mask_title)


######################################################################################################

# -----------------------------------------------------------
# Two-panel figure: quantiles for all experiments and differences vs default
# -----------------------------------------------------------

# Prepare long-format data for all experiments
# Calculate median, 5%, and 95% quantiles for each experiment and year
quantile_long <- lapply(seq_along(experiment_suffixes), function(i) {
  suffix <- experiment_suffixes[i]
  exp_name <- experiment_names[i]
  emissions_file <- paste0("../results/MC Runs/MC Runs_TunedParams/emissions", suffix, ".csv")
  if (!file.exists(emissions_file)) return(NULL)
  ems <- fread(emissions_file)
  params <- fread(paste0("../results/MC Runs/MC Runs_TunedParams/params", suffix, ".csv"))
  
  if (exists("mask")) ems <- ems[mask, , drop = FALSE]
  data.frame(
    Experiment = exp_name,
    year = years,
    median = apply(ems, 2, median, na.rm = TRUE),
    q05 = apply(ems, 2, quantile, probs = 0.05, na.rm = TRUE),
    q95 = apply(ems, 2, quantile, probs = 0.95, na.rm = TRUE)
  )
})
quantile_long <- do.call(rbind, quantile_long)

# Get default run quantiles for difference plot
default_quant <- subset(quantile_long, Experiment == "default run")

# Calculate differences for each experiment vs default run
quantile_diff_long <- merge(
  quantile_long, 
  default_quant[, c("year", "median", "q05", "q95")], 
  by = "year", suffixes = c("", "_default")
)
quantile_diff_long$median_diff <- quantile_diff_long$median - quantile_diff_long$median_default
quantile_diff_long$q05_diff <- quantile_diff_long$q05 - quantile_diff_long$q05_default
quantile_diff_long$q95_diff <- quantile_diff_long$q95 - quantile_diff_long$q95_default

# Remove default run from difference plot (difference is always zero)
quantile_diff_long <- subset(quantile_diff_long, Experiment != "default run")

# Set up color palette for experiments
exp_colors <- RColorBrewer::brewer.pal(max(3, length(experiment_names)), "Set1")
names(exp_colors) <- experiment_names

# Top panel: All experiments, median and quantile ribbons
fig_quantiles_all <- ggplot(quantile_long, aes(x = year, group = Experiment, color = Experiment, fill = Experiment)) +
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = Experiment), alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(aes(y = median, color = Experiment), size = 1.1, show.legend = TRUE) +
  geom_line(aes(y = q05), linetype = "dotted", size = 0.7, show.legend = FALSE) +
  geom_line(aes(y = q95), linetype = "dashed", size = 0.7, show.legend = FALSE) +
  scale_color_manual(values = exp_colors) +
  scale_fill_manual(values = exp_colors) +
  labs(
    x = "",
    y = "Emissions (GtC per year)",
    title = "Median and 5–95% Quantile Range of Emissions Pathways"
  ) +
  theme_minimal(base_size = 14)

# Bottom panel: Difference vs default run
fig_quantiles_diff <- ggplot(quantile_diff_long, aes(x = year, group = Experiment, color = Experiment, fill = Experiment)) +
  geom_ribbon(aes(ymin = q05_diff, ymax = q95_diff, fill = Experiment), alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(aes(y = median_diff, color = Experiment), size = 1.1, show.legend = FALSE) +
  geom_line(aes(y = q05_diff), linetype = "dotted", size = 0.7, show.legend = FALSE) +
  geom_line(aes(y = q95_diff), linetype = "dashed", size = 0.7, show.legend = FALSE) +
  scale_color_manual(values = exp_colors[names(exp_colors) != "default run"]) +
  scale_fill_manual(values = exp_colors[names(exp_colors) != "default run"]) +
  labs(
    x = "Year",
    y = "Difference from Default (GtC per year)",
    title = "Difference in Median and 5–95% Quantiles vs Default Run"
  ) +
  theme_minimal(base_size = 14)

# Combine the two panels
fig_combined <- fig_quantiles_all / fig_quantiles_diff +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal") &
  guides(
    color = guide_legend(ncol = 2, override.aes = list(linetype = 1, size = 1.1)),
    fill = guide_legend(ncol = 2)
  )

print(fig_combined)
ggsave(
  filename = paste0("../results/emissions_quantiles_all_and_diff", # fig_suffix, 
                    if (exists("mask_title") && mask_title != "") paste0("_", mask_title) else "", ".png"),
  plot = fig_combined,
  width = 8, height = 10
)



# --- Policy Analysis ---
quantile_long_pol <- lapply(seq_along(experiment_suffixes), function(i) {
  suffix <- experiment_suffixes[i]
  exp_name <- experiment_names[i]
  policy_file <- paste0("../results/MC Runs/MC Runs_TunedParams/policy", suffix, ".csv")
  if (!file.exists(policy_file)) return(NULL)
  pol <- fread(policy_file)
  params <- fread(paste0("../results/MC Runs/MC Runs_TunedParams/params", suffix, ".csv"))
  
  if (exists("mask")) pol <- pol[mask, , drop = FALSE]
  data.frame(
    Experiment = exp_name,
    year = years,
    median = apply(pol, 2, median, na.rm = TRUE),
    q05 = apply(pol, 2, quantile, probs = 0.05, na.rm = TRUE),
    q95 = apply(pol, 2, quantile, probs = 0.95, na.rm = TRUE)
  )
})
quantile_long_pol <- do.call(rbind, quantile_long_pol)
default_quant_pol <- subset(quantile_long_pol, Experiment == "default run")
quantile_diff_long_pol <- merge(
  quantile_long_pol, 
  default_quant_pol[, c("year", "median", "q05", "q95")], 
  by = "year", suffixes = c("", "_default")
)
quantile_diff_long_pol$median_diff <- quantile_diff_long_pol$median - quantile_diff_long_pol$median_default
quantile_diff_long_pol$q05_diff <- quantile_diff_long_pol$q05 - quantile_diff_long_pol$q05_default
quantile_diff_long_pol$q95_diff <- quantile_diff_long_pol$q95 - quantile_diff_long_pol$q95_default
quantile_diff_long_pol <- subset(quantile_diff_long_pol, Experiment != "default run")

fig_quantiles_all_pol <- ggplot(quantile_long_pol, aes(x = year, group = Experiment, color = Experiment, fill = Experiment)) +
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = Experiment), alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(aes(y = median, color = Experiment), size = 1.1, show.legend = TRUE) +
  geom_line(aes(y = q05), linetype = "dotted", size = 0.7, show.legend = FALSE) +
  geom_line(aes(y = q95), linetype = "dashed", size = 0.7, show.legend = FALSE) +
  scale_color_manual(values = exp_colors) +
  scale_fill_manual(values = exp_colors) +
  labs(
    x = "",
    y = "Policy Value",
    title = "Median and 5–95% Quantile Range of Policy Pathways"
  ) +
  theme_minimal(base_size = 14)

fig_quantiles_diff_pol <- ggplot(quantile_diff_long_pol, aes(x = year, group = Experiment, color = Experiment, fill = Experiment)) +
  geom_ribbon(aes(ymin = q05_diff, ymax = q95_diff, fill = Experiment), alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(aes(y = median_diff, color = Experiment), size = 1.1, show.legend = FALSE) +
  geom_line(aes(y = q05_diff), linetype = "dotted", size = 0.7, show.legend = FALSE) +
  geom_line(aes(y = q95_diff), linetype = "dashed", size = 0.7, show.legend = FALSE) +
  scale_color_manual(values = exp_colors[names(exp_colors) != "default run"]) +
  scale_fill_manual(values = exp_colors[names(exp_colors) != "default run"]) +
  labs(
    x = "Year",
    y = "Difference from Default (Policy Value)",
    title = "Difference in Median and 5–95% Quantiles vs Default Run (Policy)"
  ) +
  theme_minimal(base_size = 14)

fig_combined_pol <- fig_quantiles_all_pol / fig_quantiles_diff_pol +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal") &
  guides(
    color = guide_legend(ncol = 2, override.aes = list(linetype = 1, size = 1.1)),
    fill = guide_legend(ncol = 2)
  )

ggsave(
  filename = paste0("../results/policy_quantiles_all_and_diff", # fig_suffix, 
                    if (exists("mask_title") && mask_title != "") paste0("_", mask_title) else "", ".png"),
  plot = fig_combined_pol,
  width = 8, height = 10
)



# --- Temperature Analysis ---

quantile_long_temp <- lapply(seq_along(experiment_suffixes), function(i) {
  suffix <- experiment_suffixes[i]
  exp_name <- experiment_names[i]
  temp_file <- paste0("../results/MC Runs/MC Runs_TunedParams/temperature", suffix, ".csv")
  if (!file.exists(temp_file)) return(NULL)
  climtemp <- fread(temp_file)
  params <- fread(paste0("../results/MC Runs/MC Runs_TunedParams/params", suffix, ".csv"))
  

  if (exists("mask")) climtemp <- climtemp[mask, , drop = FALSE]
  data.frame(
    Experiment = exp_name,
    year = years,
    median = apply(climtemp, 2, median, na.rm = TRUE),
    q05 = apply(climtemp, 2, quantile, probs = 0.05, na.rm = TRUE),
    q95 = apply(climtemp, 2, quantile, probs = 0.95, na.rm = TRUE)
  )
})
quantile_long_temp <- do.call(rbind, quantile_long_temp)
default_quant_temp <- subset(quantile_long_temp, Experiment == "default run")
quantile_diff_long_temp <- merge(
  quantile_long_temp, 
  default_quant_temp[, c("year", "median", "q05", "q95")], 
  by = "year", suffixes = c("", "_default")
)
quantile_diff_long_temp$median_diff <- quantile_diff_long_temp$median - quantile_diff_long_temp$median_default
quantile_diff_long_temp$q05_diff <- quantile_diff_long_temp$q05 - quantile_diff_long_temp$q05_default
quantile_diff_long_temp$q95_diff <- quantile_diff_long_temp$q95 - quantile_diff_long_temp$q95_default
quantile_diff_long_temp <- subset(quantile_diff_long_temp, Experiment != "default run")

fig_quantiles_all_temp <- ggplot(quantile_long_temp, aes(x = year, group = Experiment, color = Experiment, fill = Experiment)) +
  geom_ribbon(aes(ymin = q05, ymax = q95, fill = Experiment), alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(aes(y = median, color = Experiment), size = 1.1, show.legend = TRUE) +
  geom_line(aes(y = q05), linetype = "dotted", size = 0.7, show.legend = FALSE) +
  geom_line(aes(y = q95), linetype = "dashed", size = 0.7, show.legend = FALSE) +
  scale_color_manual(values = exp_colors) +
  scale_fill_manual(values = exp_colors) +
  labs(
    x = "",
    y = "Temperature (°C)",
    title = "Median and 5–95% Quantile Range of Temperature Pathways"
  ) +
  theme_minimal(base_size = 14)

fig_quantiles_diff_temp <- ggplot(quantile_diff_long_temp, aes(x = year, group = Experiment, color = Experiment, fill = Experiment)) +
  geom_ribbon(aes(ymin = q05_diff, ymax = q95_diff, fill = Experiment), alpha = 0.18, color = NA, show.legend = FALSE) +
  geom_line(aes(y = median_diff, color = Experiment), size = 1.1, show.legend = FALSE) +
  geom_line(aes(y = q05_diff), linetype = "dotted", size = 0.7, show.legend = FALSE) +
  geom_line(aes(y = q95_diff), linetype = "dashed", size = 0.7, show.legend = FALSE) +
  scale_color_manual(values = exp_colors[names(exp_colors) != "default run"]) +
  scale_fill_manual(values = exp_colors[names(exp_colors) != "default run"]) +
  labs(
    x = "Year",
    y = "Difference from Default (°C)",
    title = "Difference in Median and 5–95% Quantiles vs Default Run (Temperature)"
  ) +
  theme_minimal(base_size = 14)

fig_combined_temp <- fig_quantiles_all_temp / fig_quantiles_diff_temp +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal") &
  guides(
    color = guide_legend(ncol = 2, override.aes = list(linetype = 1, size = 1.1)),
    fill = guide_legend(ncol = 2)
  )

ggsave(
  filename = paste0("../results/temperature_quantiles_all_and_diff", # fig_suffix, 
                    if (exists("mask_title") && mask_title != "") paste0("_", mask_title) else "", ".png"),
  plot = fig_combined_temp,
  width = 8, height = 10
)