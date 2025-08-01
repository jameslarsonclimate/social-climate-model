library(data.table)
library(ggplot2)
library(patchwork)

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

# -----------------------------------------------------------
# Existing analysis: Percentage of runs with zero emissions
# -----------------------------------------------------------
# Initialize a matrix to store percentage zeros for every year
percentZeroMat <- matrix(NA_real_, 
                         nrow = length(experiment_suffixes), 
                         ncol = length(years))
colnames(percentZeroMat) <- as.character(years)
rownames(percentZeroMat) <- experiment_names

# Initialize matrices to store the lower and upper quantiles
lower10Mat <- matrix(NA_real_,
           nrow = length(experiment_suffixes),
           ncol = length(years))
upper90Mat <- matrix(NA_real_,
           nrow = length(experiment_suffixes),
           ncol = length(years))
colnames(lower10Mat) <- as.character(years)
rownames(lower10Mat) <- experiment_names
colnames(upper90Mat) <- as.character(years)
rownames(upper90Mat) <- experiment_names


# Loop through each experiment and calculate percentages for every year
for (i in seq_along(experiment_suffixes)) {
  suffix <- experiment_suffixes[i]
  emissions_file <- paste0("../results/MC Runs/MC Runs_TunedParams/emissions", suffix, ".csv")
  if (!file.exists(emissions_file)) {
    warning(paste("File not found:", emissions_file))
    next
  }
  ems <- fread(emissions_file)
  params <- fread(paste0("../results/MC Runs/MC Runs_TunedParams/params", suffix, ".csv"))

  # Define the mask for filtering
  mask <- params$Evidence > 0.2 & params$"Shifting Baselines" != 0
  mask_title = "Evidence>0.2_and_ShiftingBaselines!=0"

  # mask_title = ''
  # rm(mask)

  if (exists("mask")) {
    print(paste("Masking values outside", mask_title))
    ems <- ems[mask, , drop = FALSE]
    params <- params[mask, , drop = FALSE]
  }
  
  # Calculate percentage of runs with emissions <= 0 for every year
  percentZeroMat[i, ] <- sapply(seq_along(years), function(j) {
    mean(ems[[j]] <= 0, na.rm = TRUE) * 100
  })

  # Calculate lower (10%) and upper (90%) quantiles for each year
  lower10Mat[i, ] <- sapply(seq_along(years), function(j) {
  quantile(ems[[j]], probs = 0.1, na.rm = TRUE)
  })
  upper90Mat[i, ] <- sapply(seq_along(years), function(j) {
  quantile(ems[[j]], probs = 0.9, na.rm = TRUE)
  })

}

# Compute percent differences relative to the default run
default_vals <- percentZeroMat[experiment_names == "default run", ]
diffMat <- sweep(percentZeroMat, 2, default_vals, FUN = function(x, y) ((x - y) / y) * 100)

# Create summary tables showing only the selected years (2040, 2060, 2080)
sel_years <- as.character(years[unname(year_indices)])
results_tbl <- data.table(Experiment = experiment_names, percentZeroMat[, sel_years, drop = FALSE])
diff_tbl <- data.table(Experiment = experiment_names, diffMat[, sel_years, drop = FALSE])

# Print and save the results tables
# print(results_tbl)
# fwrite(results_tbl, "../results/emissions_zero_percentages_by_experiment.csv")

# print(diff_tbl)
# fwrite(diff_tbl, "../results/emissions_percent_differences_by_experiment.csv")

# Two panel vertically stacked plot with a shared x-axis
# Top: time series of percentage of runs at zero emissions
# Bottom: percent differences relative to the default run

# Convert the matrices to long format for ggplot2
dtPercentZero <- as.data.table(percentZeroMat, keep.rownames = "Experiment")
dtPercentZero <- melt(dtPercentZero,
                      id.vars = "Experiment",
                      variable.name = "Year",
                      value.name = "PercentZero")
dtPercentZero$Year <- as.numeric(as.character(dtPercentZero$Year))

dtDiff <- as.data.table(diffMat, keep.rownames = "Experiment")
dtDiff <- melt(dtDiff,
               id.vars = "Experiment",
               variable.name = "Year",
               value.name = "PercentDifference")
dtDiff$Year <- as.numeric(as.character(dtDiff$Year))

# Top plot: Time series of percentage zero emissions
fig_top <- ggplot(dtPercentZero, aes(x = Year, y = PercentZero, color = Experiment)) +
  geom_line(linewidth = 0.9) +
  labs(x = "",
       y = "Percent Zero Emissions",
       title = paste("Zero Emissions Time Series (", mask_title, ")", sep="")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Bottom plot: Percent differences relative to the default run
fig_bottom <- ggplot(dtDiff, aes(x = Year, y = PercentDifference, color = Experiment)) +
  geom_hline(yintercept = 1, color = "grey", size = 1.5) +
  geom_line(linewidth = 0.9) +
  labs(x = "Year",
       y = "Percent Difference (log scale)",
       title = "Percent Differences Relative to Default Run") +
  scale_y_log10(limits = c(0.1, 400)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Combine the two plots vertically with a shared legend
fig <- fig_top / fig_bottom +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal") &
  guides(color = guide_legend(ncol = 2))

print(fig)
ggsave(paste0("../results/emissions_time_series_", mask_title, ".png"), plot = fig, width = 8, height = 6)

# -----------------------------------------------------------
# New Analysis: 10% and 90% Quantiles of Emissions Pathways
# -----------------------------------------------------------
# # Initialize matrices to store the lower and upper quantiles
# lower10Mat <- matrix(NA_real_,
#            nrow = length(experiment_suffixes),
#            ncol = length(years))
# upper90Mat <- matrix(NA_real_,
#            nrow = length(experiment_suffixes),
#            ncol = length(years))
# colnames(lower10Mat) <- as.character(years)
# rownames(lower10Mat) <- experiment_names
# colnames(upper90Mat) <- as.character(years)
# rownames(upper90Mat) <- experiment_names

# # Loop through each experiment and calculate quantiles for every year
# for (i in seq_along(experiment_suffixes)) {
#   suffix <- experiment_suffixes[i]
#   emissions_file <- paste0("../results/MC Runs/MC Runs_TunedParams/emissions", suffix, ".csv")
#   if (!file.exists(emissions_file)) {
#   warning(paste("File not found:", emissions_file))
#   next
#   }
#   ems <- fread(emissions_file)
  
#   # Calculate lower (10%) and upper (90%) quantiles for each year
#   lower10Mat[i, ] <- sapply(seq_along(years), function(j) {
#   quantile(ems[[j]], probs = 0.1, na.rm = TRUE)
#   })
#   upper90Mat[i, ] <- sapply(seq_along(years), function(j) {
#   quantile(ems[[j]], probs = 0.9, na.rm = TRUE)
#   })
# }

# Calculate differences relative to the default run for the quantiles
defaultLower10 <- lower10Mat[experiment_names == "default run", ]
defaultUpper90 <- upper90Mat[experiment_names == "default run", ]
diffLower10Mat <- sweep(lower10Mat, 2, defaultLower10, FUN = "-")
diffUpper90Mat <- sweep(upper90Mat, 2, defaultUpper90, FUN = "-")

# Prepare long-format data for plotting quantiles
# For absolute quantiles:
dtLower <- as.data.table(lower10Mat, keep.rownames = "Experiment")
dtLower <- melt(dtLower,
        id.vars = "Experiment",
        variable.name = "Year",
        value.name = "Value")
dtLower[, Quantile := "Lower10"]

dtUpper <- as.data.table(upper90Mat, keep.rownames = "Experiment")
dtUpper <- melt(dtUpper,
        id.vars = "Experiment",
        variable.name = "Year",
        value.name = "Value")
dtUpper[, Quantile := "Upper90"]

dtQuantiles <- rbind(dtLower, dtUpper)
dtQuantiles[, Year := as.numeric(as.character(Year))]

# For differences relative to the default run:
dtDiffLower <- as.data.table(diffLower10Mat, keep.rownames = "Experiment")
dtDiffLower <- melt(dtDiffLower,
          id.vars = "Experiment",
          variable.name = "Year",
          value.name = "Value")
dtDiffLower[, Quantile := "Lower10"]

dtDiffUpper <- as.data.table(diffUpper90Mat, keep.rownames = "Experiment")
dtDiffUpper <- melt(dtDiffUpper,
          id.vars = "Experiment",
          variable.name = "Year",
          value.name = "Value")
dtDiffUpper[, Quantile := "Upper90"]

dtDiffQuantiles <- rbind(dtDiffLower, dtDiffUpper)
dtDiffQuantiles[, Year := as.numeric(as.character(Year))]

# -----------------------------------------------------------
# Create figure with two panels for quantile analyses
# -----------------------------------------------------------
# Top panel: Plot absolute quantiles over time
fig_quantiles_top <- ggplot(dtQuantiles, aes(x = Year, y = Value, color = Experiment)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ Quantile, scales = "free_y") +
  labs(x = "",
       y = "Emissions",
      title = paste("10% and 90% Quantiles of Emissions Pathways\n(", mask_title, ")", sep="")) +
  coord_cartesian(ylim = c(0, 22)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Bottom panel: Plot differences relative to default run over time
fig_quantiles_bottom <- ggplot(dtDiffQuantiles, aes(x = Year, y = Value, color = Experiment)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ Quantile, scales = "free_y") +
  labs(x = "Year",
     y = "Difference (Absolute)",
     title = "Quantile Differences Relative to Default Run") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Combine the two plots vertically with a shared legend
fig_quantiles <- fig_quantiles_top / fig_quantiles_bottom +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(fig_quantiles)
ggsave(paste0("../results/emissions_quantiles_", mask_title, ".png"), plot = fig_quantiles, width = 8, height = 8)



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
  mask <- params$Evidence > 0.2 & params$"Shifting Baselines" != 0
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
  geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.18, color = NA) +
  geom_line(aes(y = median), size = 1.1) +
  geom_line(aes(y = q05), linetype = "dashed", size = 0.7) +
  geom_line(aes(y = q95), linetype = "dashed", size = 0.7) +
  scale_color_manual(values = exp_colors) +
  scale_fill_manual(values = exp_colors) +
  labs(
    x = "",
    y = "Emissions (GtC per year)",
    title = "Median and 5–95% Quantile Range of Emissions Pathways"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Bottom panel: Difference vs default run
fig_quantiles_diff <- ggplot(quantile_diff_long, aes(x = year, group = Experiment, color = Experiment, fill = Experiment)) +
  geom_ribbon(aes(ymin = q05_diff, ymax = q95_diff), alpha = 0.18, color = NA) +
  geom_line(aes(y = median_diff), size = 1.1) +
  geom_line(aes(y = q05_diff), linetype = "dashed", size = 0.7) +
  geom_line(aes(y = q95_diff), linetype = "dashed", size = 0.7) +
  scale_color_manual(values = exp_colors[names(exp_colors) != "default run"]) +
  scale_fill_manual(values = exp_colors[names(exp_colors) != "default run"]) +
  labs(
    x = "Year",
    y = "Difference from Default (GtC per year)",
    title = "Difference in Median and 5–95% Quantiles vs Default Run"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# Combine the two panels
fig_combined <- fig_quantiles_all / fig_quantiles_diff +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.box = "horizontal") &
  guides(color = guide_legend(ncol = 2), fill = guide_legend(ncol = 2))

print(fig_combined)
ggsave(
  filename = paste0("../results/emissions_quantiles_all_and_diff", # fig_suffix, 
                    if (exists("mask_title") && mask_title != "") paste0("_", mask_title) else "", ".png"),
  plot = fig_combined,
  width = 10, height = 8
)


