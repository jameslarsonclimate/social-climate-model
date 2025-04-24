library(data.table)

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

# Years of interest and their indices (assuming years = 2020:2100)
years <- 2020:2100
year_indices <- c(
  "2040" = which(years == 2040),
  "2060" = which(years == 2060),
  "2080" = which(years == 2080)
)

library(data.table)

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

# Initialize a matrix to store percentage of runs at zero emissions for every year
percentZeroMat <- matrix(NA_real_, 
                         nrow = length(experiment_suffixes), 
                         ncol = length(years))
colnames(percentZeroMat) <- as.character(years)
rownames(percentZeroMat) <- experiment_names

# Loop through each experiment and calculate percentages for each year (2020:2100)
for (i in seq_along(experiment_suffixes)) {
  suffix <- experiment_suffixes[i]
  emissions_file <- paste0("../results/MC Runs/MC Runs_TunedParams/emissions", suffix, ".csv")
  if (!file.exists(emissions_file)) {
    warning(paste("File not found:", emissions_file))
    next
  }
  ems <- fread(emissions_file)
  
  # Calculate percentage of runs with emissions <= 0 for every year
  percentZeroMat[i, ] <- sapply(seq_along(years), function(j) {
    mean(ems[[j]] <= 0, na.rm = TRUE) * 100
  })
}

# Compute percent differences relative to the default run (row with experiment_name == "default run")
default_vals <- percentZeroMat[experiment_names == "default run", ]
diffMat <- sweep(percentZeroMat, 2, default_vals, FUN = function(x, y) ((x - y) / y) * 100)

# Create summary tables showing only the selected years (2040, 2060, 2080)
sel_years <- as.character(years[unname(year_indices)])
results_tbl <- data.table(Experiment = experiment_names, percentZeroMat[, sel_years, drop = FALSE])
diff_tbl <- data.table(Experiment = experiment_names, diffMat[, sel_years, drop = FALSE])

# Print and optionally save the results tables
print(results_tbl)
fwrite(results_tbl, "../results/emissions_zero_percentages_by_experiment.csv")

print(diff_tbl)
fwrite(diff_tbl, "../results/emissions_percent_differences_by_experiment.csv")

# Two panel vertically stacked plot with a shared x-axis (years)
# Top: time series of percentage of runs at zero emissions
# Bottom: percent differences relative to the default run

# Convert the matrices to long format for ggplot2
library(data.table)
library(ggplot2)
library(patchwork)

# Prepare the Percent Zero data table
dtPercentZero <- as.data.table(percentZeroMat, keep.rownames = "Experiment")
dtPercentZero <- melt(dtPercentZero,
            id.vars = "Experiment",
            variable.name = "Year",
            value.name = "PercentZero")
dtPercentZero$Year <- as.numeric(as.character(dtPercentZero$Year))

# Prepare the Percent Difference data table
dtDiff <- as.data.table(diffMat, keep.rownames = "Experiment")
dtDiff <- melt(dtDiff,
         id.vars = "Experiment",
         variable.name = "Year",
         value.name = "PercentDifference")
dtDiff$Year <- as.numeric(as.character(dtDiff$Year))

# Top plot: Time series of percentage zero emissions
fig_top <- ggplot(dtPercentZero, aes(x = Year, y = PercentZero, color = Experiment)) +
  geom_line(linewidth = 0.9) +
  # (Optional) Add vertical lines if desired; adjust xintercept values accordingly
  # geom_vline(xintercept = c(2039, 2049), color = "black", alpha = 0.7) +
  labs(x = "",
     y = "Percent Zero Emissions",
     title = "Zero Emissions Time Series") +
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

# Combine the two plots vertically (with a shared legend)
fig <- fig_top / fig_bottom +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Print and save the combined plot
print(fig)
ggsave("../results/emissions_time_series.png", plot = fig, width = 8, height = 6)
