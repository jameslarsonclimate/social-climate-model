library(data.table)
library(ggplot2)
library(RColorBrewer)

setwd('~/Documents/Research/social-climate-model/code')
data_dir <- "../results/MC Runs/MC Runs_TunedParams/"
years    <- 2020:2100

fig_suffixes <- c(
  # '_originalMooreData',
  # '_initClimSupportNormalDistribution',
  # '_CESM_HR_local_natVar_multiplier1',
  # '_CESM_HR_local_natVar_defaultSupporterInitialDistribution',
  # '_CESM_LM_local_Tambora_2030_normalDistribution',
  # '_CESM_LM_local_1850PIcntl_normalDistribution'
  # '_CESM_LM_global_member10_Tambora_2030_normalDistribution_multiplier1'
  # '_CESM_LM_local_Tambora_2030_defaultSupporterInitialDistribution'
  '_CESM_HR_local_natVar_multiplier1',
  '_CESM_HR_local_natVar-totalGDPweighted',
  '_CESM_HR_local_natVar-popWeighted'
)

labels <- c(
  # "Original Moore Data",
  # "Init Climate Support Normal Distribution",
  # "CESM HR Local NatVar Normal Distribution",
  # "CESM HR Local NatVar Default Supporter",
  # "CESM LM Local Tambora 2030 Normal Distribution",
  # "CESM LM Local 1850 PI Control Normal Distribution"
  # "CESM LM Global Member10 Tambora 2030 Normal Distribution"
  # "CESM LM Local Tambora 2030 Default Supporter"
  "CESM HR Local NatVar Normal Distribution",
  "CESM HR Local NatVar Total GDP Weighted",
  "CESM HR Local NatVar Population Weighted"
)

colors <- brewer.pal(length(fig_suffixes), "Dark2")

# ---- Gather natural variability values ----
natvar_list <- lapply(seq_along(fig_suffixes), function(i) {
  suffix <- fig_suffixes[i]
  label  <- labels[i]
  natvar <- fread(paste0(data_dir, "natvar", suffix, ".csv"))
  mat    <- as.matrix(natvar)
  data.table(
    value = as.vector(mat),
    Experiment = label
  )
})
dt_natvar <- rbindlist(natvar_list)

# ---- Overlapping histograms of natural variability values ----
p_natvar_hist <- ggplot(dt_natvar, aes(x=value, fill=Experiment, color=Experiment)) +
  geom_histogram(position="identity", alpha=0.4, bins=60, size=1.2) +
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  labs(x="Natural Variability Value", y="Count", title="Distribution of Natural Variability Values Across Datasets") +
  xlim(-2, 2) +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom") +
  guides(fill=guide_legend(ncol=1), color=guide_legend(ncol=1))

ggsave("../results/natvar_multidataset_histogram.png", p_natvar_hist, width=8, height=6)

# ---- Overlapping density plots of natural variability values ----
p_natvar_density <- ggplot(dt_natvar, aes(x=value, color=Experiment, fill=Experiment)) +
  geom_density(alpha=0.3, size=1.2) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="Natural Variability Value", y="Density", title="Density of Natural Variability Values Across Datasets") +
  xlim(-2, 2) +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom") +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))

ggsave("../results/natvar_multidataset_density.png", p_natvar_density, width=8, height=6)


# ---- Gather standard deviations of natural variability for each run ----
natvar_sd_list <- lapply(seq_along(fig_suffixes), function(i) {
  suffix <- fig_suffixes[i]
  label  <- labels[i]
  natvar <- fread(paste0(data_dir, "natvar", suffix, ".csv"))
  mat    <- as.matrix(natvar)
  sd_vals <- apply(mat, 1, sd, na.rm=TRUE)
  data.table(
    sd = sd_vals,
    Experiment = label
  )
})
dt_natvar_sd <- rbindlist(natvar_sd_list)

# ---- Overlapping histograms of standard deviations ----
p_natvar_sd_hist <- ggplot(dt_natvar_sd, aes(x=sd, fill=Experiment, color=Experiment)) +
  geom_histogram(position="identity", alpha=0.4, bins=50, size=1.2) + # Increased outline thickness
  scale_fill_manual(values=colors) +
  scale_color_manual(values=colors) +
  labs(x="Standard Deviation of Natural Variability (per run)", y="Count", title="Distribution of Natural Variability SD Across Datasets") +
  xlim(0, 1.5) +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom") +
  guides(fill=guide_legend(ncol=1), color=guide_legend(ncol=1))

ggsave("../results/natvar_multidataset_sd_histogram.png", p_natvar_sd_hist, width=8, height=6)

# ---- Overlapping density plots of standard deviations ----
p_natvar_sd_density <- ggplot(dt_natvar_sd, aes(x=sd, color=Experiment, fill=Experiment)) +
  geom_density(alpha=0.3, size=1.2, adjust=1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="Standard Deviation of Natural Variability (per run)", y="Density", title="Density of Natural Variability SD Across Datasets") +
  xlim(0, 1.5) +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom") +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))

ggsave("../results/natvar_multidataset_sd_density.png", p_natvar_sd_density, width=8, height=6)


# ---- Compare two specific datasets: Tambora 2030 vs 1850 PI control ----
compare_suffixes <- c(
  '_CESM_LM_local_Tambora_2030_normalDistribution',
  '_CESM_LM_local_1850PIcntl_normalDistribution'
)
compare_labels <- c(
  "CESM LM Local Tambora 2030 Normal Distribution",
  "CESM LM Local 1850 PI Control Normal Distribution"
)
compare_colors <- brewer.pal(2, "Set2")

compare_natvar_list <- lapply(seq_along(compare_suffixes), function(i) {
  suffix <- compare_suffixes[i]
  label  <- compare_labels[i]
  natvar <- fread(paste0(data_dir, "natvar", suffix, ".csv"))
  mat    <- as.matrix(natvar)
  data.table(
    value = as.vector(mat),
    Experiment = label
  )
})
dt_compare_natvar <- rbindlist(compare_natvar_list)

p_compare_hist <- ggplot(dt_compare_natvar, aes(x=value, fill=Experiment, color=Experiment)) +
  geom_histogram(position="identity", alpha=0.4, bins=60, size=1.2) +
  scale_fill_manual(values=compare_colors) +
  scale_color_manual(values=compare_colors) +
  labs(
    x="Natural Variability Value",
    y="Count",
    title="Comparison: Tambora 2030 vs 1850 PI Control (CESM LM Local, Normal Distribution)"
  ) +
  xlim(-2, 2) +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom") +
  guides(fill=guide_legend(ncol=1), color=guide_legend(ncol=1))

ggsave("../results/natvar_compare_Tambora2030_vs_1850PIcntl_histogram.png", p_compare_hist, width=8, height=6)


# ---- Compare standard deviations per run: Tambora 2030 vs 1850 PI control ----
compare_natvar_sd_list <- lapply(seq_along(compare_suffixes), function(i) {
  suffix <- compare_suffixes[i]
  label  <- compare_labels[i]
  natvar <- fread(paste0(data_dir, "natvar", suffix, ".csv"))
  mat    <- as.matrix(natvar)
  sd_vals <- apply(mat, 1, sd, na.rm=TRUE)
  data.table(
    sd = sd_vals,
    Experiment = label
  )
})
dt_compare_natvar_sd <- rbindlist(compare_natvar_sd_list)

p_compare_sd_hist <- ggplot(dt_compare_natvar_sd, aes(x=sd, fill=Experiment, color=Experiment)) +
  geom_histogram(position="identity", alpha=0.4, bins=50, size=1.2) +
  scale_fill_manual(values=compare_colors) +
  scale_color_manual(values=compare_colors) +
  labs(
    x="Standard Deviation of Natural Variability (per run)",
    y="Count",
    title="SD Comparison: Tambora 2030 vs 1850 PI Control (CESM LM Local, Normal Distribution)"
  ) +
  xlim(0, 1.5) +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom") +
  guides(fill=guide_legend(ncol=1), color=guide_legend(ncol=1))

ggsave("../results/natvar_compare_Tambora2030_vs_1850PIcntl_sd_histogram.png", p_compare_sd_hist, width=8, height=6)

