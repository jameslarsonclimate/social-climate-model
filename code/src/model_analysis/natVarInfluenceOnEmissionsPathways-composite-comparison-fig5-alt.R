library(data.table)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

setwd('~/Documents/Research/social-climate-model/code/')
data_dir <- "/Users/jglarson/Documents/Research/social-climate-model/results/MC Runs/MC Runs_TunedParams/"
years    <- 2020:2100

fig_suffixes <- c(
  # '_originalMooreData',
  '_initClimSupportNormalDistribution',
  '_CESM_HR_local_natVar_multiplier1',
  # '_CESM_LM_local_Tambora_2030_defaultSupporterInitialDistribution',
  # '_initClimSupportNormalDistribution',
  # '_CESM_HR_local_natVar_defaultSupporterInitialDistribution',
  # '_CESM_LM_local_1850PIcntl_normalDistribution',
  # '_CESM_LM_global_member10_Tambora_2030_normalDistribution_multiplier1',
  '_CESM_LM_local_Tambora_2030_normalDistribution'
)

labels <- c(
  # "Original Moore Data",
  # "CESM LM Local Tambora 2030 Default Supporter",
  "Global Temperature Variability",
  "Pre-Industrial Local Temperature Variability",
  # "CESM HR Local NatVar Default Supporter",
  # "CESM LM Local 1850 PI Control Normal Distribution",
  # "CESM LM Global Member10 Tambora 2030 Normal Distribution",
  "CESM LM Tambora Eruption Local Variability"
)


colors <- brewer.pal(length(fig_suffixes), "Dark2")
# colors <- c("#1a936f", "#7570b3", "#5da70a", "#d95f02", "#dd0977", "#e6ab02", "#fff424")
linstyles <- c("solid", "dashed", "dotted", "dotdash", "twodash", "longdash")

# ---- Emissions ----
dt_long_ems <- rbindlist(lapply(seq_along(fig_suffixes), function(i) {
  suffix <- fig_suffixes[i]
  label  <- labels[i]
  ems    <- fread(paste0(data_dir, "emissions", suffix, ".csv"))
  mat    <- as.matrix(ems)
  data.table(
    Experiment = label,
    year       = years,
    median     = apply(mat, 2, median, na.rm=TRUE)
  )
}))
dt_long_ems$Experiment <- factor(dt_long_ems$Experiment, levels = labels)
p_ems <- ggplot(dt_long_ems, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Emissions (GtC/yr)", title="Median Emissions") +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom", legend.box="horizontal") +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))

# ---- Climate Supporters ----
dt_long_sup <- rbindlist(lapply(seq_along(fig_suffixes), function(i) {
  suffix <- fig_suffixes[i]
  label  <- labels[i]
  dist_file <- paste0(data_dir, "distributions", suffix, ".Rdata")
  load(dist_file)   # loads 'dist' array [run, year, group]
  supporters_mat <- dist[,,3]
  data.table(
    Experiment = label,
    year       = years,
    median     = apply(supporters_mat, 2, median, na.rm=TRUE)
  )
}))

# ---- Temperature (new third panel) ----
dt_long_temp <- rbindlist(lapply(seq_along(fig_suffixes), function(i) {
  suffix <- fig_suffixes[i]
  label  <- labels[i]
  temp   <- fread(paste0(data_dir, "temperature", suffix, ".csv"))
  tmat   <- as.matrix(temp)
  data.table(
    Experiment = label,
    year       = years,
    median     = apply(tmat, 2, median, na.rm=TRUE)
  )
}))

# Set factor levels for both data tables
dt_long_ems$Experiment  <- factor(dt_long_ems$Experiment,  levels = labels)
dt_long_sup$Experiment  <- factor(dt_long_sup$Experiment,  levels = labels)
dt_long_temp$Experiment <- factor(dt_long_temp$Experiment, levels = labels)

# Remove guides from individual plots
p_ems <- ggplot(dt_long_ems, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="Year", y="Emissions (GtC/yr)", title="Median Emissions") +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom", legend.box="horizontal")

p_sup <- ggplot(dt_long_sup, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Fraction of Climate Supporters", title="Median Fraction of Climate Supporters") +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom", legend.box="horizontal")

p_temp <- ggplot(dt_long_temp, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Temperature (°C)", title="Median Temperature (°C)") +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom", legend.box="horizontal")

# Combine and collect guides (legend)
fig_combined <- p_sup / p_ems / p_temp +
  plot_layout(guides="collect") &
  theme(legend.position="bottom", legend.box="horizontal") &
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))

ggsave(
  "../results/natvar_multidataset_3panel.png",
  fig_combined, width=7, height=13, dpi=300
)
