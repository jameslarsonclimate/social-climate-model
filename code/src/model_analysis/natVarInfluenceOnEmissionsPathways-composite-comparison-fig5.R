library(data.table)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

setwd('~/Documents/Research/social-climate-model/code/')
data_dir <- "/Users/jglarson/Documents/Research/social-climate-model/results/MC Runs/MC Runs_TunedParams/"
years    <- 2020:2100

fig_suffixes <- c(
  '_initClimSupportNormalDistribution',
  '_CESM_HR_local_natVar_multiplier1',
  '_CESM_LM_local_Tambora_2030_normalDistribution'
)

labels <- c(
  "Pre-industrial global mean temperature variability",
  "Pre-industrial local temperature variability",
  "Volcanic local temperature variability"
)


# colors <- brewer.pal(length(fig_suffixes), "Dark2")
colors <- c("#000000", brewer.pal(8, "Dark2")[2:3], brewer.pal(8, "Dark2")[6])

# colors <- brewer.pal(8, "Dark2")[c(1, 8, 3)]
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
  labs(x="Year", y="Emissions (GtC/yr)") + #, title="Emissions (GtC/yr)") +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  theme_minimal(base_size=14) +
  theme(
    legend.position="none",
    panel.grid.minor = element_blank()
  )


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

# Set factor levels for both data tables
dt_long_ems$Experiment  <- factor(dt_long_ems$Experiment,  levels = labels)
dt_long_sup$Experiment  <- factor(dt_long_sup$Experiment,  levels = labels)

# Remove guides from individual plots 
p_sup <- ggplot(dt_long_sup, aes(year, median, color=Experiment, fill=Experiment)) +
  geom_line(size=1.1) +
  scale_color_manual(values=colors) +
  scale_fill_manual(values=colors) +
  labs(x="", y="Fraction of Climate Supporters") + #, title="Median Fraction of Climate Supporters") +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  theme_minimal(base_size=14) +
  theme(
    legend.position="none",
    panel.grid.minor = element_blank()
  )

p_sup <- p_sup + annotate("text", x = -Inf, y = Inf, label = "a",
                          fontface = "bold", hjust = -2, vjust = 1.7, size = 6)

p_ems <- p_ems + annotate("text", x = -Inf, y = Inf, label = "b",
                          fontface = "bold", hjust = -2, vjust = 1.7, size = 6)


# Combine and collect guides (legend)
# fig_combined <- p_sup / p_ems +
  # plot_layout(guides="collect") &
  # # plot_annotation(tag_levels = 'a') &
  # theme(legend.position="bottom", legend.box="horizontal"
  #       # plot.tag = element_text(face = "bold"),        # bold tag text
  #       # plot.tag.position = c(0.91, 0.93)              # top-left inside panels
  #       ) &
  # guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))

  # Combine without legend
fig_combined <- p_sup / p_ems +
  plot_layout(guides="collect")


figwidth <- 6
figheight <- 7.5

ggsave(
  "../results/manuscriptFigures/natvar_multidataset_2panel.png",
  fig_combined, width=figwidth, height=figheight, dpi=300
)

ggsave(
  "../results/manuscriptFigures/natvar_multidataset_2panel.pdf",
  fig_combined, width=figwidth, height=figheight, dpi=300
)

