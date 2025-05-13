library(ggplot2)
library(plot.matrix)
library(data.table)
library(tidyverse)
library(reshape2)
library(ggridges)

setwd("/Users/jglarson/Documents/Research/social-climate-model/code")
source("src/model_analysis/model_parametertune.R")

fig_suffix = '_pulseTempAnom_2K_2030-2040'
# fig_suffix = '_pulseTempAnom_2K_2070-2080'
# fig_suffix = ''
# fig_suffix = '_noNatVar'
# fig_suffix = '_fixedNatVar-lackOfClimateSupport'
# fig_suffix = '_fixedNatVar-mediumClimateSupport'
# fig_suffix = '_fixedNatVar-highClimateSupport'
fig_suffix = "_varyInitialDistribution"



# ---- Load data ----
data_dir <- "../results/MC Runs/MC Runs_TunedParams/"
title_suffix = fig_suffix
print(paste0(data_dir, "emissions",    fig_suffix, ".csv"))
ems    <- fread(paste0(data_dir, "emissions",    fig_suffix, ".csv"))
print(paste0(data_dir, "emissions",    fig_suffix, ".csv"))



plot_decadal_emission_ridges <- function(ems_data, title_suffix = "") {
  # ensure matrix
  ems_mat <- if (inherits(ems_data, "data.table") || inherits(ems_data, "data.frame")) {
    as.matrix(ems_data)
  } else ems_data
  
  # select decades
  years <- 2020:2100
  decade_years   <- seq(2020, 2100, by = 10)
  decade_indices <- match(decade_years, years)
  
  # build long data.frame
  n_runs <- nrow(ems_mat)
  df <- data.frame(
    emission = as.vector(ems_mat[, decade_indices]),
    decade   = factor(
      rep(decade_years, each = n_runs),
      levels = rev(decade_years)
    )
  )
  
  # plot
p <- ggplot(df, aes(x = emission, y = decade)) +
  ggridges::geom_density_ridges_gradient(
    aes(fill = after_stat(x)),
    scale = 1.2, rel_min_height = 0.01,
    color = "black", alpha = 0.8, linewidth = 0.2,      # thinner outline
  ) +
  scale_fill_viridis_c(name = "Emissions\n(GtC/yr)", option = "C") +
    labs(
      title    = paste0("Emissions Distributions at Start of Each Decade",
                        if (title_suffix != "") paste0("\n", title_suffix) else "\n"),
      subtitle = paste0(nrow(ems_mat), " simulated pathways"),
      x        = "Emissions (GtC per year)",
      y        = "Decade"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      axis.text.y     = element_text(size = 12),
      plot.title      = element_text(hjust = 0.5),
      plot.subtitle   = element_text(hjust = 0.5)
    ) +
    xlim(-1.5, 23)
  
  return(p)
}

# ---- Example usage & save ----
ridge_plot <- plot_decadal_emission_ridges(ems, fig_suffix)
ggsave(
  filename = paste0("../results/emissions_decadal_ridges", 
                    if (title_suffix!="" ) paste0("_", title_suffix) else "", ".png"),
  plot     = ridge_plot,
  width    = 8, height = 6
)
