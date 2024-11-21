# Capture the start time
start_time <- Sys.time()
options(error = quote({ traceback(); dump.frames(); }))

print('Running, running, running, running away')
setwd('~/Documents/Research/social-climate-model/code')

library(ggplot2)
library(forcats)
library(metR)
library(geomtextpath)
library(patchwork) # For combining plots

# Create a sequence of 81 numbers (you can adjust the length as needed)
natvar1 <- rep(0, 81)
natvar1[seq(10, 81, by = 10)] <- 5

coeff = 0.1

# Define the temperature matrix with 81 rows and 2 columns
mat <- array(0, dim=c(length(time), 5))

# First column values: start at 1, rise to 3 by index 41, drop to 1.5 by index 46, and stay at 1.5
mat[1:41, 2] <- seq(1, 3, length.out = 41)   # Values from 1 to 3 by index 41
mat[42:46, 2] <- seq(3, 1.5, length.out = 5) # Drop from 3 to 1.5 from index 41 to 46
mat[47:81, 2] <- 1.5                        # Stay at 1.5 from index 46 to 81

# for (natvar_it in seq(0, 20, 2)) {
# for (natvar_it in c(5)){

  # fileSaveSuffix = ""
  # fileSaveSuffix = paste0("-natvar_", natvar_it) 
  # fileSaveSuffix = natvar_it

source("src/model.R")  # Load the model script
homophily_param01 = 0.7
m = model(shiftingbaselines = 0, evidenceeffect=0.015)# , natvar=TRUE, historical=TRUE) #, natvar_multiplier=8, temperature_input=mat)

# # Create a data frame with all the variables
# data <- data.frame(
#   time = seq(2020, 2100, length.out = 81),       # Time from 2020 to 2100
#   emissions = m$emissions,                       # Emissions data
#   totalemissions = m$total_emissions,  #rowSums(m$emissions),             # Total emissions data
#   temperature = m$temp[,1],                      # Surface temperature (first column)
#   evidence = m$evidence[,1,],                     # Evidence data (first column)
#   anomaly = m$anomaly,                           # Climate anomaly
#   opposed = m$distributions[,1,],                 # Opposed distribution
#   neutral = m$distributions[,2,],                 # Neutral distribution
#   support = m$distributions[,3,]                  # Support distribution
# )


# Create list to store plots
plot_list <- list()

# Create custom titles
subplot_titles <- c(
  "OECD Emissions", "Asia Emissions", "LAM Emissions", 
  "MAF Emissions", "REF Emissions"
)

for(r in 1:5) {

# Create a data frame with all the variables
data <- data.frame(
  time = seq(2020, 2100, length.out = 81),       # Time from 2020 to 2100
  emissions = m$emissions[,r],                       # Emissions data
  total_emissions = m$total_emissions,  #rowSums(m$emissions),             # Total emissions data
  temperature = m$temp[,1],                      # Surface temperature (first column)
  evidence = m$evidence[,1,r],                     # Evidence data (first column)
  weather = m$weather[,r],                         # Weather
  opposed = m$distributions[,1,r],                 # Opposed distribution
  neutral = m$distributions[,2,r],                 # Neutral distribution
  support = m$distributions[,3,r]                  # Support distribution
)


# current_data <- subset(data, m$year >= year_start & m$year <= year_end)

# Create individual plot
plot_list[[r]] = ggplot(data, aes(x = m$year)) +
  # Emissions lines
  geom_textline(aes(y = emissions, color = "emissions"), label="emissions", linetype = "solid", linewidth = 0.7, vjust = -0.15, size=2) +
  geom_textline(aes(y = total_emissions, color = "Total emissions"), label="Total", linetype = "solid", linewidth = 0.7, vjust=-0.15, size=2) +
  # geom_textline(aes(y = m$emissions_outside[,1], color = "Asia Emissions"), label="Asia", linetype = "solid", linewidth = 0.7, vjust=-0.15, size=2) +
  # geom_textline(aes(y = m$emissions_outside[,2], color = "LAM Emissions"), label="LAM", linetype = "solid", linewidth = 0.7, vjust=-0.15, size=2) +
  # geom_textline(aes(y = m$emissions_outside[,3], color = "MAF Emissions"), label="MAF", linetype = "solid", linewidth = 0.7, vjust=-0.3, size=2) +
  # geom_textline(aes(y = m$emissions_outside[,4], color = "Ref Emissions"), label="REF", linetype = "solid", linewidth = 0.7, vjust=1.5, size=2) +
  
  # Temperature-related lines
  geom_line(aes(y = temperature, color = "Temperature"), linetype = "dotdash", linewidth = 0.9) +
  # geom_line(aes(y = evidence, color = "Evidence"), linewidth = 0.9) +
  geom_line(aes(y = weather, color = "Weather"), linetype = "dotdash", linewidth = 0.9) +
  
  # Population distribution lines
  geom_textline(aes(y = opposed/coeff, color = "Opposed"), label="Opp", linetype = "longdash", linewidth = 0.9, vjust=-0.15, size=2) +
  geom_textline(aes(y = neutral/coeff, color = "Neutral"), label="Neu", linetype = "longdash", linewidth = 0.9, vjust=-0.15, size=2) +
  geom_textline(aes(y = support/coeff, color = "Support"), label="Sup", linetype = "longdash", linewidth = 0.9, vjust=1.5, size=2) +
  
  scale_y_continuous(
    name = "Emissions, Temperature, Evidence, and Anomaly",
    limits = c(-1, 21), 
    sec.axis = sec_axis(~. * coeff, name = "Population Distributions")
  ) +
  labs(
    x = "Year",
    title = subplot_titles[r]
  ) +
  scale_color_manual(
    values = c(
      "Emissions" = "#6a040f", "Total emissions" = "#e85d04",
      # "Asia Emissions" = "#d00000", "LAM Emissions" = "#e85d04",
      # "MAF Emissions" = "#f48c06", "Ref Emissions" = "#ffba08",
      "Temperature" = "#85C1E9", "Evidence" = "#caf0f8",
      "Anomaly" = "#f4acb7", "Opposed" = "#ee4a70",
      "Neutral" = "#8d99ae", "Support" = "#06d667"
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
}

# Combine all plots
combined_plot <- wrap_plots(plot_list, ncol = 3)

# Save combined plot
ggsave("../results/timeSeries-subplots.png", plot=combined_plot, 
      width=10, height=6, dpi=300)


# fig = ggplot(data, aes(x = m$year)) +
#     # Emissions lines
#     # geom_line(aes(y = m$emissions, color = "OECD emissions"), linetype = "solid", linewidth = 0.7) +
#     geom_textline(aes(y = m$emissions, color = "OECD emissions"), label="OECD emissions", linetype = "solid", linewidth = 0.7, vjust = -0.15, size=3, hjust = 0.2)+
#     geom_textline(aes(y = m$totalemissions, color = "Total emissions"), label="Total emissions", linetype = "solid", linewidth = 0.7, vjust=-0.15, size=3, hjust=0.1) +
#     geom_textline(aes(y = m$emissions_outside[,1], color = "Asia Emissions"), label="Asia emissions", linetype = "solid", linewidth = 0.7, vjust=-0.15, size=3) +
#     geom_textline(aes(y = m$emissions_outside[,2], color = "LAM Emissions"), label="LAM emissions", linetype = "solid", linewidth = 0.7, vjust=-0.15, size=3) +
#     geom_textline(aes(y = m$emissions_outside[,3], color = "MAF Emissions"), label="MAF emissions", linetype = "solid", linewidth = 0.7, vjust=-0.3, size=3) +
#     geom_textline(aes(y = m$emissions_outside[,4], color = "Ref Emissions"), label="REF emissions", linetype = "solid", linewidth = 0.7, vjust=1.5, size=3) +

#     # Temperature-related lines
#     geom_line(aes(y = m$temp[,1], color = "Temperature"), linetype = "dotdash", linewidth = 0.9) +
#     geom_line(aes(y = m$evidence[,1], color = "Evidence"), linewidth = 0.9) +
#     geom_line(aes(y = m$anomaly, color = "Anomaly"), linetype = "dotdash", linewidth = 0.9) +

#     # Population distribution lines
#     geom_textline(aes(y = m$distributions[,1]/coeff, color = "Opposed"), label="Opposed", linetype = "longdash", linewidth = 0.9, vjust=-0.15, size=3, hjust=0.2) +
#     geom_textline(aes(y = m$distributions[,2]/coeff, color = "Neutral"), label="Neutral", linetype = "longdash", linewidth = 0.9, vjust=-0.15, size=3, hjust=0.2) +
#     geom_textline(aes(y = m$distributions[,3]/coeff, color = "Support"), label="Support", linetype = "longdash", linewidth = 0.9, vjust=1.5, size=3, hjust=0.2) +

#     # Adjust y-axes
#     scale_y_continuous(
#       name = "Emissions, Temperature, Evidence, and Anomaly",
#       limits = c(-1, 21), 
#       sec.axis = sec_axis(~. * coeff, name = "Population Distributions")
#     ) +
    
#     # Labeling and title
#     labs(
#       x = "Year",
#       y = "Model output",
#       title = "Model Outputs over Time (2020 to 2100)"
#     ) +

#     # Updated color scheme using distinct, dark-pastel colors
#     scale_color_manual(
#       values = c(
#         "OECD emissions" = "#6a040f",       # Dark-pastel red
#         "Total emissions" = "#5f0f40",      # Dark-pastel purple
#         "Asia Emissions" = "#d00000",       # Dark-pastel green
#         "LAM Emissions" = "#e85d04",        # Dark-pastel blue
#         "MAF Emissions" = "#f48c06",        # Dark-pastel brown
#         "Ref Emissions" = "#ffba08",        # Dark-pastel red
#         "Temperature" = "#85C1E9",          # Light-pastel blue
#         "Evidence" = "#caf0f8",             # Light-pastel green
#         "Anomaly" = "#f4acb7",              # Light-pastel pink
#         "Opposed" = "#ee4a70",              # Dark-pastel red-pink
#         "Neutral" = "#8d99ae",              # Dark-pastel gray
#         "Support" = "#06d667"               # Dark-pastel gray
#       )
#     ) +
#     theme_minimal() +
#     theme(
#       legend.title = element_blank(),
#       # Set white background for panel and plot
#       panel.background = element_rect(fill = "white", color = NA),
#       plot.background = element_rect(fill = "white", color = NA),
#       panel.grid.major = element_line(color = "grey90"), # Optional: make grid lines light grey
#       panel.grid.minor = element_blank() # Optional: hide minor grid lines
#     )

#   # ggsave(paste("../results/default", fileSaveSuffix,"-timeSeries.png", sep=""), plot=fig)
#   ggsave(paste("../results/default-timeSeries.png", sep=""), plot=fig, width=16/2, height=9/2)


