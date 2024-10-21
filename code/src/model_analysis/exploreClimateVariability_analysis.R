# Capture the start time
start_time <- Sys.time()

print('Running, running, running, running away')
setwd('~/Documents/Research/social-climate-model/code')

library(ggplot2)
library(forcats)
library(metR)

# Create a sequence of 81 numbers (you can adjust the length as needed)
natvar1 <- rep(0, 81)
natvar1[seq(10, 81, by = 10)] <- 5

coeff = 0.1

# for (natvar_it in seq(0, 20, 2)) {
for (natvar_it in c(5)){

  # fileSaveSuffix = ""
  # fileSaveSuffix = paste0("-natvar_", natvar_it) 
  fileSaveSuffix = natvar_it

  source("src/model.R")  # Load the model script
  homophily_param01 = 0.7
  m = model(shiftingbaselines = 0, natvar_multiplier=8)

  # Create a data frame with all the variables
  data <- data.frame(
    time = seq(2020, 2100, length.out = 81),       # Time from 2020 to 2100
    emissions = m$emissions,                       # Emissions data
    totalemissions = m$totalemissions,             # Total emissions data
    temperature = m$temp[,1],                      # Surface temperature (first column)
    evidence = m$evidence[,1],                     # Evidence data (first column)
    anomaly = m$anomaly,                           # Climate anomaly
    opposed = m$distributions[,1],                 # Opposed distribution
    neutral = m$distributions[,2],                 # Neutral distribution
    support = m$distributions[,3]                  # Support distribution
  )


  # Plot each variable using ggplot2
  fig = ggplot(data, aes(x = m$year)) +
                geom_line(aes(y = m$emissions, color = "emissions")) +
                geom_line(aes(y = m$totalemissions, color = "totalemissions")) +
                geom_line(aes(y = m$temp[,1], color = "temperature")) +
                geom_line(aes(y = m$evidence[,1], color = "evidence")) +
                geom_line(aes(y = m$anomaly, color = "anomaly"), linetype = "dashed") +
                # Secondary y-axis variables, rescaled to fit within the range of 0 to 1.5 on the secondary axis
                geom_line(aes(y = m$distributions[,1]/coeff, color = "opposed")) +
                geom_line(aes(y = m$distributions[,2]/coeff, color = "neutral")) +
                geom_line(aes(y = m$distributions[,3]/coeff, color = "support")) +
                
                # Combine both primary and secondary axes into a single scale_y_continuous call
                scale_y_continuous(
                  # Features of the first axis (primary y-axis)
                  name = "Emissions, Total Emissions, Temperature, Evidence, and Anomaly",
                  limits = c(-3, 30),  # Set the limits for the primary y-axis
                  
                  # Add a second axis and specify its features (secondary y-axis)
                  sec.axis = sec_axis(~. * coeff, name = "Population Distributions")
                ) +
                labs(
                  x = "Year",
                  y = "Model output",
                  title = "Model Outputs over Time (2020 to 2100)"
                ) +
                scale_color_manual(
                  values = c("emissions" = "red", 
                            "totalemissions" = "blue", 
                            "temperature" = "purple", 
                            "mass" = "orange", 
                            "weather" = "brown", 
                            "evidence" = "cyan", 
                            "anomaly" = "pink",
                            "opposed" = "yellow",
                            "neutral" = "darkgray",
                            "support" = "green")
                ) +
                theme_minimal() +
                theme(
                  legend.title = element_blank(),
                  # Set white background for panel and plot
                  panel.background = element_rect(fill = "white", color = NA),
                  plot.background = element_rect(fill = "white", color = NA),
                  panel.grid.major = element_line(color = "grey90"), # Optional: make grid lines light grey
                  panel.grid.minor = element_blank() # Optional: hide minor grid lines
                )

  ggsave(paste("../results/shiftingBaselines_1", fileSaveSuffix,"-timeSeries.png", sep=""), plot=fig)

}
