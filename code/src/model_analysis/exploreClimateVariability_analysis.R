# Capture the start time
start_time <- Sys.time()
options(error = quote({ traceback(); dump.frames(); }))

print('Running, running, running, running away')
setwd('~/Documents/Research/social-climate-model/code')

library(ggplot2)
library(forcats)
library(metR)
library(geomtextpath)

# Create a sequence of 81 numbers (you can adjust the length as needed)
natvar1 <- rep(0, 81)
natvar1[seq(10, 81, by = 10)] <- 5

coeff = 0.1

# Define the temperature matrix with 81 rows and 2 columns
mat <- matrix(0, nrow = 81, ncol = 2)

# First column values: start at 1, rise to 3 by index 41, drop to 1.5 by index 46, and stay at 1.5
mat[1:41, 1] <- seq(1, 3, length.out = 41)   # Values from 1 to 3 by index 41
mat[42:46, 1] <- seq(3, 1.5, length.out = 5) # Drop from 3 to 1.5 from index 41 to 46
mat[47:81, 1] <- 1.5                        # Stay at 1.5 from index 46 to 81

# Second column always 0
mat[, 2] <- 0

# for (natvar_it in seq(0, 20, 2)) {
for (natvar_it in c(5)){

  # fileSaveSuffix = ""
  # fileSaveSuffix = paste0("-natvar_", natvar_it) 
  fileSaveSuffix = natvar_it

  source("src/model.R")  # Load the model script
  # emissions = read.csv("../data/emissions_ssp2_rcp45-moreRegions.csv")
  # bau1=emissions[,3]/1000*12/(12+16+16) # conversion factor from MtCO2 per year to GtC per year
  # bau_outside1=emissions[,6:9]/1000*12/(12+16+16)

  # homophily_param01 = 0.7
  frac_opp_01 = 1
  frac_neut_01 = 0
  evidenceeffect1 = 0.18
  shiftingbaselines1 = 1
  # m = model() # evidenceeffect=0.02, natvar_multiplier=8, temperature_input=mat)

# Number of runs
num_runs <- 1

# Initialize a list to store the results of each run
results_list <- vector("list", num_runs)

# Run the model user-defined times and store each result
for (i in 1:num_runs) {
  results_list[[i]] <- model(shiftingExtremes=TRUE, natvar=TRUE, historical=TRUE)
}

# Initialize a list to accumulate the sums
avg_results <- results_list[[1]]

# Set all numeric elements to zero
for (name in names(avg_results)) {
  if (is.numeric(avg_results[[name]])) {
    avg_results[[name]] <- 0
  } else if (is.matrix(avg_results[[name]])) {
    avg_results[[name]] <- matrix(0, nrow = nrow(avg_results[[name]]), ncol = ncol(avg_results[[name]]))
  }
}

# Sum up the results from each run
for (i in 1:num_runs) {
  for (name in names(avg_results)) {
    if (is.numeric(avg_results[[name]]) || is.matrix(avg_results[[name]])) {
      avg_results[[name]] <- avg_results[[name]] + results_list[[i]][[name]]
    }
  }
}

# Calculate the average
for (name in names(avg_results)) {
  if (is.numeric(avg_results[[name]]) || is.matrix(avg_results[[name]])) {
    avg_results[[name]] <- avg_results[[name]] / num_runs
  }
}

# Use avg_results as your model output
m <- avg_results

params <- data.frame(
  Parameter = c(
    "evidenceeffect1",
    "shiftingbaselines1",
    "biassedassimilation1",
    "frac_opp_01",
    "frac_neut_01",
    "lag_param01",
    "lbd_param01",
    "forcestrong1",
    "forceweak1",
    "homophily_param01",
    "num_runs"
  ),
  Value = c(
    evidenceeffect1,
    shiftingbaselines1,
    biassedassimilation1,
    frac_opp_01,
    frac_neut_01,
    lag_param01,
    lbd_param01,
    forcestrong1,
    forceweak1,
    homophily_param1,
    num_runs
  )
)

print(params, row.names = FALSE)
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
  # fig = ggplot(data, aes(x = m$year)) +
  #               geom_line(aes(y = m$emissions, color = "OECD emissions")) +
  #               geom_line(aes(y = m$totalemissions, color = "total emissions")) +
  #               geom_line(aes(y = m$temp[,1], color = "temperature")) +
  #               geom_line(aes(y = m$evidence[,1], color = "evidence")) +
  #               geom_line(aes(y = m$anomaly, color = "anomaly"), linetype = "dashed") +
  #               geom_line(aes(y = m$emissions_outside[,1], color = "Asia Emissions")) +
  #               geom_line(aes(y = m$emissions_outside[,2], color = "LAM Emissions")) +
  #               geom_line(aes(y = m$emissions_outside[,3], color = "MAF Emissions")) +
  #               geom_line(aes(y = m$emissions_outside[,4], color = "Ref Emissions")) +

  #               # Secondary y-axis variables, rescaled to fit within the range of 0 to 1.5 on the secondary axis
  #               geom_line(aes(y = m$distributions[,1]/coeff, color = "opposed")) +
  #               geom_line(aes(y = m$distributions[,2]/coeff, color = "neutral")) +
  #               geom_line(aes(y = m$distributions[,2]/coeff, color = "support")) +
                
  #               # Combine both primary and secondary axes into a single scale_y_continuous call
  #               scale_y_continuous(
  #                 # Features of the first axis (primary y-axis)
  #                 name = "Emissions, Total Emissions, Temperature, Evidence, and Anomaly",
  #                 limits = c(-1, 21),  # Set the limits for the primary y-axis
                  
  #                 # Add a second axis and specify its features (secondary y-axis)
  #                 sec.axis = sec_axis(~. * coeff, name = "Population Distributions")
  #               ) +
  #               labs(
  #                 x = "Year",
  #                 y = "Model output",
  #                 title = "Model Outputs over Time (2020 to 2100)"
  #               ) +
  #               scale_color_manual(
  #                 values = c("OECD emissions" = "red", 
  #                           "total emissions" = "blue",
  #                           "Asia Emissions" = "#f200ff",
  #                           "LAM Emissions" = "#00ff66",
  #                           "MAF Emissions" = "#ff8000",
  #                           "Ref Emissions" = "blue", 
  #                           "temperature" = "#a4f020", 
  #                           "mass" = "orange", 
  #                           "weather" = "brown", 
  #                           "evidence" = "cyan", 
  #                           "anomaly" = "pink",
  #                           "opposed" = "yellow",
  #                           "neutral" = "darkgray",
  #                           "support" = "green")
  #               ) +
fig = ggplot(data, aes(x = m$year)) +
    # Emissions lines
    # geom_line(aes(y = m$emissions, color = "OECD emissions"), linetype = "solid", linewidth = 0.7) +
    geom_textline(aes(y = m$emissions, color = "OECD emissions"), label="OECD emissions", linetype = "solid", linewidth = 0.7, vjust = -0.15, size=3, hjust = 0.2)+
    geom_textline(aes(y = m$totalemissions, color = "Total emissions"), label="Total emissions", linetype = "solid", linewidth = 0.7, vjust=-0.15, size=3, hjust=0.1) +
    # geom_textline(aes(y = m$emissions_outside[,1], color = "Asia Emissions"), label="Asia emissions", linetype = "solid", linewidth = 0.7, vjust=-0.15, size=3) +
    # geom_textline(aes(y = m$emissions_outside[,2], color = "LAM Emissions"), label="LAM emissions", linetype = "solid", linewidth = 0.7, vjust=-0.15, size=3) +
    # geom_textline(aes(y = m$emissions_outside[,3], color = "MAF Emissions"), label="MAF emissions", linetype = "solid", linewidth = 0.7, vjust=-0.3, size=3) +
    # geom_textline(aes(y = m$emissions_outside[,4], color = "Ref Emissions"), label="REF emissions", linetype = "solid", linewidth = 0.7, vjust=1.5, size=3) +

    # Temperature-related lines
    geom_line(aes(y = m$weather, color = "Temperature"), linewidth = 0.9) +
    # geom_line(aes(y = m$evidence[,1], color = "Evidence"), linewidth = 0.9) +
    geom_line(aes(y = m$anomaly, color = "Anomaly"), linewidth = 0.9) +

    # Population distribution lines
    geom_textline(aes(y = m$distributions[,1]/coeff, color = "Opposed"), label="Opposed", linetype = "longdash", linewidth = 0.9, vjust=-0.15, size=3, hjust=0.2) +
    geom_textline(aes(y = m$distributions[,2]/coeff, color = "Neutral"), label="Neutral", linetype = "longdash", linewidth = 0.9, vjust=-0.15, size=3, hjust=0.2) +
    geom_textline(aes(y = m$distributions[,3]/coeff, color = "Support"), label="Support", linetype = "longdash", linewidth = 0.9, vjust=1.5, size=3, hjust=0.2) +

    # Adjust y-axes
    scale_y_continuous(
      name = "Emissions, Temperature, Evidence, and Anomaly",
      limits = c(-1, 21), 
      sec.axis = sec_axis(~. * coeff, name = "Population Distributions")
    ) +
    
    # Labeling and title
    labs(
      x = "Year",
      y = "Model output",
      title = "Model Outputs over Time (2020 to 2100)"
    ) +

    # Updated color scheme using distinct, dark-pastel colors
    scale_color_manual(
      values = c(
        "OECD emissions" = "#6a040f",       # Dark-pastel red
        "Total emissions" = "#5f0f40",      # Dark-pastel purple
        "Asia Emissions" = "#d00000",       # Dark-pastel green
        "LAM Emissions" = "#e85d04",        # Dark-pastel blue
        "MAF Emissions" = "#f48c06",        # Dark-pastel brown
        "Ref Emissions" = "#ffba08",        # Dark-pastel red
        "Temperature" = "#85C1E9",          # Light-pastel blue
        "Evidence" = "#caf0f8",             # Light-pastel green
        "Anomaly" = "#f4acb7",              # Light-pastel pink
        # "Weather" = "#000000",              # Light-pastel pink
        "Opposed" = "#ee4a70",              # Dark-pastel red-pink
        "Neutral" = "#8d99ae",              # Dark-pastel gray
        "Support" = "#06d667"               # Dark-pastel gray
      )
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

  # ggsave(paste("../results/default", fileSaveSuffix,"-timeSeries.png", sep=""), plot=fig)
  ggsave(paste("../results/default-timeSeries.png", sep=""), plot=fig, width=16/2, height=9/2)

}
