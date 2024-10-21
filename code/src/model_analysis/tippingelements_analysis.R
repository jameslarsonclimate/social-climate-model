# Capture the start time
start_time <- Sys.time()

print('Running, running, running, running away')
setwd('~/Documents/Research/social-climate-model/code')
fileSaveSuffix=""

library(ggplot2)
library(forcats)
library(metR)

source("src/model.R")  # Load the model script
m = model()

coeff = 0.1

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
                limits = c(0, 15),  # Set the limits for the primary y-axis
                
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

ggsave(paste("../results/figure2a-timeSeries", fileSaveSuffix,".png", sep=""), plot=fig)


# ggsave("../results/defaultModel-timeSeries.png", plot=fig)
stop('stopping -JGL')

#graphs to demonstrate interaction of model tipping points

# Loop over a set param
# for (evidenceeffect1_it in c(0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 2)) {
for (iterable in c(0)) {

  # Print the current homophily_param01 value for tracking
  print(paste("Running for evidenceeffect1 = ", evidenceeffect1_it))

  # Save the plot with the iterable value as a suffix
  fileSaveSuffix = paste0("-evidenceeffect1_", evidenceeffect1_it)  
  # fileSaveSuffix = 'original'

  #1. Individual Behavior
  print('Individual Behavior')

  # Set default model parameters to explore the effect of individual behavior
  source("src/model.R")  # Load the model script
  homophily_param1 = 0.7  # Set the degree of similarity in social networks, originally 0.7
  frac_opp_01 = 0.2  # Initial fraction of people opposed to climate policy
  frac_neut_01 = 0.6  # Initial fraction of people neutral to climate policy
  forcestrong1 = 0.2  # Strength of the external force (e.g., media or policy)
  pol_feedback1 = 5  # Policy feedback strength parameter
  pbc_mid1 = -0.5  # Parameter controlling the midpoint of behavior change due to policy
  pbc_steep1 = 1.5  # Steepness of the policy-based behavior change curve
  pbc_opinionchange1 = c(0, 0, -0.5)  # effect of climate chnage opinion on probability of adoption for opposition, neutral, support
  evidenceeffect1 = 0  # Effect of evidence on public opinion
  policyopinionfeedback_01 = 0  # Feedback between policy and public opinion
  m_max1 = 0.035  # Maximum effect of policy on mitigation
  lbd_param = 0.2  # Learning-by-doing effect (cost reduction with cumulative mitigation)
  
  # Change the value for the loop
  # evidenceeffect1 = evidenceeffect1_it  # This will now vary with each iteration

  # Loop over ranges of willingness to change behavior and credibility-enhancing display (CED)
  adoption_param = seq(0, 0.7, by = 0.01)  # Sequence of adoption parameters
  cred_param = seq(0, 0.5, by = 0.01)  # Sequence of CED effect parameters

  # Create a grid of all combinations of adoption_param and cred_param
  params = expand.grid(adoption_param, cred_param)  
  ems_output = numeric()  # Initialize an empty numeric vector for emissions output
  weather_output = numeric() # Initialize an empty numeric vector for weather output


  # Loop through each combination of parameters
  for(i in 1:dim(params)[1]){
    m = model(pbc_opinionchange=c(0, 0, -1*params[i,1]), ced_param=params[i, 2])
    ems_output[i] = m$totalemissions[81]  # extract cumulative emissions for 2020-2100
    # weather_output[i] = m$weather[81]  # extract cumulative emissions for 2020-2100    
  }

  # Store the results in the params data frame
  params$emissions = ems_output
  colnames(params) = c("individual_adoption", "ced_effect", "emissions_2100")  # Set column names

  # Plot the results
  a = ggplot(params,aes(x=individual_adoption, y=ced_effect, fill=emissions_2100, z=emissions_2100))+geom_tile()
  a = a + labs(x="Propensity for Individual Action by Climate Policy Supporters", y="Credibility Enhancing Display Effect", fill="2100\nEmissions\n(GtC)")
  a = a + theme_bw()+theme(strip.background=element_rect(fill="white"), legend.title.align = 0.5,text=element_text(size=14))
  a = a + scale_fill_gradient(low="blue",high="tomato3")
  a = a + geom_contour(breaks=c(2,4,6,8,10,12,14,16,18,20,22), col="black", lwd=0.75) + geom_text_contour(size=6.5, label.placement=label_placement_fraction(), skip=2, rotate=FALSE)
  ggsave(paste("../results/figure2a", fileSaveSuffix,".png", sep=""), plot=a)


  # Value used to transform the data
  coeff <- 10
  
  # Create a data frame with all the variables manually, no reshaping
  data <- data.frame(
    time = seq(2020, 2100, length.out = 81),     # Time from 2020 to 2100
    emissions = m$emissions,                     # Emissions data
    totalemissions = m$totalemissions,           # Total emissions data
    # mitigation = m$mitigation,                 # Mitigation data
    temperature = m$temp[,1],                    # Surface temperature
    # mass = m$mass,                             # Climate-related mass
    # weather = m$weather,                       # Weather data
    evidence = m$evidence[,1],                   # Evidence data
    anomaly = m$anomaly,                         # Climate anomaly
    opposed = m$distributions[,1]/coeff,
    neutral = m$distributions[,2]/coeff,
    support = m$distributions[,3]/coeff
  )

  # Plot each variable using ggplot2
  fig = ggplot(data, aes(x = time)) +
                geom_line(aes(y = emissions, color = "emissions")) +
                geom_line(aes(y = totalemissions, color = "totalemissions")) +
                # geom_line(aes(y = mitigation, color = "mitigation")) +
                geom_line(aes(y = temperature, color = "temperature")) +
                geom_line(aes(y = evidence, color = "evidence")) +
                geom_line(aes(y = anomaly, color = "anomaly")) +
                geom_line(aes(y = opposed, color = "opposed")) +
                geom_line(aes(y = neutral, color = "neutral")) +
                geom_line(aes(y = support, color = "support")) +
                scale_y_continuous(
                  # Features of the first axis
                  name = "Emissions, Total Emissions, Temperature, Evidence, and Anomaly",
                  
                  # Add a second axis and specify its features
                  sec.axis = sec_axis(~.*coeff, name="Population Distributions")
                )

                labs(
                  x = "Year",
                  y = "Model output",
                  title = "Model Outputs over Time (2020 to 2100)"
                ) +
                scale_color_manual(
                  values = c("emissions" = "red", 
                            "totalemissions" = "blue", 
                            # "mitigation" = "green", 
                            "temperature" = "purple", 
                            "mass" = "orange", 
                            "weather" = "brown", 
                            "evidence" = "cyan", 
                            "anomaly" = "pink",
                            "opposed" = "yellow",
                            "neutral" = "darkgray",
                            "support" = "green")
                ) +
                theme_ipsum() +
                theme(
                  legend.title = element_blank(),
                  # Set white background for panel and plot
                  panel.background = element_rect(fill = "white", color = NA),
                  plot.background = element_rect(fill = "white", color = NA),
                  panel.grid.major = element_line(color = "grey90"), # Optional: make grid lines light grey
                  panel.grid.minor = element_blank() # Optional: hide minor grid lines
                )


  ggsave(paste("../results/figure2a-timeSeries", fileSaveSuffix,".png", sep=""), plot=fig)


  ggsave(paste("../results/figure2a-timeSeriesNormalized", fileSaveSuffix,".png", sep=""), plot=fig)

  # stop()
  
  #2. Technical Change
  print('Technical Change')

  # Load the model script again for another experiment
  source("src/model.R")  
  homophily_param1 = 0.75  # Higher similarity in social networks, originally 0.75
  frac_opp_01 = 0.2  # Fraction of climate policy opposers
  frac_neut_01 = 0.50  # Fraction of neutral individuals
  forcestrong1 = 0.3  # Strength of external force
  evidenceeffect1 = 0  # Effect of evidence on behavior
  policyopinionfeedback_01 = 0  # No feedback between policy and opinion
  m_max1 = 0.025  # Maximum effect of policy on mitigation
  pol_feedback1 = 0  # No policy feedback

  # Change the value for the loop
  # evidenceeffect1 = evidenceeffect1_it  # This will now vary with each iteration

  # Sweep over ranges of endogenous cost reduction and policy support
  lbd_sweep = seq(0, 0.25, by = 0.005)  # Endogenous cost reduction
  fracsupp_sweep = seq(0.15, 0.35, by = 0.005)  # Fraction of policy supporters
  sq_sweep = c(1.25, 9)  # Status quo bias levels

  # Create a grid of all parameter combinations
  params = expand.grid(lbd_sweep, fracsupp_sweep, sq_sweep)
  ems_output=numeric()

  # Loop through each combination of parameters
  for(i in 1:dim(params)[1]){
    m = model(lbd_param=params[i, 1], frac_opp_0=0.5-params[i, 2], pol_response=params[i, 3])
    ems_output[i] = sum(m$totalemissions[1:81])  # Calculate total emissions over 2020-2100
  }

  # Store the results in the params data frame
  params$emissions = ems_output
  colnames(params) = c("endogenous_cost", "frac_support", "status_quo_bias", "cumulative_emissions")  # Set column names
  params$status_quo_bias = as.factor(params$status_quo_bias)  # Convert status quo bias to a factor
  params$status_quo_bias = fct_recode(params$status_quo_bias, "Low Status-Quo Bias" = "1.25", "High Status Quo Bias" = "9")

  # Plot the results
  a = ggplot(params,aes(x=endogenous_cost*100,y=frac_support/0.5,fill=cumulative_emissions,z=cumulative_emissions))+geom_tile()
  a = a + facet_wrap(~status_quo_bias)+labs(x="Endogenous Cost Reductions (% per Doubling)",y="Initial Fraction Climate Policy Supporters",fill="Total Emissions\n2020-2100\n(GtC)")
  a = a + theme_bw()+theme(strip.background =element_rect(fill="white"), legend.title.align = 0.5, legend.box.just = "center", strip.text=element_text(face="bold", size=12), text=element_text(size=14))
  a = a + scale_fill_gradient(low="yellow",high="turquoise3")
  a = a + geom_contour(breaks=seq(600,1400,by=50), col="black", lwd=0.75)+geom_text_contour(size=6.5, label.placement = label_placement_fraction(), skip=2, rotate=FALSE)
  ggsave(paste("../results/figure2b",fileSaveSuffix,".png", sep=""), plot=a)

  # Create a data frame with all the variables manually, no reshaping
  data <- data.frame(
    time = seq(2020, 2100, length.out = 81),     # Time from 2020 to 2100
    emissions = m$emissions,                     # Emissions data
    totalemissions = m$totalemissions,           # Total emissions data
    # mitigation = m$mitigation,                   # Mitigation data
    temperature = m$temp[,1],                         # Surface temperature
    # mass = m$mass,                               # Climate-related mass
    # weather = m$weather,                         # Weather data
    evidence = m$evidence[,1],                       # Evidence data
    anomaly = m$anomaly,                          # Climate anomaly
    opposed = m$distributions[,1],
    neutral = m$distributions[,2],
    support = m$distributions[,3]
  )

# Plot each variable using ggplot2
  fig = ggplot(data, aes(x = time)) +
                geom_line(aes(y = emissions, color = "emissions")) +
                geom_line(aes(y = totalemissions, color = "totalemissions")) +
                # geom_line(aes(y = mitigation, color = "mitigation")) +
                geom_line(aes(y = temperature, color = "temperature")) +
                geom_line(aes(y = evidence, color = "evidence")) +
                geom_line(aes(y = anomaly, color = "anomaly")) +
                geom_line(aes(y = opposed, color = "opposed")) +
                geom_line(aes(y = neutral, color = "neutral")) +
                geom_line(aes(y = support, color = "support")) +
                labs(
                  x = "Year",
                  y = "Model output",
                  title = "Model Outputs over Time (2020 to 2100)"
                ) +
                scale_color_manual(
                  values = c("emissions" = "red", 
                            "totalemissions" = "blue", 
                            # "mitigation" = "green", 
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


  ggsave(paste("../results/figure2b-timeSeries", fileSaveSuffix,".png", sep=""), plot=fig)


# Plot each variable using ggplot2
fig = ggplot(data, aes(x = time)) +
              geom_line(aes(y = normalize(emissions), color = "emissions")) +
              geom_line(aes(y = normalize(totalemissions), color = "totalemissions")) +
              # geom_line(aes(y = mitigation, color = "mitigation")) +
              geom_line(aes(y = normalize(temperature), color = "temperature")) +
              geom_line(aes(y = normalize(evidence), color = "evidence")) +
              geom_line(aes(y = normalize(anomaly), color = "anomaly")) +
              geom_line(aes(y = normalize(opposed), color = "opposed")) +
              geom_line(aes(y = normalize(neutral), color = "neutral")) +
              geom_line(aes(y = normalize(support), color = "support")) +
              labs(
                x = "Year",
                y = "Normalized model output (0 to 1)",
                title = "Model Outputs over Time (2020 to 2100)"
              ) +
              scale_color_manual(
                values = c("emissions" = "red", 
                          "totalemissions" = "blue", 
                          # "mitigation" = "green", 
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

  ggsave(paste("../results/figure2b-timeSeriesNormalized", fileSaveSuffix,".png", sep=""), plot=fig)


  # 3. Perception of Climate Change
  print('Perception of Climate Change')

  # Load the model script for analyzing perception
  source("src/model.R")
  frac_opp_01 = 0.26  # Doubtful and dismissive individuals, originally 0.26
  frac_neut_01 = 0.33  # Cautious and disengaged individuals
  policyopinionfeedback_01 = 0  # No feedback between policy and opinion
  homophily_param = 0.95   # Very high similarity in social networks, originally 0.95
  # homophily_param1 = 0.95   # Very high similarity in social networks, originally 0.95

  # Change the value for the loop
  # evidenceeffect1 = evidenceeffect1_it  # This will now vary with each iteration

  # Sweep over biased assimilation, evidence effect, and shifting baselines
  biassedass_sweep = seq(0, 0.9, by = 0.05)  # Range of biased assimilation parameters
  perception_sweep = seq(0, 0.25, by = 0.025)  # Range of perception parameters
  baselines = c(0, 1)  # Fixed vs. shifting baseline settings
  
  # Create a grid of all parameter combinations
  params = expand.grid(biassedass_sweep, perception_sweep, baselines)
  
  dist_output_pro = numeric()  # Store output for climate policy proponents
  dist_output_con = numeric()  # Store output for climate policy opponents
  year = 2050  # Target year for analysis
  reps = 30  # Number of model repetitions to average over randomization 
  
  # Loop through each combination of parameters
  for (i in 1:dim(params)[1]) {
    # print(i)  # Track progress by printing the current iteration
    
    # Initialize temporary numeric vectors to store results from repeated model runs
    temp1 = numeric(length = reps)
    temp2 = numeric(length = reps)
    
    # Check if the third parameter in the current row is equal to 0
    if (params[i, 3] == 0){
      # Run the model with the current set of parameters (no shifting baselines)
      m = model(biassedassimilation = params[i, 1], evidenceeffect = params[i, 2], shiftingbaselines = params[i, 3])
      
      # Extract the "opposed" distribution for the given year and store in 'dist_output_con'
      dist_output_con[i] = m$distributions[which(m$year == year), 1]
      
      # Extract the "pro-climate" distribution for the given year and store in 'dist_output_pro'
      dist_output_pro[i] = m$distributions[which(m$year == year), 3]
      
      # Skip to the next iteration since no further averaging is needed for fixed baselines
      next  
    }
    
    # If the third parameter is greater than 0 (shifting baselines case)
    if (params[i, 3] > 0){
      # Run the model multiple times to average results, since shifting baselines involve randomness
      for (j in 1:reps){
        # Run the model with the current parameters
        m = model(biassedassimilation = params[i, 1], evidenceeffect = params[i, 2], shiftingbaselines = params[i, 3])
        
        # Store the "pro-climate" distribution for the given year from this run in 'temp1'
        temp1[j] = m$distributions[which(m$year == year), 3]
        
        # Store the "opposed" distribution for the given year from this run in 'temp2'
        temp2[j] = m$distributions[which(m$year == year), 1]
      }
      # After running the model 'reps' times, calculate the mean for the "pro-climate" and "opposed" distributions
      dist_output_pro[i] = mean(temp1)
      dist_output_con[i] = mean(temp2)
    }
  }

  # Store the results in the params data frame
  params$opposed = dist_output_con
  colnames(params) = c("biassed_assimilation", "evidenceeffect", "shiftingbaseline", "opposers2050")
  params$shiftingbaseline = as.factor(params$shiftingbaseline)
  params$shiftingbaseline = fct_recode(params$shiftingbaseline, "Shifting Baseline" = "1", "Fixed Baseline" = "0")
  
  # Define the fixed range for the fill colors
  fixed_range <- c(0, 12)  # Example range from 0% to 100%
  
  # Plot the results with fixed range
  a = ggplot(params, aes(x = biassed_assimilation, y = evidenceeffect, fill = opposers2050 * 100, z = opposers2050 * 100)) + geom_tile()
  a = a + facet_wrap(~shiftingbaseline) + labs(x = "Biased Assimilation", y = "Effect of Perceived Weather on Opinion", fill = "Policy Opposers\n2050 (%)")
  a = a + theme_bw() + theme(strip.background = element_rect(fill = "white"), legend.title.align = 0.5, legend.box.just = "center", strip.text = element_text(face = "bold", size = 12), text = element_text(size = 14))
  a = a + scale_fill_gradient(low = "darkorchid", high = "palegreen2", limits = fixed_range)
  
  ggsave(paste("../results/figure2c", fileSaveSuffix, ".png", sep = ""), plot = a)

    # Create a data frame with all the variables manually, no reshaping
  data <- data.frame(
    time = seq(2020, 2100, length.out = 81),     # Time from 2020 to 2100
    emissions = m$emissions,                     # Emissions data
    totalemissions = m$totalemissions,           # Total emissions data
    # mitigation = m$mitigation,                   # Mitigation data
    temperature = m$temp[,1],                         # Surface temperature
    # mass = m$mass,                               # Climate-related mass
    # weather = m$weather,                         # Weather data
    evidence = m$evidence[,1],                       # Evidence data
    anomaly = m$anomaly,                          # Climate anomaly
    opposed = m$distributions[,1],
    neutral = m$distributions[,2],
    support = m$distributions[,3]
  )

  # Plot each variable using ggplot2
  fig = ggplot(data, aes(x = time)) +
                geom_line(aes(y = emissions, color = "emissions")) +
                geom_line(aes(y = totalemissions, color = "totalemissions")) +
                # geom_line(aes(y = mitigation, color = "mitigation")) +
                geom_line(aes(y = temperature, color = "temperature")) +
                geom_line(aes(y = evidence, color = "evidence")) +
                geom_line(aes(y = anomaly, color = "anomaly")) +
                geom_line(aes(y = opposed, color = "opposed")) +
                geom_line(aes(y = neutral, color = "neutral")) +
                geom_line(aes(y = support, color = "support")) +
                labs(
                  x = "Year",
                  y = "Model output",
                  title = "Model Outputs over Time (2020 to 2100)"
                ) +
                scale_color_manual(
                  values = c("emissions" = "red", 
                            "totalemissions" = "blue", 
                            # "mitigation" = "green", 
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


  ggsave(paste("../results/figure2c-timeSeries", fileSaveSuffix,".png", sep=""), plot=fig)

  # Plot each variable using ggplot2
  fig = ggplot(data, aes(x = time)) +
                geom_line(aes(y = normalize(emissions), color = "emissions")) +
                geom_line(aes(y = normalize(totalemissions), color = "totalemissions")) +
                # geom_line(aes(y = mitigation, color = "mitigation")) +
                geom_line(aes(y = normalize(temperature), color = "temperature")) +
                geom_line(aes(y = normalize(evidence), color = "evidence")) +
                geom_line(aes(y = normalize(anomaly), color = "anomaly")) +
                geom_line(aes(y = normalize(opposed), color = "opposed")) +
                geom_line(aes(y = normalize(neutral), color = "neutral")) +
                geom_line(aes(y = normalize(support), color = "support")) +
                labs(
                  x = "Year",
                  y = "Normalized model output (0 to 1)",
                  title = "Model Outputs over Time (2020 to 2100)"
                ) +
                scale_color_manual(
                  values = c("emissions" = "red", 
                            "totalemissions" = "blue", 
                            # "mitigation" = "green", 
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

  ggsave(paste("../results/figure2c-timeSeriesNormalized", fileSaveSuffix,".png", sep=""), plot=fig)


}

print(paste("Run completed at", Sys.time()))
print(paste("Duration:", Sys.time() - start_time))
