# Load required libraries
library(ggplot2)  # for plotting
library(forcats)  # for factor reordering and manipulation
library(metR)     # for creating contour plots

# Set up model parameters to analyze the effect of natural variability and shifting baselines
source("src/model.R")  # Load the main model script
homophily_param1 = 0.7  # Degree of similarity between individuals in the population
frac_opp_01 = 0.2  # Fraction of population opposing climate policy
frac_neut_01 = 0.6  # Fraction of population neutral to climate policy
forcestrong1 = 0.2  # External influence on population (e.g., media, social norms)
pol_feedback1 = 5  # Strength of policy feedback (e.g., government intervention)
pbc_mid1 = -0.5  # Middle point of policy belief change curve
pbc_steep1 = 1.5  # Steepness of policy belief change curve
pbc_opinionchange1 = c(0, 0, -0.5)  # Impact of policies on opinion change
evidenceeffect1 = 0  # Effect of evidence on opinion change
policyopinionfeedback_01 = 0  # Feedback from policy opinion on the model
m_max1 = 0.035  # Maximum adoption rate for individual behavior change
lbd_param = 0.2  # Learning-by-doing parameter (how quickly tech costs drop)
baselineshift = TRUE

# Loop over ranges of natural variability and shifting baselines
natvar_multiplier = seq(4, 16, by = 0.5)  # Range of natural variability
weights_multiplier = seq(0.5, 2.0, by = 0.05)  # Range of weights multiplier for how much people assimilate prior years of weather
params = expand.grid(natvar_multiplier, weights_multiplier)  # Create a grid of all possible combinations
ems_output = numeric()  # Initialize a vector to store emissions output

# Loop through each combination of natural variability and shifting baseline impact
for(i in 1:dim(params)[1]){
  m = model(natvar_multiplier = params[i, 1], weights_multiplier = params[i, 2], shiftingbaselines = 1)  # Run the model with varying natural variability and shifting baselines
  ems_output[i] = m$totalemissions[81]  # Extract cumulative emissions for the year 2100
}

# Store the results in a data frame and label columns
params$emissions = ems_output
colnames(params) = c("natural_variability", "weights_multiplier", "emissions_2100")

# Create a heatmap to show the relationship between natural variability, assimilation weight, and emissions
heatmap_plot = ggplot(params, aes(x = natural_variability, y = weights_multiplier, fill = emissions_2100, z = emissions_2100)) + 
  geom_tile() +
  labs(x = "Natural Variability Effect", 
       y = "Assimilation Weight Effect", fill = "2100\nEmissions\n(GtC)") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"), legend.title.align = 0.5, text = element_text(size = 14)) +
  scale_fill_gradient(low = "blue", high = "red") +
  geom_contour(breaks = seq(2, 20, by = 2), col = "black", lwd = 0.75) + 
  geom_text_contour(size = 6.5, label.placement = label_placement_fraction(), skip = 2, rotate = FALSE)

# Save the plot
ggsave("../results/natural_variability_assimilation_weight_heatmap.pdf", plot = heatmap_plot)
