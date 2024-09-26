# Load required libraries
library(ggplot2)  # for plotting
library(forcats)  # for factor reordering and manipulation
library(metR)  # for creating contour plots

# 1. Individual Behavior

# Set some model defaults to demonstrate the effect of individual behavior on emissions
source("src/model.R")  # Load the main model script
homophily_param1 = 0.7  # Degree of similarity between individuals in the population
frac_opp_01 = 0.2  # Fraction of population opposing climate policy
frac_neut_01 = 0.6  # Fraction of population neutral to climate policy
forcestrong1 = 0.2  # External influence on population (e.g., media, social norms)
pol_feedback1 = 5  # Strength of policy feedback (e.g., government intervention)
pbc_mid1 = -0.5  # Middle point of policy belief change curve
pbc_steep1 = 1.5  # Steepness of policy belief change curve
pbc_opinionchange1 = c(0, 0, -0.5)  # Impact of policies on opinion change
evidenceeffect1 = 0  # Effect of evidence on opinion change (not modeled here)
policyopinionfeedback_01 = 0  # Feedback from policy opinion on the model
m_max1 = 0.035  # Maximum adoption rate for individual behavior change
lbd_param = 0.2  # Learning-by-doing parameter (how quickly tech costs drop)

# Loop over ranges of willingness to change behavior and credibility-enhancing displays (CED)
adoption_param = seq(0, 0.7, by = 0.01)  # Range of individual behavior adoption
cred_param = seq(0, 0.5, by = 0.01)  # Range of CED impact (credibility of information)
params = expand.grid(adoption_param, cred_param)  # Create a grid of all possible combinations
ems_output = numeric()  # Initialize a vector to store emissions output

# Loop through each combination of individual adoption and CED impact
for(i in 1:dim(params)[1]){
  m = model(pbc_opinionchange = c(0, 0, -1 * params[i, 1]), ced_param = params[i, 2])  # Run the model with varying adoption and CED impact
  ems_output[i] = m$totalemissions[81]  # Extract cumulative emissions for the year 2100
}

# Store the results in a data frame and label columns
params$emissions = ems_output
colnames(params) = c("individual_adoption", "ced_effect", "emissions_2100")

# Create a heatmap to show the relationship between individual adoption, CED effect, and emissions
a = ggplot(params, aes(x = individual_adoption, y = ced_effect, fill = emissions_2100, z = emissions_2100)) + 
  geom_tile()
a = a + labs(x = "Propensity for Individual Action by Climate Policy Supporters", 
             y = "Credibility Enhancing Display Effect", fill = "2100\nEmissions\n(GtC)")
a = a + theme_bw() + theme(strip.background = element_rect(fill = "white"), legend.title.align = 0.5, text = element_text(size = 14))
a = a + scale_fill_gradient(low = "blue", high = "tomato3")
a = a + geom_contour(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), col = "black", lwd = 0.75) + 
  geom_text_contour(size = 6.5, label.placement = label_placement_fraction(), skip = 2, rotate = FALSE)
ggsave("../results/figure2a.pdf", plot = a)  # Save the plot as a PDF

# 2. Technical Change
source("src/model.R")  # Reload the model script with new parameters
homophily_param1 = 0.75  # Adjusted similarity between individuals
frac_opp_01 = 0.2  # Fraction of policy opposition remains unchanged
frac_neut_01 = 0.50  # Fraction of population neutral to climate policy reduced
forcestrong1 = 0.3  # Increased external influence on population
evidenceeffect1 = 0  # No direct effect of evidence
policyopinionfeedback_01 = 0  # No policy feedback
m_max1 = 0.025  # Maximum adoption rate for technical change
pol_feedback1 = 0  # No policy feedback on opinions

# Define sweeps over technical change parameters
lbd_sweep = seq(0, 0.25, by = 0.005)  # Range for learning-by-doing parameter
fracsupp_sweep = seq(0.15, 0.35, by = 0.005)  # Range for fraction supporting climate policy
sq_sweep = c(1.25, 9)  # Status-quo bias (low vs high)

params = expand.grid(lbd_sweep, fracsupp_sweep, sq_sweep)  # Create a grid of parameter combinations
ems_output = numeric()  # Initialize emissions output

# Loop through parameter grid and simulate model
for(i in 1:dim(params)[1]){
  m = model(lbd_param = params[i, 1], frac_opp_0 = 0.5 - params[i, 2], pol_response = params[i, 3])  # Run model with varying learning and support
  ems_output[i] = sum(m$totalemissions[1:81])  # Sum total emissions from 2020-2100
}

# Store results and label columns
params$emissions = ems_output
colnames(params) = c("endogenous_cost", "frac_support", "status_quo_bias", "cumulative_emissions")
params$status_quo_bias = as.factor(params$status_quo_bias)  # Convert status-quo bias to factor
params$status_quo_bias = fct_recode(params$status_quo_bias, "Low Status-Quo Bias" = "1.25", "High Status Quo Bias" = "9")

# Create a heatmap to visualize the effects of learning-by-doing and status-quo bias on emissions
a = ggplot(params, aes(x = endogenous_cost * 100, y = frac_support / 0.5, fill = cumulative_emissions, z = cumulative_emissions)) + 
  geom_tile()
a = a + facet_wrap(~status_quo_bias) + 
  labs(x = "Endogenous Cost Reductions (% per Doubling)", y = "Initial Fraction Climate Policy Supporters", fill = "Total Emissions\n2020-2100\n(GtC)")
a = a + theme_bw() + theme(strip.background = element_rect(fill = "white"), legend.title.align = 0.5, legend.box.just = "center", 
                           strip.text = element_text(face = "bold", size = 12), text = element_text(size = 14))
a = a + scale_fill_gradient(low = "yellow", high = "turquoise3")
a = a + geom_contour(breaks = seq(600, 1400, by = 50), col = "black", lwd = 0.75) + 
  geom_text_contour(size = 6.5, label.placement = label_placement_fraction(), skip = 2, rotate = FALSE)
ggsave("../results/figure2b.pdf", plot = a)  # Save the plot as a PDF

# 3. Perception of Climate Change
source("src/model.R")  # Reload the model for another analysis

# Set initial parameters for public opinion based on segments of the population (e.g., Global Warming's Six Americas)
frac_opp_01 = 0.26  # Doubtful and dismissive groups
frac_neut_01 = 0.33  # Cautious and disengaged groups
policyopinionfeedback_01 = 0  # No feedback from policy opinions
homophily_param = 0.95  # Strong similarity within groups

# Define sweeps over biased assimilation and weather perception effects
biassedass_sweep = seq(0, 0.9, by = 0.05)  # Range of biased assimilation (people's tendency to interpret information based on pre-existing beliefs)
perception_sweep = seq(0, 0.25, by = 0.025)  # Range of perception effects (effect of weather on public opinion)
baselines = c(0, 1)  # Shifting baselines (whether people adjust their expectations over time)

params = expand.grid(biassedass_sweep, perception_sweep, baselines)  # Create parameter grid

# Initialize storage for opinion distributions
dist_output_pro = numeric()
dist_output_con = numeric()
year = 2050  # Target year for analysis
reps = 1500  # Number of model runs for averaging

# Loop through parameter grid and simulate model
for(i in 1:dim(params)[1]){
  print(i)
  temp1 = numeric(length = reps)  # Initialize temporary storage for pro-climate action group
  temp2 = numeric(length = reps)  # Initialize temporary storage for anti-climate action group
  if(params[i, 3] == 1){
    baselineshift = TRUE  # Apply baseline shift if parameter is set to 1
  } else {
    baselineshift = FALSE  # No baseline shift if parameter is 0
  }
  # Perform multiple repetitions for each parameter combination
  for(j in 1:reps){
    m = model(opp_0 = frac_opp_01, neut_0 = frac_neut_01, biassedassimilation = params[i, 1], weathereffect = params[i, 2], baseline = baselineshift)  # Run model
    temp1[j] = m$opiniondist[year, 3]  # Store pro-climate opinion distribution in year 2050
    temp2[j] = m$opiniondist[year, 1]  # Store anti-climate opinion distribution in year 2050
  }
  dist_output_pro[i] = mean(temp1)  # Average across repetitions for pro-climate group
  dist_output_con[i] = mean(temp2)  # Average across repetitions for anti-climate group
}

# Store the results and label columns
params$pro_climate2050 = dist_output_pro
params$anti_climate2050 = dist_output_con
colnames(params) = c("biased_assimilation", "weather_perception", "baseline_shift", "pro_climate", "anti_climate")

# Create a heatmap to visualize the public opinion shifts in 2050 under different scenarios
a = ggplot(params, aes(x = biased_assimilation, y = weather_perception, fill = pro_climate, z = pro_climate)) + 
  geom_tile()
a = a + facet_wrap(~baseline_shift) + 
  labs(x = "Biased Assimilation", y = "Weather Effect", fill = "Pro-Climate Fraction\nin 2050")
a = a + theme_bw() + theme(strip.background = element_rect(fill = "white"), legend.title.align = 0.5, legend.box.just = "center", 
                           strip.text = element_text(face = "bold", size = 12), text = element_text(size = 14))
a = a + scale_fill_gradient(low = "palegreen", high = "steelblue")
a = a + geom_contour(breaks = c(0.1, 0.25, 0.5, 0.75), col = "black", lwd = 0.75) + 
  geom_text_contour(size = 6.5, label.placement = label_placement_fraction(), skip = 2, rotate = FALSE)
ggsave("../results/figure2c.pdf", plot = a)  # Save the plot as a PDF
