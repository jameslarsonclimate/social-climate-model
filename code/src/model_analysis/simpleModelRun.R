# Clean script to run the social climate model and create a plot

# Setup
setwd('~/Documents/Research/social-climate-model/code')
library(ggplot2)

# Load the model
source("src/model.R")

# Set model parameters
frac_opp_01 <- 0.5        # Fraction opposing climate policy at t=0
frac_neut_01 <- 0.3       # Fraction neutral at t=0
evidenceeffect1 <- 0.15    # Strength of evidence effect
biassedassimilation1 <- 0.1  # Strength of biased assimilation
shiftingbaselines1 <- 1   # Whether shifting baselines are active

# Create a timeseries with a triangular pulse from index 10 to 20
# Initialize a vector of 81 zeros and define the peak value
ts <- numeric(81)
peak <- 2

# Create ascending values from index 10 to 15 and descending values from index 16 to 20
ts[11:15] <- seq(0, peak, length.out = 6)
ts[16:21] <- seq(peak - peak/5, 0, length.out = 5)
  
# Example output of internally generated natural variability that results in:
# - Majority climate supporters in ~2055
# - Net zero emissions in ~2090
# ts <- c(
#   0.133842953, -0.065487578, 0.129109393, -0.069017929, 0.584610252,
#   0.319442202, 0.350141883, 0.856237555, 0.740352139, 0.318956116,
#   0.505064617, 0.760360126, 0.094969291, -0.073465609, -0.116369872,
#   -0.473726221, 0.157055352, -0.064971138, -0.132389645, 0.028965936,
#   0.257804372, 0.325864269, 0.596301545, 0.310239613, -0.005382159,
#   -0.125759004, -0.276082519, -0.480876377, -0.537635965, -0.435412122,
#   -0.297650389, -0.319626429, -0.561412574, -0.715974137, -0.663435558,
#   -0.390001544, -0.427180852, -0.408288461, 0.175796509, -0.226157358,
#   -0.395769509, -0.250313217, -0.365319772, -0.231098545, 0.018702495,
#   -0.242858404, 0.037601432, -0.040663705, 0.301615875, 0.126242778,
#   -0.394601911, -0.117267255, 0.143090743, -0.127437175, 0.095114001,
#   0.699648894, 0.942651981, 0.387641568, 0.861152703, 0.908385819,
#   0.588097841, 0.917275321, 0.721406489, 0.535314193, 0.159254638,
#   0.119618600, -0.114804506, 0.381691350, 0.274759474, -0.309654467,
#   -0.162296890, -0.411838048, -0.242283146, -0.167290071, 0.028700409,
#   -0.032134208, -0.398540297, -0.037209780, -0.202788485, -0.274764916,
#   -0.518708527
# )

# Example output of internally generated natural variability that results in:
# - Majority climate supporters in ~2035
# - Net zero emissions in ~2075
# This is a climate that promotes climate supporters
# ts <- c(
#   -0.67704994, -0.56296169, 0.20469505, 0.12178787, -0.30266837, -0.07048941,
#   0.28024932, 0.24712123, -0.17678515, 0.04101050, 0.24998563, -0.34139183,
#   0.18872790, 0.35319768, 0.47587127, 0.16041274, 0.04916426, 0.51155494,
#   0.65448188, 0.48125646, 0.76822399, 0.64792114, 0.74081283, 0.77439964,
#   0.69447310, 0.52472408, 0.51376338, 0.16855018, 0.49650381, 0.58099224,
#   0.48498498, 0.38242786, 0.52401343, 0.38257102, 0.69391815, -0.25488174,
#   0.48000086, 0.48987867, 0.55391545, 0.34511112, 0.69226122, 0.40424713,
#   0.35856940, 0.98332664, 0.34301646, 0.53542841, 0.38079007, 0.01473457,
#   0.28090790, -0.02899917, 0.06952627, 0.53095578, -0.21523872, -0.19720691,
#   0.11654373, -0.31103421, -0.02856493, 0.20379062, -0.18102896, -0.23051458,
#   -0.30129623, -0.47995420, 0.06583502, -0.10761788, -0.14177160, -0.07291975,
#   -0.37901846, -0.23060744, -0.51540471, -0.21383997, -0.30768690, -0.62547881,
#   -0.35078094, -0.43146621, -0.60706820, -0.33365032, -0.07809506, -0.11361601,
#   -0.48523043, -0.19303042, 0.04642298
# )

# # Example output of internally generated natural variability that results in:
# # - Climate supporters never reach majority
# # - Emissions stay high (SSP3-7.0) for entirety of run
# # This is a climate that promotes buisness as usual
# ts <- c(
#   0.37927546, 0.14797994, -0.07297981, -0.08728853, -0.07830655, -0.08790408,
#   -0.05810093, -0.20510265, 0.04376236, 0.23367906, 0.05704081, 0.50227240,
#   0.39932231, -0.24663818, 0.19560222, 0.16719105, -0.10275608, -0.44936831,
#   -0.06344066, 0.11405526, -0.43049336, -0.63392917, 0.11367537, -0.46577790,
#   -0.15761725, -0.21891219, 0.13652073, -0.19472288, -0.42080276, -0.23087182,
#   -0.14347206, -0.44581006, 0.46876776, 0.02828554, -0.40988185, -0.18645090,
#   -0.71127151, -0.68891421, -0.12533891, -0.46600504, -0.43007705, -0.67696496,
#   0.04989338, 0.08060462, 0.18540005, 0.36929543, 0.05028143, 0.23517306,
#   0.43408447, -0.14329964, 0.12233230, -0.03682000, 0.12615687, -0.08833765,
#   0.02127670, 0.17222456, 0.55058025, 0.28175860, -0.06390461, 0.43561294,
#   0.10091578, -0.45414046, 0.24005863, 0.15483816, -0.15760568, 0.23584144,
#   0.26816849, 0.02089773, -0.22644128, 0.08683933, 0.10918248, 0.37218345,
#   0.24975125, -0.23387813, -0.43169722, -0.27474710, -0.23422222, -0.58773169,
#   -0.17133926, -0.60449470, -0.61034812
# )



# Run the model
m <- model(natvar_multiplier = 0)

# Create a scaling coefficient for the secondary axis
coeff <- 0.05

# Create a data frame with model outputs
data <- data.frame(
  time = m$year,
  emissions = m$emissions,
  total_emissions = m$totalemissions,
  temperature = m$temp[,1],
  weather = m$weather,
  evidence = m$evidence[,1],
  anomaly = m$anomaly,
  opposed = m$distributions[,1],
  neutral = m$distributions[,2],
  support = m$distributions[,3]
)

# Create a plot of key variables
fig <- ggplot(data, aes(x = time)) +
  # Add vertical lines at the beginning (bottom z-order)
  geom_vline(xintercept = c(2029, 2039), color = "black", alpha = 0.7) +

  # Primary y-axis
  geom_line(aes(y = total_emissions, color = "Total Emissions"), linewidth = 0.9) +
  geom_line(aes(y = weather, color = "Weather"), linewidth = 0.9) +
  geom_line(aes(y = evidence, color = "Evidence"), linewidth = 0.9) +
  
  # Secondary y-axis (population distributions)
  geom_line(aes(y = opposed/coeff, color = "Opposed"), linewidth = 0.9) +
  geom_line(aes(y = neutral/coeff, color = "Neutral"), linewidth = 0.9) +
  geom_line(aes(y = support/coeff, color = "Support"), linewidth = 0.9) +
  
  # Set up axes
  scale_y_continuous(
    name = "Emissions and Temperature",
    limits = c(-2, 25), 
    sec.axis = sec_axis(~. * coeff, name = "Population Distributions")
  ) +
  
  # Labels and title
  labs(
    x = "Year",
    y = "Model Output",
    title = "Social Climate Model Output",
    subtitle = paste("Evidence Effect =", evidenceeffect1, 
                    ", Biased Assimilation =", biassedassimilation1,
                    ",\nInitial Opposition =", frac_opp_01,
                    ", Initial Neutral =", frac_neut_01)
  ) +
  
  # Color scheme
  scale_color_manual(
    values = c(
      "Total Emissions" = "#5f0f40",    # Dark purple
      "Weather" = "#ffba08",            # Gold
      "Evidence" = "#caf0f8",           # Light blue
      "Opposed" = "#ee4a70",            # Red-pink
      "Neutral" = "#8d99ae",            # Gray
      "Support" = "#06d667"             # Green
    )
  ) +
  
  # Theme customization
  theme_minimal(base_size = 14) +
  theme(
    legend.title = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Print and save the plot
print(fig)
ggsave("../results/standard_model_run.png", plot = fig, width = 8, height = 6)