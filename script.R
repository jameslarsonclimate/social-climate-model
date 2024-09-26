
#install.packages('ggplot2')
#install.packages('forcats')
#install.packages('metR')
print('test')
setwd("~/Documents/Research/Moore-etal-2022-code")
load("../data/naturalvariability.Rdat")

#source("src/model_analysis/shiftingBaselinesAndClimateVariability.R")

print("running")
source("src/model_analysis/tippingelements_analysis.R")
#source("src/model_analysis/tippingelements_analysis-commentedByJL.R")

#source("src/model_analysis/montecarlo_parametertune.R")


print("finished running")

#note that colors of emissions-policy trajectories in figure 3 will differ from that of the manuscript because order of clusters from k-means clustering is random and can not be standardized using a seed in this environment. Legends have been removed from Figure 3 and Figure S3 for this reason
