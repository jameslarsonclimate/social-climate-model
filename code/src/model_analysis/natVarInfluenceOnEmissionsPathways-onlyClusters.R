library(ggplot2)
library(plot.matrix)
library(data.table)
library(tidyverse)
library(reshape2)
library(patchwork)
library(forcats)
library(EnvStats)
library(randomForest)
library(randomForestExplainer)

setwd("/Users/jglarson/Documents/Research/social-climate-model/code")
source("src/model_analysis/model_parametertune.R")

fig_suffix = ''




# ####------kmeans clustering of tuned output---------

params=fread(paste0("../results/MC Runs/MC Runs_TunedParams/params", fig_suffix, ".csv"))
pol=fread(paste0("../results/MC Runs/MC Runs_TunedParams/policy", fig_suffix, ".csv"))
ems=fread(paste0("../results/MC Runs/MC Runs_TunedParams/emissions", fig_suffix, ".csv"))

mc=dim(params)[1]

df=cbind(pol,ems)
df_scaled=scale(df)
df_scaled_temp = scale(df)  # Create temporary scaled version

# Save scaling attributes BEFORE removing columns
old_center <- attr(df_scaled_temp, "scaled:center")
old_scale <- attr(df_scaled_temp, "scaled:scale")

#drop zero variance columns
nacols=which(apply(df_scaled,MARGIN=2,function(x) sum(is.na(x)))==mc)
df_scaled=df_scaled[,-nacols]

# After removing NA columns, save the column indices you kept
valid_cols <- colnames(df_scaled)

# This section runs k‑means clustering with a range of cluster numbers (from 2 to 9) to compute the “within sum of squares” (WSS) for each option.
#visualize ideal number of clusters
nclustertest=2:9
wss=numeric(length=length(nclustertest))
set.seed(2090)
for(i in 1:length(nclustertest)){
  wss[i]=kmeans(df_scaled,nclustertest[i],iter.max=20)$tot.withinss
  print(i)
}
pdf(file="../results/figureS5.pdf")
plot(x=nclustertest,y=wss,type="b",xlab="Number of Clusters",ylab="Within Sum of Squares")
dev.off()

#five clusters looks good
nclus=5
set.seed(2090)
test=kmeans(df_scaled,nclus)

#plot outcomes over time for different clusters
ems=as.data.frame(ems)
ems$cluster=test$cluster

# The code groups the emissions data by cluster and computes the mean (average) emissions for each year within each cluster.
clems=ems%>%
  group_by(cluster)%>%
  summarize_all(mean)
colnames(clems)=c("cluster",2020:2100)
clems=melt(clems,id.vars="cluster")
colnames(clems)=c("Cluster","Year","Emissions")
clems$Cluster=as.factor(clems$Cluster)

# Calculate the Proportion of Runs per Cluster
# A frequency table is computed from the cluster assignments, and the counts are converted into percentages (proportion of the total Monte Carlo runs).
nruns=data.frame(table(ems$cluster))
colnames(nruns)=c("Cluster","nsims")
nruns$nsims=nruns$nsims/mc*100
clems$Year=as.numeric(as.character(clems$Year))

clems=merge(clems,nruns)

# Add names of scenarios and order from most to least common
clems$Cluster=fct_relevel(clems$Cluster, "2","3","1","5","4")
clems$Cluster=fct_recode(clems$Cluster,"Modal Path"="2","Aggresive Action"="3","Technical Challenges"="1","Little and Late"="4","Delayed Recognition"="5")

# Plot the Cluster-Averaged Emissions Trajectories
cols=c("#FED789", "#023743", "#72874E", "#476F84", "#A4BED5", "#c42449")
a=ggplot(clems,aes(x=Year,y=Emissions,group=Cluster,col=Cluster,lwd=nsims))+geom_line()+theme_bw()+theme(text=element_text(size=16))
a=a+scale_color_manual(values=cols)+labs(x="",color="Cluster",lwd="Percent of Runs",y="Global Emissions (GtC per year)")+guides(color = guide_legend(override.aes = list(size = 2)))
a=a+theme(legend.position="none")
a=a+scale_y_continuous(limits=c(-0.1,18))
# ggsave("../results/figure3_emissions.pdf",plot=a) 
# ggsave(paste0("../results/figure3_emissionsPathways/figure3_emissions", fig_suffix, ".pdf"), plot=a)
ggsave(paste0("../results/figure3_emissionsPathways/figure3_emissions", fig_suffix, ".jpg"), plot=a)


pol=as.data.frame(pol)
pol$cluster=test$cluster
clpol=pol%>%
  group_by(cluster)%>%
  summarize_all(mean)
colnames(clpol)=c("cluster",2020:2100)
clpol=melt(clpol,id.vars="cluster")
colnames(clpol)=c("Cluster","Year","Policy")
clpol$Cluster=as.factor(clpol$Cluster)
clpol=merge(clpol,nruns)
clpol$Year=as.numeric(as.character(clpol$Year))

clpol$Cluster=fct_relevel(clpol$Cluster, "2","3","1","5","4")
clpol$Cluster=fct_recode(clpol$Cluster,"Modal Path"="2","Aggresive Action"="3","Technical Challenges"="1","Little and Late"="4","Delayed Recognition"="5")

b=ggplot(clpol,aes(x=Year,y=Policy,group=Cluster,col=Cluster,lwd=nsims))+geom_line()+theme_bw()
b=b+scale_color_manual(values=cols)+labs(x="",color="Cluster",lwd="Percent of Runs",y="Climate Policy Stringency")+ theme(legend.position="none",text=element_text(size=16))
# ggsave("../results/figure3_policy.pdf",plot=b)
ggsave(paste0("../results/figure3_emissionsPathways/figure3_policy", fig_suffix, ".pdf"), plot=a)


#parameter combinations associated with each cluster
params_cluster=scale(params)
params_cluster=data.frame(params_cluster,cluster=test$cluster)
params_cluster=params_cluster%>%
  group_by(cluster)%>%
  summarize_all(mean)

colnames(params_cluster)=c("Cluster",colnames(params_cluster)[2:10],"Max Mit. Rate","Max Mit Time","CED","Policy-Adoption","ACost_Init","ACost_Steep","Opinion-Adoption","ETC Effect","Social Norm Effect","Adoption Effect","LBD Effect","Lag Time","Temp-Emissions")
#drop weak force as it doesn't add anything interesting over just the strong force
params_cluster=params_cluster[,-which(colnames(params_cluster)=="Weak.Force")]

params_cluster=melt(params_cluster,id.var="Cluster")
params_cluster$Cluster=as.factor(params_cluster$Cluster)

params_cluster$Cluster=fct_relevel(params_cluster$Cluster, "2","3","1","5","4")
params_cluster$Cluster=fct_recode(params_cluster$Cluster,"Modal Path"="2","Aggresive Action"="3","Technical Challenges"="1","Little and Late"="4","Delayed Recognition"="5")

#order parameters to group by component
params_cluster$variable=fct_relevel(params_cluster$variable,"Homophily","Strong.Force","Evidence","Pol.Opinion","CED","Policy-Adoption","ACost_Init","ACost_Steep","Opinion-Adoption","ETC Effect","Social Norm Effect","Status.Quo.Bias","Pol.Int.Feedback","Max Mit. Rate","Max Mit Time","LBD Effect","Lag Time","Temp-Emissions","Adoption Effect","Biased.Assimilation","Shifting.Baselines")
  
d=ggplot(params_cluster,aes(x=variable,y=value,group=Cluster,fill=Cluster))+geom_bar(stat="identity",position="dodge")
d=d+scale_fill_manual(values=cols)+labs(x="",y="Cluster Mean Value",fill="Cluster")+theme_bw()+theme(axis.text.x = element_text(angle = 90))
d=d+theme(legend.position="none")
# ggsave("../results/figureS3.pdf",plot=d)
ggsave(paste0("../results/figure3_emissionsPathways/figureS3", fig_suffix, ".pdf"), plot=a)




##############################################################################################################################
# Read in new data, reuse the old clusters, and plot
##############################################################################################################################


print('Reading in new data')
# fig_suffix = '_pulseTempAnom_2K_2030-2040'
# fig_suffix = ''
# fig_suffix = '_pulseTempAnom_2K_2070-2080'
# fig_suffix = '_noNatVar'
# fig_suffix = '_fixedNatVar-highClimateSupport'
# fig_suffix = '_fixedNatVar-mediumClimateSupport'
# fig_suffix = '_fixedNatVar-lackOfClimateSupport'
fig_suffix = '_varyInitialDistribution'

params=fread(paste0("../results/MC Runs/MC Runs_TunedParams/params", fig_suffix, ".csv"))
pol=fread(paste0("../results/MC Runs/MC Runs_TunedParams/policy", fig_suffix, ".csv"))
ems=fread(paste0("../results/MC Runs/MC Runs_TunedParams/emissions", fig_suffix, ".csv"))

mc=dim(params)[1]

df=cbind(pol,ems) # combine policy and emissions data
# Only select columns that existed in the original scaled data
# df <- df[, valid_cols]

# df_scaled=scale(df) # scale the data
# old_center <- attr(df_scaled, "scaled:center")
# old_scale  <- attr(df_scaled, "scaled:scale")
df_scaled <- scale(df, center = old_center, scale = old_scale)
# df_scaled <- scale(df, center = attr(df_scaled_original, "scaled:center")[valid_cols], 
#                       scale = attr(df_scaled_original, "scaled:scale")[valid_cols])

#drop zero variance columns
nacols=which(apply(df_scaled,MARGIN=2,function(x) sum(is.na(x)))==mc)
df_scaled=df_scaled[,-nacols]

# Function to find closest centroid for each observation
assign_to_cluster <- function(data_point, centroids) {
  distances <- apply(centroids, 1, function(c) sum((data_point - c)^2))
  return(which.min(distances))
}

# Apply function to each row of the new scaled data
print("Assigning new data points to closest clusters...")
new_clusters <- apply(df_scaled, 1, function(row) assign_to_cluster(row, test$centers))


# Commenting the below so as to reuse the previously defined clusters

# # This section runs k‑means clustering with a range of cluster numbers (from 2 to 9) to compute the “within sum of squares” (WSS) for each option.
# #visualize ideal number of clusters
# nclustertest=2:9
# wss=numeric(length=length(nclustertest))
# set.seed(2090)
# for(i in 1:length(nclustertest)){
#   wss[i]=kmeans(df_scaled,nclustertest[i],iter.max=20)$tot.withinss
#   print(i)
# }
# pdf(file="../results/figureS5.pdf")
# plot(x=nclustertest,y=wss,type="b",xlab="Number of Clusters",ylab="Within Sum of Squares")
# dev.off()

# #five clusters looks good
# nclus=5
# set.seed(2090)
# test=kmeans(df_scaled,nclus)


#plot outcomes over time for different clusters
# This section assigns the new data to the existing clusters
ems=as.data.frame(ems)

# ems$cluster=test$cluster
# With this new line that uses the properly assigned clusters:
ems$cluster=new_clusters

# The code groups the emissions data by cluster and computes the mean (average) emissions for each year within each cluster.
clems=ems%>% # group the data by cluster
  group_by(cluster)%>% # summarize the data
  summarize_all(mean) # compute the mean for each cluster
# The code reshapes the data from wide format to long format, making it easier to plot.
colnames(clems)=c("cluster",2020:2100) # set column names
clems=melt(clems,id.vars="cluster") # reshape the data
colnames(clems)=c("Cluster","Year","Emissions") # set column names
clems$Cluster=as.factor(clems$Cluster) # convert the cluster variable to a factor

# Calculate the Proportion of Runs per Cluster
# A frequency table is computed from the cluster assignments, and the counts are converted into percentages (proportion of the total Monte Carlo runs).
nruns=data.frame(table(ems$cluster))
colnames(nruns)=c("Cluster","nsims")
nruns$nsims=nruns$nsims/mc*100
clems$Year=as.numeric(as.character(clems$Year))

clems=merge(clems,nruns) # merge the data frames to include the number of simulations per cluster

# Add names of scenarios and order from most to least common
clems$Cluster=fct_relevel(clems$Cluster, "2","3","1","5","4")
clems$Cluster=fct_recode(clems$Cluster,"Modal Path"="2","Aggresive Action"="3","Technical Challenges"="1","Little and Late"="4","Delayed Recognition"="5")

# Plot the Cluster-Averaged Emissions Trajectories
cols=c("#FED789", "#023743", "#72874E", "#476F84", "#A4BED5", "#c42449")
a=ggplot(clems,aes(x=Year,y=Emissions,group=Cluster,col=Cluster,lwd=nsims))+geom_line()+theme_bw()+theme(text=element_text(size=16))
a=a+scale_color_manual(values=cols)+labs(x="",color="Cluster",lwd="Percent of Runs",y="Global Emissions (GtC per year)")+guides(color = guide_legend(override.aes = list(size = 2)))
a=a+theme(legend.position="none")
a=a+scale_y_continuous(limits=c(-0.1,18))
# ggsave("../results/figure3_emissions.pdf",plot=a) 
# ggsave(paste0("../results/figure3_emissionsPathways/figure3_emissions", fig_suffix, ".pdf"), plot=a)
ggsave(paste0("../results/figure3_emissionsPathways/figure3_emissions", fig_suffix, ".jpg"), plot=a)
