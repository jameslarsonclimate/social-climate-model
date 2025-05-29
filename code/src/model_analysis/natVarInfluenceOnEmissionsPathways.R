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

# fig_suffix = '_pulseTempAnom_2K_2030-2040'
# fig_suffix = '_pulseTempAnom_2K_2070-2080'
# fig_suffix = '_noNatVar'
# fig_suffix = ''
# fig_suffix = '_fixedNatVar-highClimateSupport'
# fig_suffix = '_fixedNatVar-moderateClimateSupport'
# fig_suffix = '_fixedNatVar-lowClimateSupport'
# fig_suffix = '_varyInitialDistribution'
fig_suffix = '_initClimSupport40percent'


# Create a timeseries with a triangular pulse from index 10 to 20
# Initialize a vector of 81 zeros and define the peak value
ts <- numeric(81)
peak <- 2

# Create ascending values from index 10 to 15 and descending values from index 16 to 20
# ts[10:15] <- seq(0, peak, length.out = 6)
# ts[16:20] <- seq(peak - peak/5, 0, length.out = 5)
# ts[50:55] <- seq(0, peak, length.out = 6)
# ts[56:60] <- seq(peak - peak/5, 0, length.out = 5)


# # Example output of internally generated natural variability that results in:
# # - Majority climate supporters in ~2035
# # - Net zero emissions in ~2075
# # This is a climate that promotes high climate support
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

# Example output of internally generated natural variability that results in:
# - Majority climate supporters in ~2055
# - Net zero emissions in ~2090
# # This is a climate that promotes moderate climate support
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


# # Example output of internally generated natural variability that results in:
# # - Climate supporters never reach majority
# # - Emissions stay high (SSP3-7.0) for entirety of run
# # This is a climate that promotes low climate support (buisness as usual)
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



# #-------------Monte Carlo of full model, with mitigation, policy, and option parameters weighted by tuning-derived probability----------
source("src/model.R")

print('reading in MC Runs files')

polopparams=fread("../results/MC Runs/parameter_tune.csv")
mitparams=fread("../results/MC Runs/parameter_tune_mitigation.csv")

#initial opinion distribution - not varied, but fixed at particular values from Pew Opinion Data
# frac_opp_01=0.07 
# frac_neut_01=0.22 

mc=100000
params=matrix(nrow=mc,ncol=22)
pol=matrix(nrow=mc,ncol=81)
ems=matrix(nrow=mc,ncol=81)
climtemp=matrix(nrow=mc,ncol=81)
dist <- array(NA, dim = c(mc, 81, 3))
natvar=matrix(nrow=mc,ncol=81)
weather=matrix(nrow=mc,ncol=81)
frac_neut_mat=matrix(nrow=mc,ncol=81)
frac_opp_mat=matrix(nrow=mc,ncol=81)

set.seed(2090)
i=0

print('starting while loop')

while(i<=mc){
  skip_to_next=FALSE
  #draw mitigation, policy and opinion parameters, weighting by tuned probability
  polops=as.numeric(polopparams[sample(1:dim(polopparams)[1],size=1,prob=polopparams$sampleweight),1:9])
  homophily_param1=polops[1]
  forcestrong1=polops[2]
  forceweak1=polops[3]
  evidenceeffect1=polops[4]
  policyopinionfeedback_01=polops[5]
  pol_response1=polops[6]
  pol_feedback1=polops[7]
  biassedassimilation1=polops[8]
  shiftingbaselines1=polops[9]
  
  mit=as.numeric(mitparams[sample(1:dim(mitparams)[1],size=1,prob=mitparams$sampleweight),1:2])
  m_max1=mit[1]
  r_max1=mit[2]
  
  #uniform sampling of other model parameters -mostly adoption-related
  ced_param1=runif(1,0,0.5)
  policy_pbcchange_max1=runif(1,0,1)
  pbc_01=runif(1,-2,0)
  pbc_steep1=runif(1,1,3)
  opchangeparam=runif(1,0,1)
  pbc_opinionchange1=c(opchangeparam,0,-1*opchangeparam) #constrain opinion effect on adoption to be symmetric for opposers and supporters
  etc_total1=runif(1,0,2)
  normeffect1=runif(1,0,1)
  adopt_effect1=runif(1,0,0.3)
  lbd_param01=runif(1,0,0.3)
  lag_param01=round(runif(1,0,30))
  
  # uniform sampling of initial opinion fractions with individual bounds and sum constraint
  repeat {
    frac_opp_01  <- runif(1, 0.1, 0.8)
    frac_neut_01 <- runif(1, 0.1, 0.8)
    s <- frac_opp_01 + frac_neut_01
    # enforce sum between 0.2 and 0.8 and each frac between 0.2 and 0.8
    if (s >= 0.2 && s <= 0.8) break
  }

  # Set the initial opinion distribution
  frac_opp_01 = 0.3
  frac_neut_01 = 0.3

#also add feedback from temperature to bau emissions
temp_emissionsparam01=rtri(1,min=-0.102,max=0.001,mode=-0.031) #distribution based on Woodard et al., 2019 PNAS estimates

# If updating the model parameters, make sure to update fig_suffix as well!
m=tryCatch(model(), error = function(e) {  # model(temperature_anomaly = ts), natvar_multiplier = 0
    skip_to_next <<- TRUE
    print(paste("Error occurred, skipping iteration", i, ":", e$message))
})

if(skip_to_next) { 
    if(!exists("e")) print(paste("Skipping iteration", i)) # In case skip_to_next was set elsewhere
    next 
}

#save output
params[i,]=c(polops,mit,ced_param1,policy_pbcchange_max1,pbc_01,pbc_steep1,opchangeparam,etc_total1,normeffect1,adopt_effect1,lbd_param01,lag_param01,temp_emissionsparam01)
pol[i,]=m$policy
ems[i,]=m$totalemissions
climtemp[i,]=m$temp[,1]
dist[i,,]=m$distributions
natvar[i,]=m$naturalvariability
weather[i,]=m$weather
frac_neut_mat[i,]=frac_neut_01
frac_opp_mat[i,]=frac_opp_01

if(i%%1000==0) print(i)
i=i+1
}
colnames(params)=c(colnames(polopparams)[1:9],colnames(mitparams)[1:2],"ced","policy_pbc","pbc_init","pbc_steep","policy_adoption","etc_total","normeffect","adopt_effect","lbd_param","lag_param","temp_emissions")

dir.create("../results/MC Runs/MC Runs_TunedParams/")
fwrite(params,file=paste0("../results/MC Runs/MC Runs_TunedParams/params", fig_suffix, ".csv"))
fwrite(pol,file=paste0("../results/MC Runs/MC Runs_TunedParams/policy", fig_suffix, ".csv"))
fwrite(ems,file=paste0("../results/MC Runs/MC Runs_TunedParams/emissions", fig_suffix, ".csv"))
fwrite(climtemp,file=paste0("../results/MC Runs/MC Runs_TunedParams/temperature", fig_suffix, ".csv"))
# fwrite(dist,file=paste0("../results/MC Runs/MC Runs_TunedParams/distributions", fig_suffix, ".csv"))
save(dist, file=paste0("../results/MC Runs/MC Runs_TunedParams/distributions", fig_suffix, ".Rdata"))
fwrite(natvar,file=paste0("../results/MC Runs/MC Runs_TunedParams/natvar", fig_suffix, ".csv"))
fwrite(weather,file=paste0("../results/MC Runs/MC Runs_TunedParams/weather", fig_suffix, ".csv"))


# ####------kmeans clustering of tuned output---------


params=fread(paste0("../results/MC Runs/MC Runs_TunedParams/params", fig_suffix, ".csv"))
pol=fread(paste0("../results/MC Runs/MC Runs_TunedParams/policy", fig_suffix, ".csv"))
ems=fread(paste0("../results/MC Runs/MC Runs_TunedParams/emissions", fig_suffix, ".csv"))

mc=dim(params)[1]

df=cbind(pol,ems)
df_scaled=scale(df)
#drop zero variance columns
nacols=which(apply(df_scaled,MARGIN=2,function(x) sum(is.na(x)))==mc)
df_scaled=df_scaled[,-nacols]

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

clems=ems%>%
  group_by(cluster)%>%
  summarize_all(mean)
colnames(clems)=c("cluster",2020:2100)
clems=melt(clems,id.vars="cluster")
colnames(clems)=c("Cluster","Year","Emissions")
clems$Cluster=as.factor(clems$Cluster)

nruns=data.frame(table(ems$cluster))
colnames(nruns)=c("Cluster","nsims")
nruns$nsims=nruns$nsims/mc*100
clems$Year=as.numeric(as.character(clems$Year))

clems=merge(clems,nruns)

#add names of scenarios and order from most to least common
clems$Cluster=fct_relevel(clems$Cluster, "2","3","1","5","4")
clems$Cluster=fct_recode(clems$Cluster,"Modal Path"="2","Aggresive Action"="3","Technical Challenges"="1","Little and Late"="4","Delayed Recognition"="5")

cols=c("#FED789", "#023743", "#72874E", "#476F84", "#A4BED5", "#c42449")
a=ggplot(clems,aes(x=Year,y=Emissions,group=Cluster,col=Cluster,lwd=nsims))+geom_line()+theme_bw()+theme(text=element_text(size=16))
a=a+scale_color_manual(values=cols)+labs(x="",color="Cluster",lwd="Percent of Runs",y="Global Emissions (GtC per year)")+guides(color = guide_legend(override.aes = list(size = 2)))
a=a+theme(legend.position="none")
# ggsave("../results/figure3_emissions.pdf",plot=a) 
ggsave(paste0("../results/figure3_emissions", fig_suffix, ".pdf"), plot=a)
ggsave(paste0("../results/figure3_emissions", fig_suffix, ".jpg"), plot=a)


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
ggsave(paste0("../results/figure3_policy", fig_suffix, ".pdf"), plot=a)


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
ggsave(paste0("../results/figureS3", fig_suffix, ".pdf"), plot=a)


#run cluster emissions paths through the climate component to generate temperature trajectories

# emissionssplit=split(clems,clems$Cluster)
# source("src/climate_component.R")

# cltemp=data.frame(Year=2020:2100)

# emissions=read.csv("../data/emissions_ssp3_rcp7.csv")
# bau1=emissions[,3]/1000*12/(12+16+16) #conversion factor from MtCO2 per year to GtC per year
# bau_outside1=emissions[,4]/1000*12/(12+16+16)
# ex_forcing1=emissions[,5]

# for(i in 1:length(emissionssplit)){
 
#   emissions_dat=emissionssplit[[i]]$Emissions[order(emissionssplit[[i]]$Year)]
  
#   #initialize ocean and atmosphere and carbon masses
#   temperature=matrix(nrow=length(emissions_dat),ncol=2)
#   temperature[1,]=temp_0
  
#   mass=matrix(nrow=length(emissions_dat),ncol=3)
#   mass[1,]=mass_0
  
#   for(t in 2:length(emissions_dat)){
#     temp3=temperaturechange(temperature[t-1,],mass[t-1,],emissions_dat[t],ex_forcing1[t],bau1[t]+bau_outside1[t],psi1_param=psi1,nu_param=nu)
#     mass[t,]=temp3[[1]]
#     temperature[t,]=temp3[[2]]
#   } 
#   cltemp=cbind(cltemp,temperature[,1])
# }

# #adjust temperature baseline to 1850-1900 average using global temperature time series
# tempdat=read.csv("../data/giss_globaltemp_19501980anomaly.csv")
# #adjustment from 1900 DICE climate model baseline
# adj=mean(tempdat$No_Smoothing[which(tempdat$Year%in%1880:1910)])-mean(tempdat$No_Smoothing[which(tempdat$Year%in%1895:1905)])

# #get 2091-2100 mean temperature change relative to 1880-1910 period
# colMeans(cltemp[which(cltemp$Year%in%2091:2100),2:6])-adj

# #### Probability distribution of temperature changes
# cltemp=as.matrix(fread(file="../results/MC Runs/MC Runs_TunedParams/temperature.csv"))

# temps=rowMeans(cltemp[,which(2020:2100%in%2091:2100)])-adj

#-------------Random Forest Modeling of Model Output ---------------------

# years=2020:2100

# params=fread("../results/MC Runs/MC Runs_TunedParams/params.csv")
# pol=fread("../results/MC Runs/MC Runs_TunedParams/policy.csv")
# ems=fread("../results/MC Runs/MC Runs_TunedParams/emissions.csv")
# colnames(params)=c(colnames(params)[1:9],"Max Mit. Rate","Max Mit Time","CED","Policy-Adoption","ACost_Init","ACost_Steep","Opinion-Adoption","ETC Effect","Social Norm Effect","Adoption Effect","LBD Effect","Lag Time","Temp-Emissions")

# y_ems=rowSums(ems) #dependent variable is cumulative emissions over the 21st century
# y_pol=as.matrix(pol)[,which(years==2030)]

# sampsize=10000
# samp=sample(1:length(y_ems),sampsize,replace=FALSE)

# rf_ems=randomForest(x=params[samp,],y=y_ems[samp],importance=TRUE, tree=TRUE,nodesize=100,mtry=5,ntree=300)
# rf_pol=randomForest(x=params[samp,],y=y_pol[samp],importance=TRUE,tree=TRUE,nodesize=100,mtry=5,ntree=300)

# min_depth_ems=min_depth_distribution(rf_ems)
# min_depth_pol=min_depth_distribution(rf_pol)

# a=plot_min_depth_distribution(min_depth_ems, mean_sample = "all_trees", k = 10)
# a=a+labs(x="",title="Cumulative Emissions 2020-2100")
# ggsave("../results/figureS4_emissions.pdf",plot=a)

# b=plot_min_depth_distribution(min_depth_pol, mean_sample = "all_trees", k = 10)
# b=b+labs(x="",title="2030 Policy")
# ggsave("../results/figureS4_policy.pdf",plot=b)


# #random forest interactions
# imp_ems=important_variables(rf_ems, k = 8, measures = c("mean_min_depth", "no_of_nodes"))
# imp_pol=important_variables(rf_pol, k=8, measures=c("mean_min_depth","no_of_nodes"))
# int_ems=min_depth_interactions(rf_ems,imp_ems)
# int_pol=min_depth_interactions(rf_pol,imp_pol)

# save(rf_ems,rf_pol,imp_ems,imp_pol,int_ems,int_pol,file="../results/MC Runs/randomforests.Rdat")
