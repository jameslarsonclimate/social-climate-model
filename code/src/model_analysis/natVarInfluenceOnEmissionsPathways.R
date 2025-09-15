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
library(sn)
library(ncdf4)

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
# fig_suffix = '_initClimSupport40percent'
# fig_suffix = '_initClimSupportNormalDistribution-'
# fig_suffix = '_initClimSupportNormalDistribution-resample3'  # Change the seed!
# fig_suffix = '_initClimSupportNormalDistribution-natVarMultiplier4'
# fig_suffix = '_ERA5natVar'
# fig_suffix = '_ERA5natVar_locations'
# fig_suffix = '_ERA5natVar0.5'
# fig_suffix = '_CESM_LM_local_Tambora_2030_normalDistribution'  
# fig_suffix = '_CESM_LM_local_1850PIcntl_normalDistribution'  
# fig_suffix = '_CESM_LM_local_Tambora_2030_defaultDistribution'  
# fig_suffix = '_CESM_LM_local_Tambora_2030_defaultSupporterInitialDistribution'  # Change the seed!
# fig_suffix = '_CESM_LM_global_member10_Tambora_2030_normalDistribution_multiplier1'  
# fig_suffix = '_CESM_HR_local_natVar_multiplier05'
# fig_suffix = '_CESM_HR_local_natVar-totalGDPweighted'
fig_suffix = '_CESM_HR_local_natVar-popWeighted'
# fig_suffix = '_CESM_HR_local_natVar_defaultSupporterInitialDistribution'  # Change the seed!
# fig_suffix = '_CESM_HR_local_natVar_scale_sd'
# fig_suffix = 'volcanicCooling_2030_-1_seed2090  '  # Change the seed!


# #-------------Monte Carlo of full model, with mitigation, policy, and option parameters weighted by tuning-derived probability----------
source("src/model.R")

print('reading in MC Runs files')

polopparams=fread("../results/MC Runs/parameter_tune.csv")
mitparams=fread("../results/MC Runs/parameter_tune_mitigation.csv")

# natvarERA5 = fread("../data/ERA5.annual.t2m.detrended.landOnly.1940-2024.csv")[, 1:81, with = FALSE]
# natvarERA5 <- nc_open("../data/ERA5.annual.t2m.detrended.landOnly.1940-2024.nc")
# natvar_array <- ncvar_get(natvarCESM_HR, "TREFHT_land_samples") # shape: 81 x 500001
# natvar_mat <- t(natvar_array)
# nc_close(natvarCESM_HR)

# natvarCESM_HR <- nc_open("../../CESM-HR-PIctrl/CESM.HR.PIcntl.TREFHT.land_samples_81timesteps.nc")
# natvar_array <- ncvar_get(natvarCESM_HR, "TREFHT_land_samples") # shape: 81 x 500001
# natvar_mat <- t(natvar_array)
# nc_close(natvarCESM_HR)

# natvarCESM_HR <- nc_open("../../CESM-HR-PIctrl/TREFHT_land_samples_by_gdp.nc")
# natvar_array <- ncvar_get(natvarCESM_HR, "TREFHT_land_samples_weighted_by_gdp") # shape: 81 x 500001
# natvar_mat <- t(natvar_array)
# nc_close(natvarCESM_HR)

natvarCESM_HR <- nc_open("../../CESM-HR-PIctrl/TREFHT_land_samples_by_pop.nc")
natvar_array <- ncvar_get(natvarCESM_HR, "TREFHT_land_samples_weighted_by_gdp") # shape: 81 x 500001
natvar_mat <- t(natvar_array)
nc_close(natvarCESM_HR)

# natvarCESM <- nc_open("../../CESM-LastMillenium/TREFHT/CESM-LastMillenium-TREFHT_land_samples_81timesteps.nc")
# natvar_array <- ncvar_get(natvarCESM, "TREFHT_land_samples") 
# natvar_mat <- t(natvar_array)
# nc_close(natvarCESM)

# natvarCESM_LM_PIcntl <- nc_open("../../CESM-LastMillenium/TREFHT/CESM-LastMillenium-PIcntl-TREFHT_land_samples_81timesteps.nc")
# natvar_array <- ncvar_get(natvarCESM_LM_PIcntl, "TREFHT_land_samples") 
# natvar_mat <- t(natvar_array)
# nc_close(natvarCESM_LM_PIcntl)


#initial opinion distribution - not varied, but fixed at particular values from Pew Opinion Data
# frac_opp_01=0.07 
# frac_neut_01=0.22 

# CESM-LM *global* Tambora 2030, member 10
# natvarCESM = c(0.229, -0.282, 0.102, 0.229, -0.472, -0.471, -0.312, -0.137, -0.103, 0.035, -0.744, -1.131, -1.067, -0.685, -0.365, -0.133, -0.275, -0.444, -0.431, -0.388, 0.004, 0.070, 0.006, -0.123, 0.042, -0.187, -0.818, -0.371, -0.012, -0.240, -1.001, -0.768, -0.379, -0.099, -0.260, -0.163, 0.242, 0.214, 0.119, -0.306, -0.189, 0.322, 0.165, 0.019, 0.054, -0.119, 0.050, -0.150, 0.356, 0.061, -0.302, 0.068, -0.083, 0.059, 0.060, -0.098, -0.022, 0.177, -0.017, 0.141, 0.136, 0.310, -0.000, 0.009, -0.113, -0.414, -0.047, 0.041, -0.066, -0.112, 0.211, -0.051, -0.052, 0.375, -0.125, 0.326, 0.150, 0.274, -0.052, -0.453, -0.528
# )

mc=100000
params=matrix(nrow=mc,ncol=24)
pol=matrix(nrow=mc,ncol=81)
ems=matrix(nrow=mc,ncol=81)
climtemp=matrix(nrow=mc,ncol=81)
dist <- array(NA, dim = c(mc, 81, 3))
natvar=matrix(nrow=mc,ncol=81)
weather=matrix(nrow=mc,ncol=81)
frac_neut_mat=matrix(nrow=mc,ncol=81)
frac_opp_mat=matrix(nrow=mc,ncol=81)

# set.seed(2090)
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

  # ---- Sample initial opinion fractions via skew-normal ----
  # Support: skew-normal truncated to [0.3,0.8]
  repeat {
    x_sup <- rsn(1, xi = 0.37, omega = 0.2, alpha = 2.8)
    if (x_sup >= 0.30 && x_sup <= 0.80) {
      frac_supp_01 <- x_sup
      break
    }
  }

  # Opposition: skew-normal truncated to [0.1, 1 - frac_supp_01]
  repeat {
    x_opp <- rsn(1, xi = 0.3, omega = 0.2, alpha = 1)
    if (x_opp >= 0.10 && x_opp <= (1 - frac_supp_01)) {
      frac_opp_01 <- x_opp
      break
    }
  }
  # Neutral is the remainder
  frac_neut_01 <- 1 - (frac_supp_01 + frac_opp_01)


  #also add feedback from temperature to bau emissions
  temp_emissionsparam01=rtri(1,min=-0.102,max=0.001,mode=-0.031) #distribution based on Woodard et al., 2019 PNAS estimates

  # rowvals <- as.numeric(natvarERA5[i+1, ])
  # if (any(is.na(rowvals))) {
  #   print(paste("NA in natvarERA5 row", i+1))
  #   next
  # }

  # If updating the model parameters, make sure to update fig_suffix as well!
  m=tryCatch(model(natvar=natvar_mat[i+1,], natvar_multiplier = 1), error = function(e) {  # natvar=natvar_mat[i+1,], natvar_multiplier = 1# model(temperature_anomaly = ts), natvar_multiplier =  0
      skip_to_next <<- TRUE
      print(paste("Error occurred, skipping iteration", i, ":", e$message))
      # Store diagnostic values if available
      if (exists("m") && is.list(m)) {
        if (!exists("error_natvar")) error_natvar <<- list()
        if (!exists("error_weather")) error_weather <<- list()
        if (!exists("error_temp")) error_temp <<- list()
        error_natvar[[length(error_natvar)+1]] <<- tryCatch(m$naturalvariability, error=function(e) rep(NA,81))
        error_weather[[length(error_weather)+1]] <<- tryCatch(m$weather, error=function(e) rep(NA,81))
        error_temp[[length(error_temp)+1]] <<- tryCatch(m$temp[,1], error=function(e) rep(NA,81))
      }
  })

  if(skip_to_next) { 
      if(!exists("e")) print(paste("Skipping iteration", i)) # In case skip_to_next was set elsewhere
      next 
  }

  #save output
  params[i,]=c(polops,mit,ced_param1,policy_pbcchange_max1,pbc_01,pbc_steep1,opchangeparam,etc_total1,normeffect1,adopt_effect1,lbd_param01,lag_param01,temp_emissionsparam01, frac_neut_01, frac_opp_01)
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
colnames(params)=c(colnames(polopparams)[1:9],colnames(mitparams)[1:2],"ced","policy_pbc","pbc_init","pbc_steep","policy_adoption","etc_total","normeffect","adopt_effect","lbd_param","lag_param","temp_emissions", "frac_neut_01", "frac_opp_01")

dir.create("../results/MC Runs/MC Runs_TunedParams/")
fwrite(params,file=paste0("../results/MC Runs/MC Runs_TunedParams/params", fig_suffix, ".csv"))
fwrite(pol,file=paste0("../results/MC Runs/MC Runs_TunedParams/policy", fig_suffix, ".csv"))
fwrite(ems,file=paste0("../results/MC Runs/MC Runs_TunedParams/emissions", fig_suffix, ".csv"))
fwrite(climtemp,file=paste0("../results/MC Runs/MC Runs_TunedParams/temperature", fig_suffix, ".csv"))
# fwrite(dist,file=paste0("../results/MC Runs/MC Runs_TunedParams/distributions", fig_suffix, ".csv"))
save(dist, file=paste0("../results/MC Runs/MC Runs_TunedParams/distributions", fig_suffix, ".Rdata"))
fwrite(natvar,file=paste0("../results/MC Runs/MC Runs_TunedParams/natvar", fig_suffix, ".csv"))
fwrite(weather,file=paste0("../results/MC Runs/MC Runs_TunedParams/weather", fig_suffix, ".csv"))


