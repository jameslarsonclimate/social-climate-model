source("src/parameters.R")
source("src/opinions_component.R")
source("src/policy_component.R")
source("src/adoption_component.R")
source("src/emissions_component.R")
source("src/climate_component.R")
source("src/cognition_component.R")
load("../data/naturalvariability.Rdat")

model=function(time=1:81,
               homophily_param=homophily_param1,
               frac_opp_0=frac_opp_01,
               frac_neut_0=frac_neut_01,
               forcestrong=forcestrong1,
               forceweak=forceweak1,
               ced_param=ced_param1,
               pol_response=pol_response1,
               pol_window=pol_window1,
               pol_feedback=pol_feedback1,
               policy_pbcchange_max=policy_pbcchange_max1,
               policy_0=policy_01,
               adoptfrac_opp_0= adoptfrac_opp_01,
               adoptfrac_neut_0=adoptfrac_neut_01,
               adoptfrac_supp_0=adoptfrac_supp_01,
               pbc_mid=pbc_mid1,
               pbc_steep=pbc_steep1,
               pbc_opinionchange=pbc_opinionchange1,
               pbc_0=pbc_01,
               etc_mid=etc_mid1,
               etc_total=etc_total1,
               etc_steep=etc_steep1,
               normeffect=normeffect1,
               bau=bau1,
               bau_outside_region=bau_outside1,
               ex_forcing=ex_forcing1,
               m_max=m_max1,
               r_max=r_max1,
               r_0=r_01,
               adopt_effect=adopt_effect1,
               evidenceeffect=evidenceeffect1,
               biassedassimilation=biassedassimilation1,
               shiftingbaselines=shiftingbaselines1,
               shiftingExtremes=FALSE,
               year0=2020,
               natvar=NULL,
               historical=NULL,
               natvar_multiplier = natvar_multiplier1,
               temperature_input=NULL,
               policyopinionfeedback_param=policyopinionfeedback_01,
               lbd_param=lbd_param01,
               lag_param=lag_param01,
               temp_emissionsparam=temp_emissionsparam01
               ){
  
  if(shiftingExtremes==TRUE) {
    source("src/cognition_component-extremeInfluence.R")
  }
  
  startdist=c(frac_opp_0,frac_neut_0,1-(frac_opp_0+frac_neut_0))
  
  params_opp=c(homophily_param,(1-homophily_param)/2,(1-homophily_param)/2)
  params_neut=c((1-homophily_param)/2,homophily_param,(1-homophily_param)/2)
  params_supp=c((1-homophily_param)/2,(1-homophily_param)/2,homophily_param)
  homophily=list(params_opp,params_neut,params_supp)
  
  force_params=forcefunc(forcestrong,forceweak,forcestrong)
  
  distributions=matrix(nrow=length(time),ncol=3)
  distributions[1,]=startdist
  
  policy=numeric(length=length(time))
  policy[1]=policy_0
  
  adoptersfrac=matrix(nrow=length(time),ncol=3)
  adoptersfrac[1,]=c(adoptfrac_opp_0,adoptfrac_neut_0,adoptfrac_supp_0)
  
  nadopters=numeric(length=length(time))
  nadopters[1]=distributions[1,]%*%adoptersfrac[1,]
  
  pbc=numeric(length=length(time))
  pbc[1]=pbc_0
  
  emissions=numeric(length=length(time))
  emissions[1]=bau[1]*(1+(temp_emissionsparam*temp_0[1]))
  
  totalemissions=numeric(length=length(time))
  totalemissions[1]=(bau[1]+rowSums(bau_outside_region[1,]))*(1+(temp_emissionsparam*temp_0[1]))
  # for (i in 1:ncol(bau_outside_t)) {  # for loop to loop through each region outside OECD
  #   totalemissions[1]=(totalemissions[1]+bau_outside_region[1,i])*(1+(temp_emissionsparam*temp_0[1]))  # potential concern here of this scaling factor applied multiple times
  # }
  
    if(is.null(natvar)) {
      for(r in 1:1) {
        naturalvariability=Re(randomts(gtemp))[1:length(time)]*natvar_multiplier
      }
  }
  if(!is.null(natvar)) {
    if(historical == TRUE) {
        df <- read.csv("../data/giss_globaltemp_19501980anomaly_detrended2ndDeg.csv")
        # Extract the last 81 rows of the second column
        naturalvariability <- df[(nrow(df) - 80):nrow(df), 2]*natvar_multiplier/2
    }
  }


  emissions_outside = matrix(nrow=length(time), ncol=4)
  for (i in 1:ncol(emissions_outside)) {
    emissions_outside[1,i] = bau_outside_region[1,i]*(1+(temp_emissionsparam*temp_0[1]))
  }
  
  mitigation=matrix(0,nrow=length(time),ncol=length(time)) #must be all zeroes to start
  
  temperature=matrix(nrow=length(time),ncol=2)
  temperature[1,]=temp_0

  
  mass=matrix(nrow=length(time),ncol=3)
  mass[1,]=mass_0
  
  bau_temp=matrix(nrow=length(time),ncol=2)
  bau_temp[1,]=temp_0
  
  bau_mass=matrix(nrow=length(time),ncol=3)
  bau_mass[1,]=mass_0
  
  # if(is.null(natvar)) naturalvariability=Re(randomts(gtemp))[1:length(time)]*natvar_multiplier
  # if(!is.null(natvar)) naturalvariability=natvar
  
  weather=numeric(length=length(time))
  weather[1]=temperature[1,1]+naturalvariability[1]
  
  evidence=matrix(nrow=length(time),ncol=3)
  evidence[1,]=rep(0,3)
  
  anomaly=numeric(length=length(time))
  anomaly[1]=ifelse(shiftingbaselines==0,weather[1],naturalvariability[1]) # if shiftingBaselines is off, anomaly=weather
  # if shitingBaselines is on, anomaly=naturalVariability 
  
  for(t in 2:length(time)){
    distributions[t,]=opinionchange(distributions[t-1,],evidence[t-1,],evidence_effect=evidenceeffect,selfsimparams=homophily,force=force_params,policychange_t_1=ifelse(t==2,0,policy[t-1]-policy[t-2]),policyopinionfeedback=policyopinionfeedback_param,adopt_t_1=adoptersfrac[t-1,],ced=ced_param)
    policy[t]=policychange(distributions[t,],policy[t-1],ifelse(t>pol_window,mean(policy[(t-pol_window):(t-1)]),mean(policy[1:(t-1)])),responsiveness=pol_response,feedback=pol_feedback)
    
    temp=adopterschange(nadopters[t-1],adoptersfrac[t-1,],policy[t-1],distributions[t,],etcmid=etc_mid,etcsteep=etc_steep,total=etc_total,init_pbc=pbc_0,maxpolpbc=policy_pbcchange_max,pbcmid=pbc_mid,pbcsteep=pbc_steep,shift=pbc_opinionchange,normstrength=normeffect,selfsimparam=homophily)
    pbc[t]=temp[[1]]
    nadopters[t]=temp[[2]]
    adoptersfrac[t,]=temp[[3]]
    
    temp2=emissionschange(bau[t],nadopters[t],policy[t],mitigation,t,temperature[t-1,1],temperature_t_lag=ifelse(t<=lag_param,temperature[t-1,1],temperature[t-lag_param,1]),effectiveness=adopt_effect,maxm=m_max,rmax=r_max,r0=r_0,lbd=lbd_param,emissions_t_lag=ifelse(t<=lag_param|lag_param==0,emissions[1],emissions[t-lag_param]),bau_t_lag=ifelse(t<=lag_param|lag_param==0,bau[1],bau[t-lag_param]),bau_outside_t=bau_outside_region[t,],emissions_outside_t=emissions_outside[t,],lag=lag_param,temp_emissions=temp_emissionsparam)
    emissions[t]=temp2[[1]]
    mitigation=temp2[[2]]
    totalemissions[t]=temp2[[3]]
    emissions_outside[t,]=temp2[[4]]
    
    #climate model
    if(is.null(temperature_input)) {
        temp3=temperaturechange(temperature[t-1,],mass[t-1,],totalemissions[t],ex_forcing[t],bau[t]+rowSums(bau_outside_region[t,]),psi1_param=psi1,nu_param=nu)
        mass[t,]=temp3[[1]]
        temperature[t,]=temp3[[2]]
      }
    if(!is.null(temperature_input)) temperature=temperature_input
    
    temp4=temperaturechange(bau_temp[t-1,],bau_mass[t-1,],bau[t]+rowSums(bau_outside_region[t,]),ex_forcing[t],bau[t]+rowSums(bau_outside_region[t,]),psi1_param=psi1,nu_param=nu)
    bau_mass[t,]=temp4[[1]]
    bau_temp[t,]=temp4[[2]]
    weather[t]=temperature[t,1]+naturalvariability[t]
    # weather[t]=temperature[t,1]
    
    temp5=anomalyfunc(weather,t,biassedassimilation,shiftingbaselines)
    anomaly[t]=temp5[[1]]
    evidence[t,]=temp5[[2]]
    
  }
  a = list(time,distributions,policy,pbc,nadopters,adoptersfrac,emissions,mitigation,bau+bau_outside_region,mass,temperature,bau_temp,evidence,anomaly,year0:(year0+length(time)-1),totalemissions,naturalvariability,weather,emissions_outside,bau_outside_region)
  names(a)=c("time","distributions","policy","pbc","nadopters","adoptersfrac","emissions","mitigation","bau_total","mass","temp","bau_temp","evidence","anomaly","year","totalemissions","naturalvariability","weather","emissions_outside","bau_outside_region")
  
  return(a)
}
  



