# Load necessary source files for different components of the model
source("src/parameters.R")
source("src/opinions_component.R")
source("src/policy_component.R")
source("src/adoption_component.R")
source("src/emissions_component.R")
source("src/climate_component.R")
source("src/cognition_component.R")
load("../data/naturalvariability.Rdat")

# Define the main model function
model=function(time=1:81,  # Simulation time period (years)
               homophily_param=homophily_param1,  # Homophily parameter (social conformity)
               frac_opp_0=frac_opp_01,  # Initial fraction of opposition
               frac_neut_0=frac_neut_01,  # Initial fraction of neutral individuals
               forcestrong=forcestrong1,  # Strength of external pressure
               forceweak=forceweak1,  # Weak force for external pressure
               ced_param=ced_param1,  # Credibility-enhancing display parameter
               pol_response=pol_response1,  # Policy responsiveness to public opinion
               pol_window=pol_window1,  # Window for policy feedback
               pol_feedback=pol_feedback1,  # Strength of policy feedback
               policy_pbcchange_max=policy_pbcchange_max1,  # Max policy-driven behavior change
               policy_0=policy_01,  # Initial policy value
               adoptfrac_opp_0= adoptfrac_opp_01,  # Initial fraction of adopters opposing
               adoptfrac_neut_0=adoptfrac_neut_01,  # Initial fraction of neutral adopters
               adoptfrac_supp_0=adoptfrac_supp_01,  # Initial fraction of supporting adopters
               pbc_mid=pbc_mid1,  # Midpoint for policy-driven behavior change
               pbc_steep=pbc_steep1,  # Steepness of policy-driven behavior change curve
               pbc_opinionchange=pbc_opinionchange1,  # Strength of policy-behavior opinion change
               pbc_0=pbc_01,  # Initial behavior change fraction
               etc_mid=etc_mid1,  # Midpoint of emissions technology change curve
               etc_total=etc_total1,  # Total emissions technology change potential
               etc_steep=etc_steep1,  # Steepness of emissions technology change
               normeffect=normeffect1,  # Norm effect parameter
               bau=bau1,  # Business-as-usual emissions
               bau_outside_region=bau_outside1,  # BAU emissions outside region of interest
               ex_forcing=ex_forcing1,  # External forcing (e.g., natural variability)
               m_max=m_max1,  # Maximum mitigation effort
               r_max=r_max1,  # Maximum response rate to mitigation
               r_0=r_01,  # Initial response rate
               adopt_effect=adopt_effect1,  # Effect of adoption on emissions
               evidenceeffect=evidenceeffect1,  # Effect of evidence on opinion change
               biassedassimilation=biassedassimilation1,  # Biased assimilation of climate evidence
               shiftingbaselines=shiftingbaselines1,  # Shifting baselines for perception of climate
               year0=2020,  # Starting year
               natvar=NULL,  # Natural variability (can be externally supplied)
               policyopinionfeedback_param=policyopinionfeedback_01,  # Feedback from policy to opinions
               lbd_param=lbd_param01,  # Learning-by-doing parameter
               lag_param=lag_param01,  # Lag in temperature response to emissions
               temp_emissionsparam=temp_emissionsparam01  # Temperature-emissions feedback parameter
){
  
  # Initial opinion distribution (opposition, neutral, support)
  startdist=c(frac_opp_0, frac_neut_0, 1-(frac_opp_0+frac_neut_0))
  
  # Homophily matrix defining opinion group interactions
  params_opp=c(homophily_param, (1-homophily_param)/2, (1-homophily_param)/2)
  params_neut=c((1-homophily_param)/2, homophily_param, (1-homophily_param)/2)
  params_supp=c((1-homophily_param)/2, (1-homophily_param)/2, homophily_param)
  homophily=list(params_opp, params_neut, params_supp)
  
  # Force parameters for external influence (strong and weak)
  force_params=forcefunc(forcestrong, forceweak, forcestrong)
  
  # Initialize matrices and vectors to store simulation results over time
  distributions=matrix(nrow=length(time), ncol=3)  # Opinion distribution
  distributions[1,]=startdist  # Initial distribution
  
  policy=numeric(length=length(time))  # Policy values
  policy[1]=policy_0  # Initial policy
  
  adoptersfrac=matrix(nrow=length(time), ncol=3)  # Fraction of adopters in each group
  adoptersfrac[1,]=c(adoptfrac_opp_0, adoptfrac_neut_0, adoptfrac_supp_0)
  
  nadopters=numeric(length=length(time))  # Total number of adopters
  nadopters[1]=distributions[1,] %*% adoptersfrac[1,]
  
  pbc=numeric(length=length(time))  # Policy-driven behavior change
  pbc[1]=pbc_0  # Initial value
  
  emissions=numeric(length=length(time))  # Emissions
  emissions[1]=bau[1]*(1+(temp_emissionsparam*temp_0[1]))
  
  totalemissions=numeric(length=length(time))  # Total emissions including external factors
  totalemissions[1]=(bau[1]+bau_outside_region[1])*(1+(temp_emissionsparam*temp_0[1]))
  
  mitigation=matrix(0, nrow=length(time), ncol=length(time))  # Mitigation matrix (starts with zeros)
  
  temperature=matrix(nrow=length(time), ncol=2)  # Temperature values (two columns for different scenarios)
  temperature[1,]=temp_0  # Initial temperature
  
  mass=matrix(nrow=length(time), ncol=3)  # Mass matrix (used in climate component)
  mass[1,]=mass_0  # Initial mass
  
  bau_temp=matrix(nrow=length(time), ncol=2)  # Business-as-usual temperature
  bau_temp[1,]=temp_0  # Initial BAU temperature
  
  bau_mass=matrix(nrow=length(time), ncol=3)  # Business-as-usual mass
  bau_mass[1,]=mass_0  # Initial BAU mass
  
  # Handle natural variability (either external or generated)
  if(is.null(natvar)) naturalvariability=Re(randomts(gtemp))[1:length(time)]*8
  if(!is.null(natvar)) naturalvariability=natvar
  
  weather=numeric(length=length(time))  # Weather variable (temperature + variability)
  weather[1]=temperature[1,1]+naturalvariability[1]
  
  evidence=matrix(nrow=length(time), ncol=3)  # Evidence for opinion change
  evidence[1,]=rep(0, 3)  # Initial evidence (none)
  
  anomaly=numeric(length=length(time))  # Anomaly from shifting baselines or natural variability
  anomaly[1]=ifelse(shiftingbaselines==0, weather[1], naturalvariability[1])
  
  # Iterate through time steps to update model components
  for(t in 2:length(time)){
    # Update opinion distributions based on evidence, policy, and adopters
    distributions[t,]=opinionchange(distributions[t-1,], evidence[t-1,], evidence_effect=evidenceeffect, selfsimparams=homophily, force=force_params, policychange_t_1=ifelse(t==2, 0, policy[t-1]-policy[t-2]), policyopinionfeedback=policyopinionfeedback_param, adopt_t_1=adoptersfrac[t-1,], ced=ced_param)
    
    # Update policy based on opinion distributions, previous policy, and feedback
    policy[t]=policychange(distributions[t,], policy[t-1], ifelse(t>pol_window, mean(policy[(t-pol_window):(t-1)]), mean(policy[1:(t-1)])), responsiveness=pol_response, feedback=pol_feedback)
    
    # Update adopters and behavior change based on policy and distributions
    temp=adopterschange(nadopters[t-1], adoptersfrac[t-1,], policy[t-1], distributions[t,], etcmid=etc_mid, etcsteep=etc_steep, total=etc_total, init_pbc=pbc_0, maxpolpbc=policy_pbcchange_max, pbcmid=pbc_mid, pbcsteep=pbc_steep, shift=pbc_opinionchange, normstrength=normeffect, selfsimparam=homophily)
    pbc[t]=temp[[1]]  # Policy-driven behavior change
    nadopters[t]=temp[[2]]  # Total adopters
    adoptersfrac[t,]=temp[[3]]  # Fraction of adopters in each group
    
    # Update emissions based on BAU and policy changes
    emissions[t]=emissionschange(bau[t], bau[t-1], pbc[t], policy[t], techparams=etc_total, policychangemax=policy_pbcchange_max, ad_eff=adopt_effect, tempemissionsparam=temp_emissionsparam, temp_1=temperature[t-1,1])
    totalemissions[t]=totalemissionschange(emissions[t], bau_outside_region[t], temp_1=temperature[t-1,1], tempemissionsparam=temp_emissionsparam)
    
    # Update climate based on emissions
    temp=climatechange(totalemissions[t], totalemissions[t-1], mass[t-1,], temp[t-1,], forcings=ex_forcing[t], m_max, r_max, r_0, natvar=naturalvariability[t], tempfeedback=temp_emissionsparam, lag_param=lag_param, startyear=year0, t=t)
    temperature[t,]=temp[[1]]  # Update temperature
    mass[t,]=temp[[2]]  # Update mass
    
    # Update weather and anomaly for perception
    weather[t]=temperature[t,1]+naturalvariability[t]
    anomaly[t]=ifelse(shiftingbaselines==0, weather[t], naturalvariability[t])
    
    # Update evidence for opinion change based on weather, policy, and anomaly
    evidence[t,]=climate_evidence(anomaly[t], policy[t], temp[t,1], biasassimilation=biassedassimilation)
  }
  
  # Compile the model results into a list for return
  return(list("opinion_distribution"=distributions,
              "policy"=policy,
              "adopters_frac"=adoptersfrac,
              "nadopters"=nadopters,
              "pbc"=pbc,
              "emissions"=emissions,
              "totalemissions"=totalemissions,
              "mitigation"=mitigation,
              "temperature"=temperature,
              "mass"=mass,
              "bau_temp"=bau_temp,
              "bau_mass"=bau_mass,
              "naturalvariability"=naturalvariability,
              "weather"=weather,
              "evidence"=evidence,
              "anomaly"=anomaly))
}
