# Load required source files for various model components
source("src/parameters.R")           # Loads parameters for the model
source("src/opinions_component.R")    # Loads the opinion dynamics component
source("src/policy_component.R")      # Loads the policy change component
source("src/adoption_component.R")    # Loads the adoption dynamics component
source("src/emissions_component.R")   # Loads the emissions change component
source("src/climate_component.R")     # Loads the climate model component
source("src/cognition_component.R")   # Loads the cognition and perception component
load("../data/naturalvariability.Rdat") # Loads natural variability data for climate modeling

# Define the core model function, with customizable parameters for simulation
model=function(time=1:81,               # Time steps for the simulation (years)
               homophily_param=homophily_param1,   # Homophily parameter: affects opinion clustering
               frac_opp_0=frac_opp_01,            # Initial fraction of opposition (opposed to policy)
               frac_neut_0=frac_neut_01,          # Initial fraction of neutrals (undecided)
               forcestrong=forcestrong1,          # Strong external force on opinions
               forceweak=forceweak1,              # Weak external force on opinions
               ced_param=ced_param1,              # Credibility-enhancing displays effect on opinions
               pol_response=pol_response1,        # Political responsiveness to opinion change
               pol_window=pol_window1,            # Time window for policy averaging (smoothing)
               pol_feedback=pol_feedback1,        # Feedback effect of policy on opinions
               policy_pbcchange_max=policy_pbcchange_max1, # Max effect of policy on perceived behavioral control (PBC)
               policy_0=policy_01,                # Initial policy value
               adoptfrac_opp_0= adoptfrac_opp_01, # Initial fraction of adopters in opposition
               adoptfrac_neut_0=adoptfrac_neut_01,# Initial fraction of adopters in neutral stance
               adoptfrac_supp_0=adoptfrac_supp_01,# Initial fraction of adopters in support
               pbc_mid=pbc_mid1,                  # Midpoint for perceived behavioral control (PBC)
               pbc_steep=pbc_steep1,              # Steepness of the PBC function
               pbc_opinionchange=pbc_opinionchange1, # Effect of opinion change on PBC
               pbc_0=pbc_01,                      # Initial PBC value
               etc_mid=etc_mid1,                  # Midpoint of emissions technology change (ETC)
               etc_total=etc_total1,              # Total potential for ETC improvement
               etc_steep=etc_steep1,              # Steepness of the ETC adoption curve
               normeffect=normeffect1,            # Effect of social norms on policy adoption
               bau=bau1,                          # Business-as-usual (BAU) emissions scenario
               bau_outside_region=bau_outside1,   # BAU emissions outside the modeled region
               ex_forcing=ex_forcing1,            # External forcing (e.g., solar variability, volcanism)
               m_max=m_max1,                      # Maximum potential mitigation effort
               r_max=r_max1,                      # Maximum emissions reduction rate
               r_0=r_01,                          # Initial emissions reduction rate
               adopt_effect=adopt_effect1,        # Effect of technology adoption on mitigation
               evidenceeffect=evidenceeffect1,    # Effect of scientific evidence on opinions
               biassedassimilation=biassedassimilation1, # Biased assimilation effect (confirmation bias)
               shiftingbaselines=shiftingbaselines1,     # Shifting baselines in risk perception
               year0=2020,                        # Starting year for the simulation
               natvar=NULL,                       # Optional natural variability data
               policyopinionfeedback_param=policyopinionfeedback_01, # Feedback from policy to opinions
               lbd_param=lbd_param01,             # Learning-by-doing effect on emissions reductions
               lag_param=lag_param01,             # Lag effect between temperature and emissions
               temp_emissionsparam=temp_emissionsparam01 # Feedback effect from temperature to emissions
               ){
  
  # Initialize the starting opinion distribution (opposed, neutral, supportive)
  startdist=c(frac_opp_0,frac_neut_0,1-(frac_opp_0+frac_neut_0))
  
  # Set up homophily parameters: likelihood of interaction within opinion groups
  params_opp=c(homophily_param,(1-homophily_param)/2,(1-homophily_param)/2)
  params_neut=c((1-homophily_param)/2,homophily_param,(1-homophily_param)/2)
  params_supp=c((1-homophily_param)/2,(1-homophily_param)/2,homophily_param)
  homophily=list(params_opp,params_neut,params_supp)
  
  # Apply external forcing on opinions (e.g., media campaigns, political influence)
  force_params=forcefunc(forcestrong,forceweak,forcestrong)
  
  # Create an empty matrix to store opinion distributions over time
  distributions=matrix(nrow=length(time),ncol=3)
  distributions[1,]=startdist # Set initial opinion distribution
  
  # Initialize policy vector over time
  policy=numeric(length=length(time))
  policy[1]=policy_0 # Set initial policy value
  
  # Initialize matrix for tracking adopters' fraction (opposed, neutral, supportive)
  adoptersfrac=matrix(nrow=length(time),ncol=3)
  adoptersfrac[1,]=c(adoptfrac_opp_0,adoptfrac_neut_0,adoptfrac_supp_0) # Initial adopters' distribution
  
  # Initialize the number of adopters over time
  nadopters=numeric(length=length(time))
  nadopters[1]=distributions[1,]%*%adoptersfrac[1,] # Initial number of adopters
  
  # Initialize perceived behavioral control (PBC) over time
  pbc=numeric(length=length(time))
  pbc[1]=pbc_0 # Set initial PBC value
  
  # Initialize emissions over time (starting with BAU emissions)
  emissions=numeric(length=length(time))
  emissions[1]=bau[1]*(1+(temp_emissionsparam*temp_0[1])) # Emissions affected by initial temperature
  
  # Initialize total emissions, including emissions outside the region
  totalemissions=numeric(length=length(time))
  totalemissions[1]=(bau[1]+bau_outside_region[1])*(1+(temp_emissionsparam*temp_0[1]))
  
  # Initialize mitigation matrix (all zeros to start)
  mitigation=matrix(0,nrow=length(time),ncol=length(time)) # Tracks mitigation efforts over time
  
  # Initialize temperature and climate-related mass (atmospheric, ocean, and other) over time
  temperature=matrix(nrow=length(time),ncol=2)
  temperature[1,]=temp_0 # Set initial temperature
  
  mass=matrix(nrow=length(time),ncol=3)
  mass[1,]=mass_0 # Set initial mass distribution in climate components
  
  # Initialize business-as-usual (BAU) temperature and mass (used for comparison)
  bau_temp=matrix(nrow=length(time),ncol=2)
  bau_temp[1,]=temp_0 # BAU initial temperature
  
  bau_mass=matrix(nrow=length(time),ncol=3)
  bau_mass[1,]=mass_0 # BAU initial mass
  
  # Generate natural variability (random climate noise) if not provided
  if(is.null(natvar)) naturalvariability=Re(randomts(gtemp))[1:length(time)]*8
  if(!is.null(natvar)) naturalvariability=natvar
  
  # Initialize weather (temperature + natural variability)
  weather=numeric(length=length(time))
  weather[1]=temperature[1,1]+naturalvariability[1]
  
  # Initialize matrix to store evidence for opinion updates
  evidence=matrix(nrow=length(time),ncol=3)
  evidence[1,]=rep(0,3) # No evidence at the start
  
  # Initialize anomaly detection (used for shifting baselines)
  anomaly=numeric(length=length(time))
  anomaly[1]=ifelse(shiftingbaselines==0,weath  er[1],naturalvariability[1])
  
  # Loop over time steps to update model state each year
  for(t in 2:length(time)){
    # Update opinion distributions based on homophily, external force, and policy feedback
    distributions[t,]=opinionchange(distributions[t-1,],evidence[t-1,],evidence_effect=evidenceeffect,selfsimparams=homophily,force=force_params,policychange_t_1=ifelse(t==2,0,policy[t-1]-policy[t-2]),policyopinionfeedback=policyopinionfeedback_param,adopt_t_1=adoptersfrac[t-1,],ced=ced_param)
    
    # Update policy based on opinion distribution and previous policy changes
    policy[t]=policychange(distributions[t,],policy[t-1],ifelse(t>pol_window,mean(policy[(t-pol_window):(t-1)]),mean(policy[1:t-1])),pol_feedback,pol_response,policy_pbcchange_max)
    
    # Update adopter fractions based on opinion distribution and perceived behavioral control (PBC)
    adoptersfrac[t,]=adoptfracchange(distributions[t,],adoptersfrac[t-1,],pbc[t-1],pbc_opinionchange)
    
    # Update total number of adopters
    nadopters[t]=distributions[t,]%*%adoptersfrac[t,]
    
    # Update PBC based on opinion and policy changes
    pbc[t]=pbcchange(pbc[t-1],distributions[t,],policy[t],pbc_steep,pbc_mid)
    
    # Update emissions based on policy, technology adoption, and mitigation
    emissions[t]=emissionschange(emissions[t-1],bau[t],adoptersfrac[t,],policy[t],adopt_effect,etc_mid,etc_total,etc_steep,nadopters[t],m_max,m_assumption=0)
    
    # Update total emissions including outside region emissions
    totalemissions[t]=emissions[t]+bau_outside_region[t]
    
    # Update mitigation effects based on emissions reduction
    mitigation[t,]=mitigationchange(mitigation[t-1,],emissions[t],r_max,policy[t])
    
    # Update temperature and climate mass based on emissions and external forcing
    temperature[t,]=temperaturechange(temperature[t-1,],mass[t-1,],emissions[t],natvar[t],ex_forcing[t],mass[t-1,],time)
    mass[t,]=masschange(mass[t-1,],emissions[t],r_max,temperature[t,],weather[t])
    
    # Update BAU temperature and mass (for comparison)
    bau_temp[t,]=bau_temperaturechange(bau_temp[t-1,],bau_mass[t-1,],bau_outside_region[t])
    bau_mass[t,]=bau_masschange(bau_mass[t-1,],bau_outside_region[t],bau_temp[t,],time)
    
    # Update natural variability (random climate noise) and weather
    weather[t]=temperature[t,1]+naturalvariability[t]
    
    # Update evidence for opinion change based on policy and climate anomalies
    evidence[t,]=evidencechange(distributions[t,],policy[t],anomaly[t-1],anomaly[t-1],weather[t],biassedassimilation)
    
    # Detect anomaly based on shifting baselines (risk perception adjustments)
    anomaly[t]=ifelse(shiftingbaselines==0,weather[t],anomalychange(anomaly[t-1],weather[t],shiftingbaselines))
  }
  
  # Output all model results in a list
  list(
    distributions=distributions,       # Opinion distributions over time (proportion of opposition, neutral, and support for policy)
    policy=policy,                     # Policy values over time (reflecting the degree of climate action taken)
    adoptersfrac=adoptersfrac,         # Fraction of technology adopters within each opinion group (opposed, neutral, supportive) over time
    nadopters=nadopters,               # Total number of adopters over time (sum of fractions from each group)
    pbc=pbc,                           # Perceived behavioral control (PBC) over time (captures public's perceived ability to take action)
    emissions=emissions,               # Emissions over time for the modeled region (accounting for policy and technology adoption effects)
    totalemissions=totalemissions,     # Total emissions over time (including both the modeled region and emissions outside the region)
    mitigation=mitigation,             # Mitigation efforts over time (tracks how emissions are reduced due to policy and technology)
    temperature=temperature,           # Temperature over time (based on emissions and natural variability; contains two columns: surface and deep ocean temperature)
    mass=mass,                         # Climate-related mass over time (distribution of carbon in different parts of the climate system like atmosphere, ocean, etc.)
    weather=weather,                   # Weather (temperature + natural variability) over time (used to assess short-term climate anomalies)
    evidence=evidence,                 # Evidence for opinion updates over time (how much new information or anomalies influence public opinion)
    anomaly=anomaly,                   # Climate anomaly over time (used to detect shifts in public perception based on changing baselines)
    bau_temp=bau_temp,                 # Business-as-usual (BAU) temperature over time (for comparison with mitigation scenarios)
    bau_mass=bau_mass                  # Business-as-usual (BAU) mass distribution over time (for comparison with mitigation scenarios)
  )

}
