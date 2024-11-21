source("src/parameters.R")
source("src/opinions_component.R")
source("src/policy_component.R")
source("src/adoption_component.R")
source("src/emissions_component.R")
source("src/climate_component.R")
source("src/cognition_component.R")
load("../data/naturalvariability.Rdat")

randomts <- function(timeseries) {
  #returns a time series with the same spectral profile as the argument, but with randomly chosen phases
  if(length(timeseries) < 81) {
    # Run twice and concatenate
    ft1 <- fft(timeseries)
    N1 <- length(timeseries)
    rphase1 <- runif(N1, min=0, max=360)
    newft1 <- complex(real=abs(ft1)*cos(rphase1), imaginary=abs(ft1)*sin(rphase1))
    output1 <- fft(newft1, inverse=TRUE)/N1
    
    ft2 <- fft(timeseries)
    rphase2 <- runif(N1, min=0, max=360)
    newft2 <- complex(real=abs(ft2)*cos(rphase2), imaginary=abs(ft2)*sin(rphase2))
    output2 <- fft(newft2, inverse=TRUE)/N1
    
    output <- c(output1, output2)
  } else {
    # Run once
    ft <- fft(timeseries)
    N <- length(timeseries)
    rphase <- runif(N, min=0, max=360)
    newft <- complex(real=abs(ft)*cos(rphase), imaginary=abs(ft)*sin(rphase))
    output <- fft(newft, inverse=TRUE)/N
  }
  return(output)
}
model=function(time=1:81,
              num_regions=5, # New parameter for number of regions
              # Convert single values to lists of length num_regions
              homophily_param=homophily_param1,  # rep(list(homophily_param1), num_regions),
              frac_opp_0=rep(frac_opp_01, num_regions),
              frac_neut_0=rep(frac_neut_01, num_regions),
              forcestrong=forcestrong1,  # rep(forcestrong1, num_regions),
              forceweak=forceweak1,  # rep(forceweak1, num_regions),
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
              year0=2020,
              natvar=NULL,
              natvar_multiplier = natvar_multiplier1,
              historical=FALSE,
              temperature_input=NULL,
              temperature_delta=NULL,
              policyopinionfeedback_param=policyopinionfeedback_01,
              lbd_param=lbd_param01,
              lag_param=lag_param01,
              temp_emissionsparam=temp_emissionsparam01
              ){

  params_opp=c(homophily_param,(1-homophily_param)/2,(1-homophily_param)/2)
  params_neut=c((1-homophily_param)/2,homophily_param,(1-homophily_param)/2)
  params_supp=c((1-homophily_param)/2,(1-homophily_param)/2,homophily_param)
  homophily=list(params_opp,params_neut,params_supp)

  # Initialize data structures with regional dimension
  distributions = array(0, dim=c(length(time), 3, num_regions))
  policy = array(0, dim=c(length(time), num_regions))
  adoptersfrac = array(0, dim=c(length(time), 3, num_regions))
  nadopters = array(0, dim=c(length(time), num_regions))
  pbc = array(0, dim=c(length(time), num_regions))
  emissions = array(0, dim=c(length(time), num_regions))  
  mitigation = array(0, dim=c(length(time), length(time), num_regions))
  weather = array(0, dim=c(length(time), num_regions))
  evidence = array(0, dim=c(length(time), 3, num_regions))
  anomaly = array(0, dim=c(length(time), num_regions))
  naturalvariability = array(0, dim=c(length(time), num_regions))
  natvarERA5 = array(0, dim=c(74, num_regions))

  # Initialize temperature and mass arrays (shared across regions)
  temperature = matrix(nrow=length(time), ncol=2)
  mass = matrix(nrow=length(time), ncol=3)
  total_emissions = rep(0, length(time))
  total_emissions[1] = (sum(bau[1,]))*(1+(temp_emissionsparam*temp_0[1])) 

  if(is.null(natvar)) {
      for(r in 1:num_regions) {
        naturalvariability[,r]=Re(randomts(gtemp))[1:length(time)]*natvar_multiplier
      }
  }
  if(!is.null(natvar)) {
      for(r in 1:num_regions) {
        natvarERA5[,r] = read.csv("../data/naturalvariability_moreRegions.csv")[,r+1]
        naturalvariability[,r] = if(historical == TRUE) {
                                        c(natvarERA5[,r], Re(randomts(natvarERA5[,r]))[1:7])} else{
                                        Re(randomts(natvarERA5[,r]))[1:length(time)]
                                        }
      }
  }

  if(is.null(temperature_delta)) { # simple conversion from Null to zeros
   temperature_delta = array(0, dim=c(length(time), num_regions))
  }

  # Initialize first timestep
  for(r in 1:num_regions) {
    distributions[1,,r] = c(frac_opp_0[r], frac_neut_0[r], 1-(frac_opp_0[r]+frac_neut_0[r]))
    policy[1,r] = policy_0
    adoptersfrac[1,,r] = c(adoptfrac_opp_0, adoptfrac_neut_0, adoptfrac_supp_0)
    nadopters[1,r] = distributions[1,,r] %*% adoptersfrac[1,,r]
    pbc[1,r] = pbc_0
    emissions[1,r] = bau[1,r] * (1+(temp_emissionsparam*temp_0[1]))
  }

  # Initialize shared variables
  temperature[1,] = temp_0
  mass[1,] = mass_0
  
  # Main temporal loop
  for(t in 2:length(time)) {
    # print(t)
    
    # Regional loop
    for(r in 1:num_regions) {
      # Update region-specific variables
      distributions[t,,r] = opinionchange(
        distributions[t-1,,r],
        evidence[t-1,,r],
        evidence_effect=evidenceeffect,
        selfsimparams=homophily,
        force=forcefunc(forcestrong, forceweak, forcestrong),
        policychange_t_1=ifelse(t==2, 0, policy[t-1,r]-policy[t-2,r]),
        policyopinionfeedback=policyopinionfeedback_param,
        adopt_t_1=adoptersfrac[t-1,,r],
        ced=ced_param
      )

      policy[t,r] = policychange(
        distributions[t,,r],
        policy[t-1,r],
        ifelse(t>pol_window, mean(policy[(t-pol_window):(t-1),r]), mean(policy[1:(t-1),r])),
        responsiveness=pol_response,
        feedback=pol_feedback
      )

      # Update adoption
      temp = adopterschange(
        nadopters[t-1,r],
        adoptersfrac[t-1,,r],
        policy[t-1,r],
        distributions[t,,r],
        etcmid=etc_mid,
        etcsteep=etc_steep,
        total=etc_total,
        init_pbc=pbc_0,
        maxpolpbc=policy_pbcchange_max,
        pbcmid=pbc_mid,
        pbcsteep=pbc_steep,
        shift=pbc_opinionchange,
        normstrength=normeffect,
        selfsimparam=homophily
      )
      pbc[t,r] = temp[[1]]
      nadopters[t,r] = temp[[2]]
      adoptersfrac[t,,r] = temp[[3]]

      temp2=emissionschange(
        bau[t,r],
        nadopters[t,r],
        policy[t,r],
        mitigation[,,r],
        t,
        temperature[t-1,1],
        temperature_t_lag=ifelse(t<=lag_param,temperature[t-1,1],temperature[t-lag_param,1]),
        effectiveness=adopt_effect,
        maxm=m_max,
        rmax=r_max,
        r0=r_0,
        lbd=lbd_param,
        emissions_t_lag=ifelse(t<=lag_param|lag_param==0,emissions[1,r],emissions[t-lag_param,r]),
        bau_t_lag=ifelse(t<=lag_param|lag_param==0,bau[1,r],bau[t-lag_param,r]),
        # bau_outside_t=bau_outside_region[t],  ## fix this and the above
        # emissions_outside_t=emissions_outside[t,],
        # I think i need to change the whole emissions file to get rid of references to bau_outside_region
        lag=lag_param,
        temp_emissions=temp_emissionsparam
        )
      emissions[t,r]=temp2[[1]]
      mitigation[,,r]=temp2[[2]]
      # totalemissions[t]=temp2[[3]]  ## fix this 

    }

    # Aggregate emissions across regions. Written by AI, likely incorrect, JGL
    # print(emissions[t,])
    # print(sum(emissions[t,]))
    total_emissions[t] = sum(emissions[t,])
    
    #climate model
    if(is.null(temperature_input[r])) {
      temp3 = temperaturechange(
        temperature[t-1,],
        mass[t-1,],
        total_emissions[t],
        ex_forcing[t],
        sum(bau[t,]), # this is a big change by AI, but it initially seems correct
        psi1_param=psi1,
        nu_param=nu
      )
      mass[t,] = temp3[[1]]
      temperature[t,] = temp3[[2]]
    }
    if(!is.null(temperature_input)) temperature=temperature_input
    
    # print(temperature[t,1])
    # print(naturalvariability[t,r])
    # print(temperature_delta[t,r])
    # Update regional weather and evidence
    for(r in 1:num_regions) {
      weather[t,r] = temperature[t,1] + naturalvariability[t,r] + temperature_delta[t,r]
      temp5 = anomalyfunc(weather[,r], t, biassedassimilation, shiftingbaselines)
      anomaly[t,r] = temp5[[1]]
      evidence[t,,r] = temp5[[2]]
    }
  }

  # Return results with regional dimension
  return(list(
    time=time,
    distributions=distributions,
    policy=policy,
    pbc=pbc,
    nadopters=nadopters,
    adoptersfrac=adoptersfrac,
    emissions=emissions,
    mitigation=mitigation,
    temperature=temperature,
    mass=mass,
    evidence=evidence,
    anomaly=anomaly,
    weather=weather,
    year=year0:(year0+length(time)-1),
    total_emissions=total_emissions
  ))
}



