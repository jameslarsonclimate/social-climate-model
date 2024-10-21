emissionschange = function(bau_t, nadopters_t, policy_t, mitigation, t, temperature_t_1, temperature_t_lag, 
                           effectiveness = adopt_effect, maxm = m_max, rmax = r_max, r0 = r_0, lbd = lbd_param, 
                           emissions_t_lag, bau_t_lag, bau_outisde_t, lag = lag_param, temp_emissions = temp_emissionsparam) {
  # This function calculates emissions changes over time as influenced by policy, mitigation, 
  # individual adoption of green behaviors, and temperature feedback effects.
  
  # 'bau_t' is business-as-usual emissions at time t.
  # 'nadopters_t' is the number of people adopting emissions-reducing actions at time t.
  # 'policy_t' is the climate policy level at time t.
  # 'mitigation' is a matrix tracking mitigation efforts over time.
  # 'temperature_t_1' is the temperature anomaly at time t-1.
  # 'effectiveness' is the effectiveness of individual adoption actions.
  # 'maxm' is the maximum mitigation capacity.
  # 'rmax' and 'r0' control the lifetime of mitigation investments.
  # 'lbd' is the learning-by-doing parameter.
  # 'emissions_t_lag' tracks past emissions.
  # 'bau_outisde_t' refers to business-as-usual emissions outside the target region.
  # 'lag' governs how emissions outside the region respond to temperature and policy changes with a delay.
  # 'temp_emissions' captures the feedback effect of temperature on emissions.

  # contemporaneous reduction from policy, depends on policy
  # mitigation_t_1 is a matrix with dimensions of max(t) * t-1 that gives persistent effect of mitigation actions in previous time periods.
  # lbd param is a "learning by doing" parameter, indicating the cost reductions from cumulative mitigation actions.

  # Calculate the cumulative mitigation from the past (previous time periods).
  cummit_t_1 = sum(mitigation[t-1, 1:(t-1)])  # cumulative policy-induced mitigation in previous time periods
  
  # Calculate the number of doublings of cumulative mitigation relative to the maximum initial value.
  # This reflects how many times the mitigation stock has doubled due to previous efforts, affecting cost reductions via the learning-by-doing effect.
  doublings = log2(cummit_t_1 / maxm)

  # Calculate the maximum possible mitigation (mmax_t) for the current time, accounting for learning-by-doing.
  # If doublings are less than or equal to 1, we use the initial max mitigation. If greater than 1, we adjust by the learning-by-doing parameter (lbd).
  mmax_t = ifelse(doublings <= 1, maxm, maxm * (1 + lbd)^doublings)

  # Calculate the mitigation for the current time (m_t) based on the current policy level.
  # If the policy is low (policy_t <= 1), no mitigation occurs. At maximum policy (policy_t >= 299), maximum mitigation (mmax_t) is applied.
  # For intermediate policy values, mitigation is a scaled proportion of the maximum (using a logarithmic relationship).
  m_t = ifelse(policy_t <= 1, 0, ifelse(policy_t >= 299, mmax_t, mmax_t * (log(policy_t) / log(300))))  # 300 is the maximum policy value.

  # Lifetime of mitigation investments (r_t) depends on the policy level.
  # As policy increases, the longevity (lifetime) of investments also increases, up to a maximum (rmax).
  r_t = min(r0 * (1 + policy_t / 10), rmax)

  # Fill in the mitigation matrix with the mitigation for the current time and its persistent effect on future periods.
  # The future effect decays exponentially based on the lifetime of the mitigation (r_t).
  futuretime = t:dim(mitigation)[1] - t
  mitigation[, t] = ifelse(rep(m_t == 0, dim(mitigation)[1]), rep(0, dim(mitigation)[1]), 
                            c(rep(0, t-1), m_t * exp(-futuretime / r_t)))

  # Calculate the emissions reduction due to policy by subtracting the cumulative effect of mitigation from business-as-usual emissions.
  emissions_policy_t = bau_t * max((1 - sum(mitigation[t, ])), 0)

  # Calculate emissions reduction from individual actions (adopters), which are non-cumulative but additional to the policy effect.
  # The total emissions are reduced by a factor proportional to the number of adopters and their effectiveness.
  emissions_t = emissions_policy_t * (1 - nadopters_t * effectiveness)

  # Apply temperature feedback: higher temperatures increase emissions via a feedback loop (temp_emissions), amplifying the emissions rate.
  emissions_t = emissions_t * (1 + (temp_emissions * temperature_t_1))

  # Calculate total emissions, considering emissions in outside regions (e.g., non-target regions like OECD).
  # These outside regions follow mitigation pathways with a lag. If the lag is 0, inside and outside regions follow the same path.
  # If lag > 0, outside regions are affected by a delayed version of the inside region's policy and emissions-temperature feedback.
  total_emissions_t = ifelse(lag == 0, 
                             emissions_t + bau_outisde_t * (1 - (bau_t - emissions_t) / bau_t),  # Same region behavior if lag == 0
                             emissions_t + bau_outisde_t * (1 - (bau_t_lag - emissions_t_lag) / bau_t_lag) * 
                             (1 - temp_emissions * (temperature_t_1 - temperature_t_lag)))

  # Return emissions for the current time (emissions_t), updated mitigation matrix, and total emissions (including the outside region).
  return(list(emissions_t, mitigation, total_emissions_t))
}
