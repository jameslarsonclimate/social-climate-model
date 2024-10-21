# Define the function 'anomalyfunc' which calculates perceived climate change (anomaly) 
# and resulting evidence based on weather, time step, cognitive biases, and shifting baseline effects.
# The default values are set for parameters related to biassed assimilation, shifting baseline, 
# weights for the shifting baseline calculation, and an initial temperature (temp0).

anomalyfunc = function(weather, t, biassed_assimilation = 0, shifting_baseline = 0, 
                       weights = c(0.23, 0.20, 0.17, 0.14, 0.11, 0.09, 0.06), temp0 = temp_0[1]) {
  
  # If shifting_baseline == 0, the anomaly is just the current weather at time 't'.
  # No shifting baseline effect is applied; the perception is based purely on current conditions.
  if (shifting_baseline == 0) anomaly = weather[t]
  
  # If shifting_baseline == 1, the anomaly is adjusted to reflect long-term cognitive biases.
  # The anomaly is based on a weighted combination of recent weather observations and a 
  # shifting baseline reference point. This captures the idea that people may "normalize" 
  # new climates by comparing recent conditions to past norms.
  if (shifting_baseline == 1) {
    
    # If t == 2, use the first known baseline temperature (temp0) as the reference.
    # The anomaly is the difference between the current weather and temp0.
    if (t == 2) anomaly = weather[t] - temp0
    
    # If t is between 2 and 9, calculate the anomaly based on a weighted sum of the weather
    # in the recent past and the reference temperature temp0. This captures a gradual shift 
    # in perception based on past weather conditions.
    if (2 < t & t < 9) {
      anomaly = weather[t] - weights %*% c(weather[(t - 2):1], rep(temp0, (9 - t)))
    }
    
    # If t >= 9, the anomaly is calculated as the difference between current weather and 
    # the weighted average of the past 7 time steps. This assumes people's perception of climate
    # is influenced by recent weather conditions more than long-term norms.
    if (t >= 9) {
      anomaly = weather[t] - weights %*% weather[(t - 2):(t - 8)]
    }
  }

  # Evidence represents how strongly the anomaly influences people's perceptions, and it can 
  # be modified by the 'biassed_assimilation' parameter, which reflects cognitive biases in 
  # interpreting the climate anomaly.
  
  # If biassed_assimilation == 0, evidence is unadjusted and directly reflects the anomaly.
  # The same value of anomaly is replicated 3 times.
  if (biassed_assimilation == 0) evidence = rep(anomaly, 3)
  
  # If biassed_assimilation > 0, the evidence is adjusted to reflect biased interpretations.
  # For negative anomalies (i.e., cooler-than-normal weather), the assimilation is reduced for some,
  # while for positive anomalies (i.e., hotter-than-normal), the assimilation is exaggerated.
  if (biassed_assimilation > 0) {
    evidence = ifelse(rep(anomaly < 0, 3), 
                      c(anomaly * (1 + biassed_assimilation), anomaly, anomaly * (1 - biassed_assimilation)), 
                      c(anomaly * (1 - biassed_assimilation), anomaly, anomaly * (1 + biassed_assimilation)))
  }

  # Return both the anomaly and the evidence as a list.
  return(list(anomaly, evidence))
}
