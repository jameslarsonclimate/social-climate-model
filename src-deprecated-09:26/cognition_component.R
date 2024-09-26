# Function that computes the perceived climate anomaly based on realized weather
# and cognitive biases such as biased assimilation and shifting baselines.
# This function is used to model how individuals perceive climate change based
# on current weather patterns and how they assimilate past climate data.

# The shifting baseline concept is based on Moore et al. 2019, where individuals
# don't perceive climate change relative to a fixed point in time, but rather 
# relative to a shifting baseline of past weather. The weights parameter reflects 
# how much weight is given to previous years' weather data in this shifting baseline.

anomalyfunc=function(weather,t,biassed_assimilation=0,shifting_baseline=0,weights=c(0.23,0.20,0.17,0.14,0.11,0.09,0.06),weights_multiplier=1,temp0=temp_0[1]){
  # weather: a time series of weather data.
  # t: the current time step.
  # biassed_assimilation: degree to which bias influences how individuals 
  #                       perceive climate anomalies.
  # shifting_baseline: binary flag indicating whether to apply shifting baseline perception.
  # weights: a vector of weights used to implement shifting baseline perception.
  # temp0: reference baseline temperature, typically from a historical baseline.

  #weights=weights*weights_multiplier
  # If shifting_baseline is off (0), use the raw weather anomaly at time t.
  if(shifting_baseline==0) anomaly=weather[t]
  
  # If shifting_baseline is on (1), calculate the anomaly relative to past temperatures.
  if(shifting_baseline==1){
    
    # Special case for time t=2, the anomaly is the current weather minus temp0 (the baseline temp).
    if(t==2) anomaly=weather[t]-temp0
    
    # For t between 3 and 8, calculate the anomaly as the current weather minus a weighted
    # combination of past weather data. For early times (t < 9), use temp0 as a placeholder
    # for missing earlier data.
    if(2<t&t<9) anomaly=weather[t]-weights%*%c(weather[(t-2):1],rep(temp0,(9-t)))
    
    # For t >= 9, calculate the anomaly relative to the weighted average of the previous 7 time points.
    if(t>=9) anomaly=weather[t]-weights%*%weather[(t-2):(t-8)]
  }

  # Now handle cognitive biases with respect to how the anomaly is interpreted (biased assimilation).
  # This is in regards to how the three different populations interpre weather events based on how they view climate change through their politcal lens
  
  # If biassed_assimilation is 0, evidence is simply a vector that repeats the calculated anomaly 3 times.
  # This means no bias is applied to how the anomaly is perceived.
  if(biassed_assimilation==0) evidence=rep(anomaly,3)
  
  # If biassed_assimilation > 0, we apply a cognitive bias to how the anomaly is perceived.
  # Negative anomalies are perceived as more extreme, and positive anomalies as less extreme (or vice versa).
  # The ifelse structure splits the evidence based on whether the anomaly is negative or positive.
  if(biassed_assimilation > 0) 
    evidence = ifelse(rep(anomaly < 0, 3),
                      # For negative anomalies, increase the magnitude by the bias factor.
                      c(anomaly*(1 + biassed_assimilation), anomaly, anomaly*(1 - biassed_assimilation)),
                      
                      # For positive anomalies, reduce the magnitude by the bias factor.
                      c(anomaly*(1 - biassed_assimilation), anomaly, anomaly*(1 + biassed_assimilation)))
  
  # Return both the anomaly and the "evidence" (biased or unbiased perception) as a list.
  return(list(anomaly,evidence))
}