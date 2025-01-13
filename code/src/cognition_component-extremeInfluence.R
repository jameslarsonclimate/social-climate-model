anomalyfunc = function(weather, t, biassed_assimilation = 0, shifting_baseline = 0, 
                       weights = c(0.23, 0.20, 0.17, 0.14, 0.11, 0.09, 0.06),
                       temp0 = temp_0[1]) {
  
  # If shifting_baseline is off, the anomaly is simply the current weather.
  if (shifting_baseline == 0) {
    anomaly = weather[t]
  }
  
  if (shifting_baseline == 1) {
    if (t == 2) {
      # If we're at t=2, no 20-year history is available yet. Continue using temp0 as before.
      anomaly = weather[t] - temp0
    } else {
      # Here we always use the last 20 years of data for mean/sd calculations.
      # If t < 21, you can handle that by using the available years only.
      window_size = min(t - 1, 20)  # Ensure we don't exceed available data
      historical_years = if (window_size < 20) {
        # Not enough data for full 20-year baseline; fallback to shorter window
        c(weather[(t - 1):max(1, t - window_size)])
      } else {
        # Exactly 20-year baseline
        c(weather[(t - 1):(t - 20)])
      }
      
      # Compute mean and sd from the last 20 years (or fewer if early in the run)
      h_mean = mean(historical_years)
      h_sd = sd(historical_years)
      
      # Determine the 2–8 year window for weighting as per original code
      if (t == 2) {
        # This case is actually handled above, but keeping for logic clarity
        anomaly = weather[t] - temp0
      } else if (2 < t & t < 9) {
        # For early t, combine a shorter window: (t-2):1 plus rep(temp0,(9-t))
        baseline_years = c(weather[(t - 2):1], rep(temp0, (9 - t)))
      } else if (t >= 9) {
        # For t >= 9, use the 7-year weighted window as in original code
        baseline_years = weather[(t - 2):(t - 8)]
      }
      
      # Adjust weights based on deviation from historical mean/sd
      adj_weights = weights
      for (i in seq_along(baseline_years)) {
        deviation = abs(baseline_years[i] - h_mean) / h_sd
        if (deviation > 1) {
          # Increase weight proportionally to how many std dev away the year is
          adj_weights[i] = adj_weights[i] * (1 + deviation)
        }
      }
      
      # Normalize adjusted weights to retain the same total sum of weights
      adj_weights = adj_weights / sum(adj_weights) * sum(weights)
      
      # Calculate anomaly with adjusted weights
      anomaly = weather[t] - adj_weights %*% baseline_years
    }
  }
  
  # Handle biased assimilation
  if (biassed_assimilation == 0) {
    evidence = rep(anomaly, 3)
  } else {
    evidence = ifelse(rep(anomaly < 0, 3),
                      c(anomaly * (1 + biassed_assimilation), anomaly, anomaly * (1 - biassed_assimilation)),
                      c(anomaly * (1 - biassed_assimilation), anomaly, anomaly * (1 + biassed_assimilation)))
  }
  # print(adj_weights)
  return(list(anomaly, evidence))
}
