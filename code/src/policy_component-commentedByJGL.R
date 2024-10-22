# Define the 'policychange' function, which models how collective policy changes based on public opinion
# (the distribution of opinions), past policy, and the responsiveness of the political system.
policychange = function(distribution_t, policy_t_1, pol_t_window, responsiveness = pol_response, feedback = pol_feedback) {
  
  # 'distribution_t': Current distribution of public opinions (opposed, neutral, support).
  # 'policy_t_1': Previous policy value at time t-1.
  # 'pol_t_window': Difference between past policy and a baseline, representing recent policy trends.
  # 'responsiveness': How quickly policy responds to public opinion (default: 'pol_response').
  # 'feedback': Amplification of responsiveness due to past policy effects (default: 'pol_feedback').

  # Calculate the 'response_feedback' term, which represents the influence of past policies on current political responsiveness.
  # The greater the past policy (pol_t_window), the stronger the political feedback loop, amplifying responsiveness.
  # This simulates how past policies can create political constituencies in favor of, or opposed to, further policy change.
  response_feedback = sqrt(abs(pol_t_window / 300)) * feedback

  # Adjust the responsiveness for pro-policy change (supporters). The idea is that if the policy has 
  # already moved in the direction of climate action (pol_t_window > 0), political resistance builds up.
  # The 'pro_responsiveness' decreases by 'response_feedback' to represent this resistance.
  pro_responsiveness = ifelse(pol_t_window > 0, 
                              max(responsiveness - response_feedback, 1),  # Lower responsiveness for further pro-policy change
                              max(responsiveness + response_feedback, 1))  # Higher responsiveness if past policies opposed climate action

  # Similarly, adjust the responsiveness for those opposing policy (opponents). If past policy has been moving 
  # in favor of climate action (pol_t_window > 0), political opposition increases, leading to higher 'opp_responsiveness'.
  opp_responsiveness = ifelse(pol_t_window > 0, 
                              max(responsiveness + response_feedback, 1),  # Higher responsiveness to policy opposition
                              max(responsiveness - response_feedback, 1))  # Lower responsiveness to policy opposition if past policies opposed action

  # Check whether more people oppose policy than support it, based on the first and third elements of 'distribution_t'.
  if (distribution_t[1] > distribution_t[3]) {
    
    # If opposition to policy exceeds support, check if the opposition majority is large enough 
    # relative to 'opp_responsiveness' to drive policy change.
    if (distribution_t[1] / distribution_t[3] < opp_responsiveness) change = 0  # Not enough opposition to drive policy change.
    
    # If the opposition majority is large enough, calculate a negative policy change based on the size of the opposition.
    # The change becomes stronger as the gap between opposition and support increases.
    if (distribution_t[1] / distribution_t[3] > opp_responsiveness) 
      change = -1 * (1 - distribution_t[2])^2 * ((distribution_t[1] / distribution_t[3]) - opp_responsiveness)^(1 + (1 / opp_responsiveness))
  }

  # Check whether more people support policy than oppose it (distribution_t[3] >= distribution_t[1]).
  if (distribution_t[3] >= distribution_t[1]) {
    
    # If support for policy exceeds opposition, check if the support majority is large enough 
    # relative to 'pro_responsiveness' to drive policy change.
    if (distribution_t[3] / distribution_t[1] < pro_responsiveness) change = 0  # Not enough support to drive policy change.
    
    # If the support majority is large enough, calculate a positive policy change based on the size of the support.
    # The change becomes stronger as the gap between support and opposition increases.
    if (distribution_t[3] / distribution_t[1] > pro_responsiveness) 
      change = (1 - distribution_t[2])^2 * ((distribution_t[3] / distribution_t[1]) - pro_responsiveness)^(1 + (1 / pro_responsiveness))
  }

  # The function returns the new policy value, constrained between -300 and 300 to prevent extreme values.
  # The new policy is the previous policy (policy_t_1) plus the calculated 'change'.
  return(max(min(policy_t_1 + change, 300), -300))
}
