# Function to model changes in opinions on climate change, given the current distribution of opinions,
# social network interactions, climate information, external forces, policy feedback, and credibility-enhancing display (CED) effects.
opinionchange=function(distribution=c(frac_opp_0,frac_neut_0,frac_supp_0), # Initial distribution of opinions (opposing, neutral, supporting)
                       evidence_t_1, # Climate evidence available at the previous time step
                       evidence_effect=evidenceeffect, # Strength of the effect of evidence on opinion change
                       selfsimparams=homophily, # Homophily parameters to model interactions within and across groups
                       force=force_params, # External forces affecting opinions (e.g., media or policy)
                       policychange_t_1=0, # Policy change at the previous time step, affecting opinion change
                       policyopinionfeedback=policyopinionfeedback_param, # Strength of the feedback effect of policy on opinions
                       adopt_t_1, # Adoption rates for the previous time step (for each group)
                       ced=ced_param){ # CED (credibility-enhancing display) parameter, affects how influential supporters are

  # Calculate the social network matrix based on current opinion distribution and homophily parameters
  network=networkfunc(distribution,selfsimparams) 

  # Adjust the persuasive force of climate policy supporters based on the difference in adoption rates (CED effect)
  force[1,3]=force[1,3]+(ced*(adopt_t_1[3]-adopt_t_1[1])) # Supporters' influence on opponents increases with higher adoption by supporters
  force[2,3]=force[2,3]+(ced*(adopt_t_1[3]-adopt_t_1[2])) # Supporters' influence on neutrals increases with higher adoption by supporters

  # The combined force felt by each group is the product of the social network interactions and external forces
  feltforce=network*force 

  # Calculate the effect of policy change on opinion, if policyopinionfeedback is non-zero
  policy_effect=policyopinionfeedback*policychange_t_1 # Feedback from policy to opinions (norm signaling)

  # Calculate the force for opponents to become neutral (opinion shift), considering felt forces, evidence, and policy effects
  force_opp_neut=min(max(sum(feltforce[1,2:3])+evidence_effect*evidence_t_1[1]+policy_effect,0),1)

  # Calculate the force for neutrals to become opponents, considering felt forces, evidence, and policy effects
  force_neut_opp=min(max(feltforce[2,1]-evidence_effect*evidence_t_1[2]-policy_effect,0),1)

  # Calculate the force for neutrals to become supporters, considering felt forces, evidence, and policy effects
  force_neut_supp=min(max(feltforce[2,3]+evidence_effect*evidence_t_1[2]+policy_effect,0),1)

  # Calculate the force for supporters to become neutral, considering felt forces, evidence, and policy effects
  force_supp_neut=min(max(sum(feltforce[3,1:2])-evidence_effect*evidence_t_1[3]-policy_effect,0),1)

  # Transition probabilities matrix: probability of moving between opinion groups (opponent <-> neutral <-> supporter)
  transitionprobs=matrix(c(1-force_opp_neut,force_opp_neut,0, # Opponent to neutral probability
                           force_neut_opp,1-(force_neut_supp+force_neut_opp),force_neut_supp, # Neutral to opponent/supporter probabilities
                           0,force_supp_neut,1-force_supp_neut),byrow=T,nrow=3) # Supporter to neutral probability

  # Return the updated distribution of opinions based on transition probabilities
  return(distribution%*%transitionprobs)
}
