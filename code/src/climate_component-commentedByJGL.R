# DICE 2007 climate model, annualized using parameters from Cai and Lontzek 2019
# This model simulates the global temperature response to CO2 emissions.
# Initial values for 2020 are calculated by running the model from 2005-2020 using observed emissions data for 2005-2019.

source("src/utility_scripts/initialize climate model.R") # Load initialization script for the climate model

# Initial carbon masses in the atmosphere, upper ocean, and lower ocean in 2020 (GtC).
# These values come from running the model between 2005-2020.
mass_0 = init_mass[16,] 

# Initial temperature values for 2020 relative to 1900.
# 1.08°C for atmospheric temperature and 0.13°C for lower ocean temperature.
temp_0 = init_temperature[16,] 

# Climate sensitivity parameters.
psi1 = 0.037  # Sensitivity of the atmosphere to radiative forcing (W/m^2).
nu = 3.8      # Total radiative forcing from a doubling of atmospheric CO2 (W/m^2).

# Carbon cycle transition matrix. This governs how carbon moves between the atmosphere, upper ocean, and lower ocean.
# Parameters based on Cai, Judd, and Lontzek (2019).
phi_carbon = matrix(c(1-0.0189288, 0.0097213, 0,      # Atmosphere to atmosphere, atmosphere to upper ocean.
                      0.0189288, 1-0.0147213, 0.0003119,  # Upper ocean to atmosphere, upper ocean to upper ocean, upper ocean to lower ocean.
                      0, 0.005, 1-0.0003119),       # Lower ocean to upper ocean, lower ocean to lower ocean.
                    byrow=TRUE, nrow=3)

# Radiative forcing function, which calculates how much the atmosphere is heated by CO2 and other factors.
# 'm_at_t' is the atmospheric carbon concentration at time t.
# 'ex_forcing_t' is the exogenous radiative forcing (i.e., from non-CO2 sources like methane).
forcing = function(m_at_t, ex_forcing_t, m_at_0 = 596.4, nu_param = nu) {
  forcing_t = nu_param * log2(m_at_t / m_at_0) + ex_forcing_t  # Forcing increases with log of CO2 concentration.
  return(forcing_t)  # Return total radiative forcing.
}

# Temperature transition matrix for the atmosphere and lower ocean.
# Governs how temperature changes in the atmosphere and ocean over time.
# Parameters are annualized from DICE2016 using the formula from Cai, Judd, and Lontzek.
phi_temp = matrix(c(1-(0.01+0.047), 0.01,  # Atmosphere to atmosphere, atmosphere to lower ocean.
                    0.0048, (1-0.0048)),   # Lower ocean to atmosphere, lower ocean to lower ocean.
                  byrow=TRUE, nrow=2)

# Function to calculate temperature changes based on past emissions and temperature.
# Inputs:
#   - temp_t_1: Temperature at time t-1 (previous time step).
#   - mass_t_1: Carbon mass in different reservoirs (atmosphere, upper ocean, lower ocean) at time t-1.
#   - emissions_t: CO2 emissions at time t.
#   - ex_forcing_t: Exogenous radiative forcing (from non-CO2 gases) at time t.
#   - bau_tot_t: Business-as-usual (BAU) total emissions at time t.
temperaturechange = function(temp_t_1, mass_t_1, emissions_t, ex_forcing_t, bau_tot_t, 
                             psi1_param = psi1, nu_param = nu, mass_0_param = mass_0,
                             phi_carbon_param = phi_carbon, phi_temp_param = phi_temp) {
  
  # Scale exogenous forcing based on the percentage reduction in emissions relative to BAU.
  # Assume maximum 50% effectiveness for reducing non-CO2 gases.
  red = (bau_tot_t - emissions_t) / (bau_tot_t)  # Reduction in emissions relative to BAU.
  red = red * 0.49  # Apply a 49% cap on the reduction's effectiveness.
  
  # Adjust exogenous forcing based on emissions reductions.
  ex_forcing_new = ex_forcing_t * (1 - red) 
  
  # Update carbon mass in atmosphere, upper ocean, and lower ocean.
  # Carbon transition is governed by the phi_carbon matrix.
  mass_t = phi_carbon_param %*% mass_t_1 + c(emissions_t, 0, 0)  # Add new emissions to atmosphere; no direct emissions to oceans.
  
  # Calculate radiative forcing based on new atmospheric carbon mass and adjusted exogenous forcing.
  forcing_t = forcing(mass_t[1], ex_forcing_new, m_at_0 = 596.4, nu_param = nu)
  
  # Update temperature in the atmosphere and lower ocean.
  # The temperature transition is governed by the phi_temp matrix, with additional forcing applied to the atmosphere.
  temp_t = phi_temp %*% temp_t_1 + c(psi1_param * forcing_t, 0)  # Forcing affects atmospheric temperature; ocean temperature is lagged.
  
  return(list(mass_t, temp_t))  # Return updated carbon mass and temperature.
}
