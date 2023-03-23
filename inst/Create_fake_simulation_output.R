######################################
# Code to create a fake simulation
# output
######################################

rm(list = ls())
library(HospitalSpreading)

# Parameters
n_steps_per_time_unit = 1                                 # time step
t_max = 20                                                # final time step
beta = 0.5                                                # transmission rate
alpha = 0.1                                               # recovery rate
npop = 5                                                  # number of hospitals
pop_size = 150                                            # starting population size of each hospital
starting_pop = rep(pop_size, npop)                        # vector of starting population sizes
index_size = 10                                           # number of infected individuals in hospital 1
index_pop = c(index_size, rep(0, npop - 1))               # vector of starting infected populations
trans_mat = make_fake_matrix(nmetapop = npop, scale = 20) # Transfer matrix

# Initialize model
initialized_model <- initialize_sis(
  beta = beta,
  alpha = alpha,
  transfer_matrix = trans_mat,
  size_subpop = starting_pop,
  I_initial_time = index_pop,
  community_prev = 0,
  n_steps_per_time_unit = n_steps_per_time_unit
  )

# Run Nsims simulations
Nsims = 2
set.seed(20230317)
model_output_example = run_simulation(initialized_model, t_max, n_steps_per_time_unit, Nsims)
model_output_example
save(model_output_example, file = "inst/model_output_example.RData")


