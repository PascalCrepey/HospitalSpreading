######################################
# Code to create a fake simulation
# output
######################################

rm(list = ls())
library(HospitalSpreading)

# Parameters
time_step = 1                                             # time step (for odin)
t_max = 5                                                 # final time step
beta = 0.5                                                # transmission rate
npop = 3                                                  # number of hospitals
pop_size = 1000                                           # starting population size of each hospital
starting_pop = rep(pop_size, npop)                        # vector of starting population sizes
index_size = 10                                           # number of infected individuals in hospital 1
index_pop = c(index_size, rep(0, npop - 1))               # vector of starting infected populations
trans_mat = make_fake_matrix(nmetapop = npop, scale = 20)  # Transfer matrix

# Initialize model
initialized_model <- initialize_odin_binomial(
  beta = beta/time_step,
  n_subpop = npop,
  size_subpop = starting_pop,
  I_per_subpop = index_pop,
  transfer_matrix = trans_mat
  )

# Run Nsims simulations
Nsims = 2
set.seed(20230317)
model_output_example = run_simulation(initialized_model, t_max, time_step, Nsims)
model_output_example
save(model_output_example, file = "inst/model_output_example.RData")


