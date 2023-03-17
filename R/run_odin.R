#' Run initialized odin model and return list object
#'
#' @param initialized_model Initialized odin model
#' @param t_max Last time step
#' @param time_step Duration of the time step
#' @param replicate Number of simulations
#'
#' @return List containing
#' 1. the prevalence and incidence per timestep, subpopulation, and simulation
#' 2. the incidence per timestep, subpopulation, and simulation
#' 3. the number of transferred infected individuals between every pair of subpopulation per timestep and simulation
#' 4. the transfer matrix
#' 5. the vector of subpopulation sizes
#' @export
run_simulation = function(initialized_model, t_max, time_step, replicate = 1) {

  # Run simulations
  simulations <- initialized_model$run(1:(t_max/time_step), replicate = replicate)

  # Get array of prevalence
  #   - Dimension 1: Supopulations
  #   - Dimension 2: Timesteps
  #   - Dimension 3: Simulation number
  prevalence = simulations[,grepl("^I\\[[0-9]+\\]$", colnames(simulations)),]
  unname(prevalence)

  # Get array of incidence
  #   - Dimension 1: Supopulations
  #   - Dimension 2: Timesteps
  #   - Dimension 3: Simulation number
  incidence = simulations[,grepl("new_I\\[[0-9]+\\]$", colnames(simulations)),]
  unname(incidence)

  # Get list of arrays of transfers
  transfers_I = lapply(asplit(simulations,3), function(x)
    getTransitionMatrix(x, initialized_model$contents()$dim_N)
    )

  # Return output
  out = list(
   prevalence = prevalence,
   incidence = incidence,
   transfers_I = transfers_I,
   transfers_tot = initialized_model$contents()$d,
   subpop_size = initialized_model$contents()$N
  )
  return(out)
}
