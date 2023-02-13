

#' Create the SimInf model
#'
#' @param tspan the time vector
#' @param npop the number of subpopulation
#' @param n_initial_infected the number of initially infected individuals
#' @param starting_pop the initial state
#' @param trans_mat the transfer matrix
#' @param beta the probability of infection
#'
#' @return a SimInf model
#' @export
#'
#' @importFrom SimInf mparse
getSimInfModel = function(tspan,
                           npop,
                           n_initial_infected,
                           starting_pop,
                           trans_mat, beta){
  ##################
  ## Compartments ##
  ##################
  compartments = c("S", "I")

  #################
  ## Transitions ##
  #################
  transitions = c(
    "@ -> mu0 -> S"
    , "S -> beta*S*I/(S+I) -> I"
    , "S -> mu1*S -> @"
    , "I -> mu1*I -> @")

  ########################
  ## General parameters ##
  ########################
  gdata = c(beta = beta, mu0 = 0, mu1 = 0)

  ########################
  ## Add sub population ##
  ########################


  u0 = data.frame(S = starting_pop - n_initial_infected,
                  I = n_initial_infected)

  E <- structure(.Data = c(1, 1),
                 .Dim = c(length(compartments),1),
                 .Dimnames = list(compartments,
                                  c("1"))
  )

  # N matrix for the shift process
  N <- matrix(rep(0, length(compartments)),
              nrow = length(compartments),
              ncol = 2,
              dimnames = list(compartments,
                              c("1", "2")))


  events <- make_siminf_events(times = tspan,
                               nmetapop = npop,
                               transfer_matrix = trans_mat,
                               select = 1, shift = 0)

  model <- SimInf::mparse(
    transitions = transitions,
    compartments = compartments,
    gdata = gdata,
    u0 = u0,
    tspan = tspan,
    E = E,
    N = N,
    events = events
  )

  return(model)

}





#' Benchmark a siminf model
#'
#' @param model
#'
#' @return the results of the benchmarck
#' @export
#'
benchmark_siminf <- function(model) {
  result <- run(model)

  results <- microbenchmark(
    SimInfResult = run(model),
    times = 10)

  return(results)
}
