library(data.table)
library(SimInf)
library(magrittr)
library(ggplot2)


getSimInfModel <- function() {
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
  gdata = c(beta = 0.16, gamma = 0.077, mu0 = 0, mu1 = 0)
  tspan_max = 30
  tspan = 1:tspan_max

  ########################
  ## Add sub population ##
  ########################
  npop <- 300

  starting_pop = rep(100, npop)
  index_pop = c(1, rep(0, npop - 1))

  u0 = data.frame(S = starting_pop - index_pop
                  , I = index_pop)


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

  A= diag(npop)
  reorderIndex = sample(nrow(A))
  if(any(reorderIndex == 1:npop)){
    print("running")
    reorderIndex = sample(nrow(A))

  }
  trans_mat = A[reorderIndex, ]


  events <- make_siminf_events(times = tspan, nmetapop = npop, transfer_matrix = trans_mat
                               , select = 1, shift = 0)

  events$event


  model <- mparse(
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





benchmark_siminf <- function(model) {
  result <- run(model)

  results <- microbenchmark(
    SimInfResult = run(model),
    times = 10)

  return(results)
}
