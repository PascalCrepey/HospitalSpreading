#' Randomisation of the initial number of infected individuals
#'
#' @param size_subpop the size of the subpopulations
#' @param I_initial_time the initially infected in each subpopulation or the total number
#' @param initial_state_rando_proc the procedure of initial state randomisation if I_initial_time has length 1
#' @param transfer_matrix the transfer matrix
#'
#' @return List with the vector of the subpopulation sizes and the vector of the initial
#' number of infected individuals by subpopulation
initial_state_randomisation = function(size_subpop, I_initial_time, initial_state_rando_proc, transfer_matrix) {

  all_rando_proc = c("uniform", "largest_pop", "most_connected_pop")

  if (length(I_initial_time) == 1 & length(I_initial_time) < length(size_subpop)) {

    if (initial_state_rando_proc == "none" | !initial_state_rando_proc %in% all_rando_proc)
      stop("You have to choose the randomisation procedure of the number of infected individuals at time 0")

    if (initial_state_rando_proc == "uniform") return(rando_uniform(size_subpop, I_initial_time))
    if (initial_state_rando_proc == "largest_pop") return(rando_largest_pop(size_subpop, I_initial_time))
    if (initial_state_rando_proc == "most_connected_pop") return(rando_most_connected_pop(size_subpop, I_initial_time, transfer_matrix))

  } else if (length(size_subpop) == length(I_initial_time)) {
    return(list(size_subpop = size_subpop, I_per_subpop = I_initial_time))

  } else {
    stop("size_subpop and I_initial_time don't have the same length")

  }

}

#' Uniform randomisation procedure
#'
#' @param size_subpop the size of the subpopulations
#' @param I_initial_time the initially infected in each subpopulation or the total number
#'
#' @return List with the vector of the subpopulation sizes and the vector of the initial
#' number of infected individuals by subpopulation
rando_uniform = function(size_subpop, I_initial_time) {
  n_pop = length(size_subpop)
  I_per_subpop = as.vector(rmultinom(n = 1, size = I_initial_time, prob = rep.int(1 / n_pop, n_pop)))
  return(list(size_subpop = size_subpop, I_per_subpop = I_per_subpop))
}

#' Randomisation among the largest subpopulations
#'
#' @param size_subpop the size of the subpopulations
#' @param I_initial_time the initially infected in each subpopulation or the total number
#'
#' @return List with the vector of the subpopulation sizes and the vector of the initial
#' number of infected individuals by subpopulation
rando_largest_pop = function(size_subpop, I_initial_time){
  prob_subpop = size_subpop/ sum(size_subpop)
  I_per_subpop = as.vector(rmultinom(n = 1, size = I_initial_time, prob = prob_subpop))
  return(list(size_subpop = size_subpop, I_per_subpop = I_per_subpop))
}


#' Randomisation among the most connected subpopulations
#'
#' @param size_subpop the size of the subpopulations
#' @param I_initial_time the initially infected in each subpopulation or the total number
#' @param transfer_matrix the transfer matrix
#'
#' @return List with the vector of the subpopulation sizes and the vector of the initial
#' number of infected individuals by subpopulation
rando_most_connected_pop = function(size_subpop, I_initial_time, transfer_matrix) {
  totalFlow  = rowSums(transfer_matrix) + colSums(transfer_matrix)
  prob_subpop = totalFlow / sum(totalFlow)
  I_per_subpop = as.vector(rmultinom(n = 1, size = I_initial_time, prob = prob_subpop))
  return(list(size_subpop = size_subpop, I_per_subpop = I_per_subpop))
}
