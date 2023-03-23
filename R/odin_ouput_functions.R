#' Rearrange odin output matrix into a matrix of transfers of infected individuals
#'
#' @param sim Raw odin output matrix
#'
#' @return Square matrix of transfers of infected individuals
getTransitionMatrix = function(sim, n_subpop, timesteps_to_keep) {
  sim_list = sim[timesteps_to_keep,grepl("^t_I\\[[0-9,]+\\]$", colnames(sim))]
  sim_list = asplit(sim_list, 1)
  out = lapply(sim_list, function(x) matrix(x, ncol = n_subpop+1, nrow = n_subpop+1)[1:n_subpop, 1:n_subpop])
  return(simplify2array(out))
}
