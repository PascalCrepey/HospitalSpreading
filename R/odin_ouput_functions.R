#' Rearrange odin output matrix into a matrix of transfers of infected individuals
#'
#' @param sim Raw odin output matrix
#'
#' @return Square matrix of transfers of infected individuals
getTransitionMatrix = function(sim, n_subpop) {
  sim_list = sim[,grepl("^t_I\\[[0-9,]+\\]$", colnames(sim))]
  sim_list = asplit(sim_list, 1)
  out = lapply(sim_list, matrix, ncol = n_subpop, nrow = n_subpop)
  return(simplify2array(out))
}
