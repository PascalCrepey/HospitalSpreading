########### Simple stochastic SI with odin - binomial approximation ##########

#' Initialize the Odin model
#'
#' @param beta the probability of infection
#' @param n_subpop number of subpopulation
#' @param size_subpop the size of the subpopulations
#' @param transfer_matrix the transfer matrix
#' @param I_per_subpop the initially infected in each subpopulation
#'
#' @return an Odin model
#' @export
#' @importFrom odin odin
#'
initialize_odin_binomial <- function(
    beta,
    n_subpop,
    size_subpop,
    transfer_matrix,
    I_per_subpop,
    time_step
  )
{

  model = odin_stoch_model_si_binom_fixing$new(
    beta = beta,
    s_initial = size_subpop - I_per_subpop,
    i_initial = I_per_subpop,
    n_hospitals = n_subpop,
    d = transfer_matrix,
    time_step = 1/time_step
    )
  return(model)

}


########### Simple stochastic SI with odin - poisson approximation ##########

#' Initialize the Odin model
#'
#' @param beta the probability of infection
#' @param n_subpop number of subpopulation
#' @param size_subpop the size of the subpopulations
#' @param transfer_matrix the transfer matrix
#' @param I_per_subpop the number of infecetd in each subpopulation
#'
#' @return an Odin model
#' @export
#'
initialize_odin_poisson <- function(
    beta,
    n_subpop,
    size_subpop,
    transfer_matrix,
    I_per_subpop
)
{

  model = odin_stoch_model_si_poisson_fixing$new(
    beta = beta,
    s_initial = size_subpop - I_per_subpop,
    i_initial = I_per_subpop,
    n_hospitals = n_subpop,
    d = transfer_matrix
    )
  return(model)

}

########### Simple stochastic SI with odin - binomial approximation ##########

#' Initialize the Odin model
#'
#' @param beta the probability of infection
#' @param alpha the recovery rate
#' @param size_subpop the size of the subpopulations
#' @param transfer_matrix the transfer matrix
#' @param I_initial_time the initially infected in each subpopulation or the total number
#' @param community_prev the community prevalence
#' @param initial_state_rando_proc the procedure of initial state randomisation if
#' I_initial_time has length 1: "none" is the default, "uniform", "largest_pop", "most_connected_pop"
#'
#' @return an Odin model
#' @export
#' @importFrom odin odin
#'
initialize_sis <- function(beta,
                           alpha,
                           size_subpop,
                           transfer_matrix,
                           I_initial_time,
                           community_prev,
                           initial_state_rando_proc = "none",
                           n_steps_per_time_unit = 1
)
{

  # Get initial conditions
  initial_state = initial_state_randomisation(size_subpop, I_initial_time, initial_state_rando_proc, transfer_matrix)

  # Transfers from and within the community
  total_inflow = colSums(transfer_matrix)
  total_outflow = rowSums(transfer_matrix)
  com_entrance = total_inflow-total_outflow
  com_entrance[com_entrance < 0] = 0
  com_exit = total_outflow - total_inflow
  com_exit[com_exit < 0] = 0

  transfer_matrix_full = rbind(transfer_matrix, com_exit)
  transfer_matrix_full = cbind(transfer_matrix_full, c(com_entrance, 0))
  rownames(transfer_matrix_full) = NULL

  # Create odin model
  model = odin_stoch_model_sis_binom$new(
    beta = beta/n_steps_per_time_unit,
    alpha = alpha/n_steps_per_time_unit,
    s_initial = initial_state$size_subpop - initial_state$I_per_subpop,
    i_initial = initial_state$I_per_subpop,
    n_subpop = length(size_subpop),
    N = size_subpop,
    d = transfer_matrix_full,
    com_p = community_prev,
    time_step = n_steps_per_time_unit
    )
  return(model)

}


