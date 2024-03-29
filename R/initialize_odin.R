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
initialize_odin_binomial <- function(beta,
                 n_subpop,
                 size_subpop,
                 transfer_matrix,
                 I_per_subpop
                 )
{

  # stoch_model_si <- odin::odin(
  #   {
  #
  #     #Total population in each hospital i
  #     N[] <- S[i] + I[i]
  #
  #     #number of patients moving that are S or I
  #     t_S[,] <- rbinom(d[i,j], S[i]/N[i])
  #     t_I[,] <- d[i,j] - t_S[i,j]
  #
  #     #Vectors of transfers out of hospitals
  #     t_S_out[] <- sum(t_S[i,])
  #     t_I_out[] <- sum(t_I[i,])
  #
  #     #Vectors of transfers into hospitals
  #     t_S_in[] <- sum(t_S[,i])
  #     t_I_in[] <- sum(t_I[,i])
  #
  #     #stochastic update
  #     new_I[] <- rbinom(S[i], 1-exp(-beta*I[i]/N[i]))
  #     update(S[]) <- S[i] - new_I[i] - t_S_out[i] + t_S_in[i]
  #     update(I[]) <- I[i] + new_I[i] - t_I_out[i] + t_I_in[i]
  #
  #     #Initial condition
  #     initial(S[]) <- s_initial[i]
  #     initial(I[]) <- i_initial[i]
  #
  #     #parameters
  #     beta <- user()
  #
  #     #initialize user input
  #     n_hospitals <- user()
  #     s_initial[] <- user()
  #     i_initial[] <- user()
  #     d[,] <- user()
  #
  #     #dimension of parameters
  #     # dim(beta) #one dimension because beta is hospital-indepedent
  #
  #     #dimension of compartments
  #     dim(N) <- n_hospitals
  #     dim(S) <- n_hospitals
  #     dim(I) <- n_hospitals
  #     dim(new_I) <- n_hospitals
  #     dim(s_initial) <- n_hospitals
  #     dim(i_initial) <- n_hospitals
  #
  #     dim(d) <- c(n_hospitals, n_hospitals)
  #     dim(t_S) <- c(n_hospitals, n_hospitals)
  #     dim(t_I) <- c(n_hospitals, n_hospitals)
  #
  #     dim(t_S_out) <- n_hospitals
  #     dim(t_I_out) <- n_hospitals
  #
  #     dim(t_S_in) <- n_hospitals
  #     dim(t_I_in) <- n_hospitals
  #
  #     # print variables at each time step
  #     output(t_S_out[]) <- TRUE
  #     output(t_I_out[]) <- TRUE
  #
  #     output(t_S_in[]) <- TRUE
  #     output(t_I_in[]) <- TRUE
  #
  #   }
  # )
  #run the model
  # model <- stoch_model_si$new(beta = beta,
  #                             s_initial = size_subpop - I_per_subpop,
  #                             i_initial = I_per_subpop,
  #                             n_hospitals = n_subpop,
  #                             d = transfer_matrix)

  model = odin_stoch_model_si_binom$new(beta = beta,
                                        s_initial = size_subpop - I_per_subpop,
                                        i_initial = I_per_subpop,
                                        n_hospitals = n_subpop,
                                        d = transfer_matrix)
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
initialize_odin_poisson <- function(beta,
                                     n_subpop,
                                     size_subpop,
                                     transfer_matrix,
                                     I_per_subpop
)
{

  # stoch_model_si <- odin::odin(
  #   {
  #
  #     #Total population in each hospital i
  #     N[] <- S[i] + I[i]
  #
  #     #number of patients moving that are S or I
  #     t_S[,] <- rbinom(d[i,j], S[i]/N[i])
  #     t_I[,] <- d[i,j] - t_S[i,j]
  #
  #     #Vectors of transfers out of hospitals
  #     t_S_out[] <- sum(t_S[i,])
  #     t_I_out[] <- sum(t_I[i,])
  #
  #     #Vectors of transfers into hospitals
  #     t_S_in[] <- sum(t_S[,i])
  #     t_I_in[] <- sum(t_I[,i])
  #
  #     #stochastic update
  #     new_I[] <- rpois(S[i] * (1-exp(-beta*I[i]/N[i])))
  #     update(S[]) <- S[i] - new_I[i] - t_S_out[i] + t_S_in[i]
  #     update(I[]) <- I[i] + new_I[i] - t_I_out[i] + t_I_in[i]
  #
  #     #Initial condition
  #     initial(S[]) <- s_initial[i]
  #     initial(I[]) <- i_initial[i]
  #
  #     #parameters
  #     beta <- user()
  #
  #     #initialize user input
  #     n_hospitals <- user()
  #     s_initial[] <- user()
  #     i_initial[] <- user()
  #     d[,] <- user()
  #
  #     #dimension of parameters
  #     # dim(beta) #one dimension because beta is hospital-indepedent
  #
  #     #dimension of compartments
  #     dim(N) <- n_hospitals
  #     dim(S) <- n_hospitals
  #     dim(I) <- n_hospitals
  #     dim(new_I) <- n_hospitals
  #     dim(s_initial) <- n_hospitals
  #     dim(i_initial) <- n_hospitals
  #
  #     dim(d) <- c(n_hospitals, n_hospitals)
  #     dim(t_S) <- c(n_hospitals, n_hospitals)
  #     dim(t_I) <- c(n_hospitals, n_hospitals)
  #
  #     dim(t_S_out) <- n_hospitals
  #     dim(t_I_out) <- n_hospitals
  #
  #     dim(t_S_in) <- n_hospitals
  #     dim(t_I_in) <- n_hospitals
  #
  #     # print variables at each time step
  #     output(t_S_out[]) <- TRUE
  #     output(t_I_out[]) <- TRUE
  #
  #     output(t_S_in[]) <- TRUE
  #     output(t_I_in[]) <- TRUE
  #
  #   }
  # )
  #run the model
  # model <- stoch_model_si$new(beta = beta,
  #                             s_initial = size_subpop - I_per_subpop,
  #                             i_initial = I_per_subpop,
  #                             n_hospitals = n_subpop,
  #                             d = transfer_matrix)
  model = odin_stoch_model_si_poisson$new(beta = beta,
                                        s_initial = size_subpop - I_per_subpop,
                                        i_initial = I_per_subpop,
                                        n_hospitals = n_subpop,
                                        d = transfer_matrix)
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
                           initial_state_rando_proc = "none"
)
{

  # Transfers from and within the community
  total_entries = rowSums(transfer_matrix)
  total_outflows = colSums(transfer_matrix)
  w_in = total_outflows-total_entries
  w_in[w_in < 0] = 0
  w_out = total_entries-total_outflows
  w_out[w_out < 0] = 0

  # Get initial conditions
  initial_state = initial_state_randomisation(size_subpop, I_initial_time, initial_state_rando_proc, transfer_matrix)

  # Create odin model
  model = odin_stoch_model_sis_binom$new(
    beta = beta,
    alpha = alpha,
    s_initial = initial_state$size_subpop - initial_state$I_per_subpop,
    i_initial = initial_state$I_per_subpop,
    n_subpop = length(size_subpop),
    N = size_subpop,
    d = transfer_matrix,
    com_p = community_prev,
    w_in = w_in,
    w_out = w_out
    )
  return(model)

}


