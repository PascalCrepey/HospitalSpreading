# library(data.table)
# library(SimInf)
# library(magrittr)
# library(ggplot2)
# library(tidyverse)
#
# source("~/Desktop/HospitalSpreading/R/getSimInfModel.R", echo=TRUE)
# source("~/Desktop/HospitalSpreading/R/siminf_supportFunctions.R", echo=TRUE)
# source("~/Desktop/HospitalSpreading/R/initialize_odin.R", echo=TRUE)
# source("~/Desktop/HospitalSpreading/R/runtime_models.R", echo=TRUE)
#
#
#
# ########################
# ## General parameters ##
# ########################
# time_step = 0.2
# beta = 0.5
# beta_odin = beta*time_step
#
# tspan_max = 365
# tspan_odin = 1:(tspan_max/time_step)
#
# tspan = 1:tspan_max
# npop <- 100
#
# pop_size = 100000
# index_size = 100
#
#
# ##############################
# ## Populations and matrices ##
# ##############################
#
# starting_pop = rep(pop_size, npop)
# index_pop = c(index_size, rep(0, npop - 1))
#
# trans_mat = make_fake_matrix(nmetapop = npop, scale = 5)
#
# trans_mat = matrix(0, nrow = npop, ncol = npop)
#
#
#
#
#
# #######################
# ## Initialise siminf ##
# #######################
#
# siminf_model <- getSimInfModel(beta = beta, tspan = tspan, npop = npop
#                                , n_initial_infected = index_pop, starting_pop = starting_pop
#                                , trans_mat = trans_mat)
#
# odin_model <- initialize_odin(beta = beta_odin,
#                               n_subpop = npop,
#                               size_subpop = starting_pop,
#                               I_per_subpop = index_pop,
#                               transfer_matrix = trans_mat
#
# )
#
#
# #######################
# ##   run time test   ##
# #######################
# print("starting test")
# b_siminf = benchmark_siminf(siminf_model, nRun = 10)
# b_odin = benchmark_odin(odin_model, times = tspan_odin, nRun = 10)
#
