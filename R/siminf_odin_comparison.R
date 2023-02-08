##################
## Load library ##
##################
library(data.table)
library(SimInf)
library(magrittr)
library(ggplot2)
library(tidyverse)

source("~/SPHINx/HospitalSpreading/getSimInfModel.R", echo=TRUE)
source("~/SPHINx/HospitalSpreading/siminf_supportFunctions.R", echo=TRUE)
source("~/SPHINx/HospitalSpreading/R/initialize_odin.R", echo=TRUE)


########################
## General parameters ##
########################
beta = 0.16
tspan_max = 100
tspan = 1:tspan_max
npop <- 3

pop_size = 1000
index_size = 1


##############################
## Populations and matrices ##
##############################

starting_pop = rep(pop_size, npop)
index_pop = c(index_size, rep(0, npop - index_size))

trans_mat = make_fake_matrix(nmetapop = npop, scale = 5)



#######################
## Initialise siminf ##
#######################

siminf_model <- getSimInfModel(beta = beta, tspan = tspan, npop = npop
               , n_initial_infected = index_pop, starting_pop = starting_pop
               , trans_mat = trans_mat)

odin_model <- initialize_odin(beta = beta,
                            n_subpop = npop,
                            size_subpop = starting_pop,
                            I_per_subpop = index_pop,
                            transfer_matrix = trans_mat

)





################
##    Run each model  ##
###############

#SimInf
siminf_result <- run(siminf_model)

#odin
res <- odin_model$run(tspan)


################
##    PLOT  ##
###############


siminf_tib <- convert_siminfU(siminf_result@U, times = tspan, comparts = c("S", "I")
                               , nmetapop = npop)

odin_tib <- res %>% as_tibble %>%
  select(step, starts_with("S"), starts_with("I")) %>%
  rename_all(~gsub("\\[", "_", .x)) %>%
  rename_all(~gsub("\\]", "", .x)) %>%
  rename(times = step) %>%
  pivot_longer(-times, names_sep = "_", names_to = c("state", "metapop"))


siminf_tib %>%
  ggplot(aes(x = times, y = value, colour = state)) +
  geom_line() +
  facet_wrap(metapop~.) +
  labs(title = "SimInf")

odin_tib %>%
  ggplot(aes(x = times, y = value, colour = state)) +
  geom_line() +
  facet_wrap(metapop~.) +
  labs(title = "Odin")
