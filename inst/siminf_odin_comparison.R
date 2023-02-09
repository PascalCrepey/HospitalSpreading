##################
## Load library ##
##################
library(data.table)
library(SimInf)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(tictoc)
library(deSolve)

source("R/getSimInfModel.R", echo=TRUE)
source("R/siminf_supportFunctions.R", echo=TRUE)
source("R/initialize_odin.R", echo=TRUE)


########################
## General parameters ##
########################
time_step = 0.01
beta = 0.5
beta_odin = beta*time_step

tspan_max = 100
tspan_odin = 1:(tspan_max/time_step)

tspan = 1:tspan_max
npop <- 3

pop_size = 100000
index_size = 100


##############################
## Populations and matrices ##
##############################

starting_pop = rep(pop_size, npop)
index_pop = c(index_size, rep(0, npop - 1))

trans_mat = make_fake_matrix(nmetapop = npop, scale = 5)

trans_mat = matrix(0, nrow = npop, ncol = npop)


#######################
## Initialise siminf ##
#######################

siminf_model <- getSimInfModel(beta = beta, tspan = tspan, npop = npop
               , n_initial_infected = index_pop, starting_pop = starting_pop
               , trans_mat = trans_mat)

odin_model_binomial100 <- initialize_odin_binomial(beta = beta*0.01,
                            n_subpop = npop,
                            size_subpop = starting_pop,
                            I_per_subpop = index_pop,
                            transfer_matrix = trans_mat

)

odin_model_binomial5 <- initialize_odin_binomial(beta = beta*0.2,
                                          n_subpop = npop,
                                          size_subpop = starting_pop,
                                          I_per_subpop = index_pop,
                                          transfer_matrix = trans_mat

)

odin_model_poisson <- initialize_odin_poisson(beta = beta,
                                          n_subpop = npop,
                                          size_subpop = starting_pop,
                                          I_per_subpop = index_pop,
                                          transfer_matrix = trans_mat

)



################
##    Run each model  ##
###############
#
# #SimInf
#
# siminf_result <- run(siminf_model)
#
# #odin
# # res <- odin_model$run(tspan)
# res <- odin_model$run(tspan_odin)
#





# ################
# ##    PLOT  ##
# ###############
#
#
# siminf_tib <- convert_siminfU(siminf_result@U, times = tspan, comparts = c("S", "I")
#                                , nmetapop = npop)
#
# odin_tib <- res %>% as_tibble %>%
#   select(step, starts_with("S"), starts_with("I")) %>%
#   rename_all(~gsub("\\[", "_", .x)) %>%
#   rename_all(~gsub("\\]", "", .x)) %>%
#   rename(times = step) %>%
#   pivot_longer(-times, names_sep = "_", names_to = c("state", "metapop")) %>%
#   mutate(metapop = as.numeric(metapop))
#
#
# siminf_tib %>%
#   ggplot(aes(x = times, y = value, colour = state)) +
#   geom_line() +
#   facet_wrap(metapop~., ncol = 10) +
#   labs(title = "SimInf")
#
# odin_tib %>%
#   ggplot(aes(x = times, y = value, colour = state)) +
#   geom_line() +
#   facet_wrap(metapop~., ncol = 10) +
#   labs(title = "Odin")

################################
####### multiple sims ##########
################################

Nsims = 100

odin_poisson_runs <- odin_model_poisson$run(tspan, replicate = Nsims)
odin_binomial100_runs <- odin_model_binomial100$run(1:(tspan_max/0.01), replicate = Nsims)
odin_binomial5_runs <- odin_model_binomial5$run(1:(tspan_max/0.2), replicate = Nsims)

for(i in 1:Nsims){

  siminf_multi_piece <- convert_siminfU(run(siminf_model)@U, times = tspan, comparts = c("S", "I")
                                        , nmetapop = npop) %>%
    mutate(sim = i)

  odin_poisson_multi_piece <- odin_poisson_runs[,,i] %>%
    as_tibble %>%
    select(step, starts_with("S"), starts_with("I")) %>%
    rename_all(~gsub("\\[", "_", .x)) %>%
    rename_all(~gsub("\\]", "", .x)) %>%
    rename(times = step) %>%
    pivot_longer(-times, names_sep = "_", names_to = c("state", "metapop")) %>%
    mutate(metapop = as.numeric(metapop)
           , sim = i)

  odin_binomial100_multi_piece <- odin_binomial100_runs[,,i] %>%
    as_tibble %>%
    select(step, starts_with("S"), starts_with("I")) %>%
    rename_all(~gsub("\\[", "_", .x)) %>%
    rename_all(~gsub("\\]", "", .x)) %>%
    rename(times = step) %>%
    pivot_longer(-times, names_sep = "_", names_to = c("state", "metapop")) %>%
    mutate(metapop = as.numeric(metapop)
           , sim = i)

  odin_binomial5_multi_piece <- odin_binomial5_runs[,,i] %>%
    as_tibble %>%
    select(step, starts_with("S"), starts_with("I")) %>%
    rename_all(~gsub("\\[", "_", .x)) %>%
    rename_all(~gsub("\\]", "", .x)) %>%
    rename(times = step) %>%
    pivot_longer(-times, names_sep = "_", names_to = c("state", "metapop")) %>%
    mutate(metapop = as.numeric(metapop)
           , sim = i)



  if(i == 1){
    siminf_multi <- siminf_multi_piece
    odin_poisson_multi <- odin_poisson_multi_piece
    odin_binomial100_multi <- odin_binomial100_multi_piece
    odin_binomial5_multi <- odin_binomial5_multi_piece
    odin_poisson_multi <- odin_poisson_multi_piece


  } else {
    siminf_multi <- rbind(siminf_multi, siminf_multi_piece)
    odin_binomial100_multi <- rbind(odin_binomial100_multi, odin_binomial100_multi_piece)
    odin_binomial5_multi <- rbind(odin_binomial5_multi, odin_binomial5_multi_piece)
    odin_poisson_multi <- rbind(odin_poisson_multi, odin_poisson_multi_piece)
  }
}

siminf_multi
odin_poisson_multi
odin_binomial100_multi
odin_binomial5_multi

# siminf_multi %>%
#   ggplot(aes(x = times, y = value, colour = state, group = interaction(state, sim))) +
#   geom_line(alpha = 0.1) +
#   facet_wrap(metapop~., ncol = 10) +
#   labs(title = "SimInf") +
#   theme_bw()

# deSolve
init <- c(S=pop_size - index_size,I=index_size)
parameters <- c(bet=beta)

eqn <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    dS <- -bet*S*I/(S + I)
    dI <- bet*S*I/(S + I)
    return(list(c(dS,dI)))})}

out<-ode(y=init,times=tspan,eqn,parms=parameters)
out.deSolve<-as.data.frame(out) %>%
  pivot_longer(-time, names_to = "state", values_to = "mean") %>%
  mutate(metapop = 1, package = "deSolve") %>%
  rename(times = time)




rbind(siminf_multi %>%
  group_by(times, metapop, state) %>%
  summarise(mean = mean(value)
            , lo = max(value)
            , hi = min(value)) %>%
    mutate(package = "SimInf")
, odin_binomial100_multi %>%
  mutate(times = times*0.01) %>%
  group_by(times, metapop, state) %>%
  summarise(mean = mean(value)
            , lo = max(value)
            , hi = min(value)) %>%
  mutate(package = "Odin binomial step 100th")
, odin_binomial5_multi %>%
  mutate(times = times*0.2) %>%
  group_by(times, metapop, state) %>%
  summarise(mean = mean(value)
            , lo = max(value)
            , hi = min(value)) %>%
  mutate(package = "Odin binomial step 5th")
, odin_poisson_multi %>%
  group_by(times, metapop, state) %>%
  summarise(mean = mean(value)
            , lo = max(value)
            , hi = min(value)) %>%
  mutate(package = "Odin poisson")
, out.deSolve) %>%
  # filter(package != "Odin") %>%
  filter(times < 30, metapop == 1) %>%
  ggplot(aes(x = times, colour = package)) +
  geom_line(aes(y = mean, linetype = "mean")) +
  geom_line(aes(y = lo, linetype = "range")) +
  geom_line(aes(y = hi, linetype = "range")) +
  facet_grid(.~state)
ggsave("plots/comparison_odin_siminf.png", height = 4, width = 7)



