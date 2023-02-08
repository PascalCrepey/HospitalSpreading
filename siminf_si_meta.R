##################
## Load library ##
##################
library(data.table)
library(SimInf)
library(magrittr)
library(ggplot2)


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
tspan_max = 100
tspan = 1:tspan_max

########################
## Add sub population ##
########################
npop <- 3

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

events <- make_siminf_events(times = tspan, nmetapop = npop, transfer_matrix = flux
                   , select = 1, shift = 0)

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
result <- run(model)

################
##    PLOT  ##
###############


results_tib <- convert_siminfU(result@U, times = tspan, comparts = compartments
                            , nmetapop = npop)



results_tib %>%
  ggplot(aes(x = times, y = value, colour = state)) +
  geom_line() +
  facet_wrap(metapop~.)

##################
## CALCUL TIME ##
#################




#Do this 1000 times
#Compare average results

microbenchmark




