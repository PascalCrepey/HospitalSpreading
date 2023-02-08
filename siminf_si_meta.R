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

events <- data.frame(
  event      = rep(rep(3, 9), tspan_max),  ## Event "extTrans" is a movement between nodes// 0) exit, 1) enter, 2) internal transfer, and 3) external transfer
  time       = rep(1:tspan_max, each = 9), ## The time that the event happens
  node       = rep(c(1, 1, 1, 2, 2, 2, 3, 3, 3), tspan_max), ## In which node does the event occur
  dest       = rep(c(1, 2, 3, 1, 2, 3, 1, 2, 3), tspan_max), ## Which node is the destination node
  n          = rep(c(0, 5, 2, 3, 0, 3, 4, 1, 0), tspan_max), ## How many individuals are moved
  proportion = rep(rep(0, 9), tspan_max), ## This is not used when n > 0
  select     = rep(rep(1, 9), tspan_max), ## Use the 4th column in the model select matrix
  shift      = rep(rep(0, 9), tspan_max) ## Not used in this example
)
events <- events[events$node != events$dest, ]

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
result

plot(result)
plot(result, index = 1)
plot(result, index = 2)
plot(result, index = 3)


plot(result, "S", index = 1)
plot(result, "S", index = 2)
plot(result, "S", index = 3)

################
##    PLOT  ##
###############

result@U
result@U %>%
  t %>%
  as.vector %>%
  cbind(., rep(c("S", "I"), each = tspan_max))
result %>% as.data.frame

##################
## CALCUL TIME ##
#################


