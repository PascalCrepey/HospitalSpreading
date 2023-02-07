########### Simple stochastic SI with odin ##########

################
## INPUT DATA ##
################

df_transfer = data.frame(A = c(NA, 3, 4),
                         B = c(5, NA, 1),
                         C = c(2, 3, NA),
                         row.names = c("A", "B", "C"))

n_hospitals = 3

s_initial = rep(100, n_hospitals)
i_initial = rep(1, n_hospitals)

param_beta = 0.16

################
##    MODEL  ##
###############

ode_model <- odin::odin(
  {

    #Total population in each hospital i
    N[] <- S[i] + I[i]

    #stochastic update
    update(S[]) <- S[i] - max(rbinom(S[i], beta*I[i]/N[i]), 0) + d[i,j]
    update(I[]) <- I[i] + max(rbinom(I[i], beta*I[i]/N[i]), 0) + d[i,j]

    #Initial condition
    initial(S[]) <- s_initial
    initial(I[]) <- i_initial

    #initialize parameters
    beta <- user()

    #initialize user input
    n_hospitals <- user()
    s_initial <- user()
    i_initial <- user()

    #dimension of parameters
    # dim(beta) #one dimension because beta is hospital-indepedent

    #dimension of compartments
    dim(N) <- n_hospitals
    dim(S) <- n_hospitals
    dim(I) <- n_hospitals

    d[,] <- c(n_hospitals, n_hospitals)

    #matrix of transfer
    d[i, j]

  }
)

################
##    PLOT  ##
###############


##################
## CALCUL TIME ##
#################


