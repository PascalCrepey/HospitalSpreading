########### Simple stochastic SI with odin ##########

################
## INPUT DATA ##
################

df_transfer = data.frame(A = c(0, 3, 4),
                         B = c(5, 0, 1),
                         C = c(2, 3, 0),
                         row.names = c("A", "B", "C"))

n_hospitals = 3

s_initial = rep(100, n_hospitals)
i_initial = rep(1, n_hospitals)

param_beta = 0.16

################
##    MODEL  ##
###############

stoch_model_si <- odin::odin(
  {

    #Total population in each hospital i
    N[] <- S[i] + I[i]

    #number of patients moving that are S or I
    t_S[,] <- rbinom(d[i,j], S[i]/N[i])
    t_I[,] <- d[i,j] - t_S[i,j]

    #Vectors of transfers out of hospitals
    t_S_out[] <- sum(t_S[i,])
    t_I_out[] <- sum(t_I[i,])

    #Vectors of transfers into hospitals
    t_S_in[] <- sum(t_S[,i])
    t_I_in[] <- sum(t_I[,i])

    #stochastic update
    update(S[]) <- S[i] - max(rbinom(S[i], 1-exp(-beta*I[i]/N[i])), 0) - t_S_out[i] + t_S_in[i]
    update(I[]) <- I[i] + max(rbinom(I[i], 1-exp(-beta*I[i]/N[i])), 0) - t_I_out[i] + t_I_in[i]

    #Initial condition
    initial(S[]) <- s_initial
    initial(I[]) <- i_initial

    #initialize parameters
    beta <- user()

    #initialize user input
    n_hospitals <- user()
    s_initial <- user()
    i_initial <- user()
    d[,] <- user()

    #dimension of parameters
    # dim(beta) #one dimension because beta is hospital-indepedent

    #dimension of compartments
    dim(N) <- n_hospitals
    dim(S) <- n_hospitals
    dim(I) <- n_hospitals

    dim(d) <- c(n_hospitals, n_hospitals)
    dim(t_S) <- c(n_hospitals, n_hospitals)
    dim(t_I) <- c(n_hospitals, n_hospitals)

    dim(t_S_out) <- n_hospitals
    dim(t_I_out) <- n_hospitals

    dim(t_S_in) <- n_hospitals
    dim(t_I_in) <- n_hospitals

  }
)

#run the model
model <- stoch_model_si$new(user = pars)
date_lim = 30
res <- model$run(0:date_lim)

################
##    PLOT  ##
###############


##################
## CALCUL TIME ##
#################


