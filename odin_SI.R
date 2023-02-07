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

stoch_model_si <- odin::odin(
  {

    #Total population in each hospital i
    N[] <- S[i] + I[i]

    #number of patients moving that are S or I
    n_S_out[] <- rbinom(sum(d[i,]), S[i]/N[i])
    n_I_out[] <- sum(d[i,]) - n_S_out[i]

    #A revoir!!
    n_S_in[] <- rbinom(sum(d[,i]), ((sum(S[])-S[i]) / (sum(N[])-N[i])) )
    n_I_in[] <- sum(d[,i]) - n_S_in[i]

    #stochastic update
    update(S[]) <- S[i] - max(rbinom(S[i], beta*I[i]/N[i]), 0) - n_S_out[i] + n_S_in[i]
    update(I[]) <- I[i] + max(rbinom(I[i], beta*I[i]/N[i]), 0) - n_I_out[i] + n_I_in[i]

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

    dim(n_S_out) <- n_hospitals
    dim(n_I_out) <- n_hospitals

    dim(n_S_in) <- n_hospitals
    dim(n_I_in) <- n_hospitals

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


