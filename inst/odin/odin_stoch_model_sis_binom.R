##############################
# PARAMETERS
##############################
# User-specified parameters
beta <- user()
alpha <- user()
com_p <- user()

n_subpop <- user()
n_com_subpop <- n_subpop+1

s_initial[] <- user()
i_initial[] <- user()
N[] <- user()
d[,] <- user()

# Dimension of variables
dim(S) <- n_subpop
dim(I) <- n_subpop
dim(N) <- n_subpop

dim(S_temp) <- n_subpop
dim(I_temp) <- n_subpop

dim(new_S) <- n_subpop
dim(new_I) <- n_subpop

dim(s_initial) <- n_subpop
dim(i_initial) <- n_subpop

dim(S_prev) <- n_com_subpop

dim(d) <- c(n_com_subpop, n_com_subpop)
dim(d_prob_out) <- c(n_com_subpop, n_com_subpop)
dim(t_S_out_all) <- n_com_subpop
dim(t_S) <- c(n_com_subpop, n_com_subpop)
dim(t_I) <- c(n_com_subpop, n_com_subpop)

##############################
# MODEL
##############################
# 1. TRAVEL STEP----------------------------
# Prevalence of susceptible
S_prev[1:n_subpop] <- S[i]/N[i]
S_prev[n_com_subpop] <- com_p

# Probability of transfer
d_prob_out[,] <- d[i,j] / sum(d[i,])

# Vector of transferred susceptible
t_S_out_all[] <- if (i<n_subpop+1) min(S[i], rbinom(sum(d[i,]), S_prev[i])) else rbinom(sum(d[i,]), S_prev[i])

# Distribution of transferred susceptible individuals into the connected subpopulations
# t_S[,1] <- rbinom(t_S_out_all[i], d_prob_out[i,1])
# t_S[,2:n_com_subpop] <- rbinom(t_S_out_all[i] - sum(t_S[i,1:(i-1)]), d_prob_out[i,j])
t_S[,] <- min(S[i], round(d[i,j]*S_prev[i]))

# Number of transferred infected individuals
t_I[,] <- d[i,j]-t_S[i,j]

# Update compartments
S_temp[] <- S[i] - sum(t_S[i,]) + sum(t_S[,i])
I_temp[] <- I[i] - sum(t_I[i,]) + sum(t_I[,i])

# 2. STEP EPIDEMICS--------------------------
new_I[] <- rbinom(S_temp[i], 1-exp(-beta*I_temp[i]/N[i]))
new_S[] <- rbinom(I_temp[i], 1-exp(-alpha))

update(S[]) <- S_temp[i] - new_I[i] + new_S[i]
update(I[]) <- I_temp[i] + new_I[i] - new_S[i]

##############################
# INITIAL CONDITIONS
##############################
initial(S[]) <- s_initial[i]
initial(I[]) <- i_initial[i]

##############################
# ADDITIONNAL VARIABLES TO
# STORE AT EACH TIME STEP
##############################
output(new_I[]) <- TRUE
output(t_I[,]) <- TRUE
