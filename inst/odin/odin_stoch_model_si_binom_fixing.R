##############################
# PARAMETERS
##############################
# User-specified parameters
beta <- user()
n_hospitals <- user()
s_initial[] <- user()
i_initial[] <- user()
d[,] <- user()

# Dimension of variables
dim(S) <- n_hospitals
dim(I) <- n_hospitals
dim(N) <- n_hospitals

dim(S_temp) <- n_hospitals
dim(I_temp) <- n_hospitals

dim(new_I) <- n_hospitals

dim(s_initial) <- n_hospitals
dim(i_initial) <- n_hospitals

dim(d) <- c(n_hospitals, n_hospitals)
dim(d_prob_out) <- c(n_hospitals, n_hospitals)
dim(t_S) <- c(n_hospitals, n_hospitals)
dim(t_I) <- c(n_hospitals, n_hospitals)
dim(t_S_out) <- n_hospitals
dim(t_I_out) <- n_hospitals

##############################
# MODEL
##############################
# 1. UPDATE INTERNAL PARAMETERS-------------
# Total population in each subpopulation i
N[] <- S[i] + I[i]

# Probabilities of transferring out
d_prob_out[,] <- d[i,j] / sum(d[i,])

# 1. TRAVEL STEP----------------------------
# t_S[,] <- rmultinom(1, S[i], d[i]/N[i])[,1]
# t_I[,] <- rmultinom(1, I[i], d[i]/N[i])[,1]

# Number of susceptible and infected individuals moving out of each subpopulation
t_S_out[] <- min(S[i], rbinom(sum(d[i,]), S[i]/N[i]))
t_I_out[] <- sum(d[i,]) - t_S_out[i]

# Distribution to the connected subpopulations
# t_S[,1] <- if (t_S_out[i] == sum(d[i,])) d[i,1] else rbinom(t_S_out[i], d_prob_out[i,1])
# t_S[,2:(n_hospitals-1)] <- if (t_S_out[i] == sum(d[i,])) d[i,j] else rbinom(t_S_out[i] - sum(t_S[i,1:(i-1)]), d_prob_out[i,j])
# t_S[,n_hospitals] <- t_S_out[i] - sum(t_S[i,1:(n_hospitals-1)])
t_S[,] <- min(S[i], round(d[i,j]*S[i]/N[i]))

# Transferred infected individuals
t_I[,] <- d[i,j]-t_S[i,j]

# Update compartments
S_temp[] <- S[i] - sum(t_S[i,]) + sum(t_S[,i])
I_temp[] <- I[i] - sum(t_I[i,]) + sum(t_I[,i])

# 2. STEP EPIDEMICS--------------------------
new_I[] <- rbinom(S_temp[i], 1-exp(-beta*I_temp[i]/N[i]))
update(S[]) <- S_temp[i] - new_I[i]
update(I[]) <- I_temp[i] + new_I[i]

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
