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
dim(S_temp) <- n_hospitals
dim(I_temp) <- n_hospitals
dim(new_I) <- n_hospitals
dim(s_initial) <- n_hospitals
dim(i_initial) <- n_hospitals
dim(N) <- n_hospitals
dim(N_temp) <- n_hospitals

dim(t_S_out) <- n_hospitals

dim(d) <- c(n_hospitals, n_hospitals)
dim(t_S) <- c(n_hospitals, n_hospitals)
dim(t_I) <- c(n_hospitals, n_hospitals)

##############################
# MODEL
##############################
# 1. UPDATE INTERNAL PARAMETERS-------------
# Total population in each subpopulation i
N[] <- S[i] + I[i]

# 1. TRAVEL STEP----------------------------
# Number of transfers out of the hospitals
# t_S[,] <- rmultinom(1, S[i], d[i]/N[i])[,1]
# t_I[,] <- rmultinom(1, I[i], d[i]/N[i])[,1]

t_S_out[] <- min(rbinom(sum(d[i,]), S[i]/N[i]))
#t_S[i,] <- rmultinom(t_S_out[i], d[i,])
t_S[,] <- min(S[i], round(d[i,j]*S[i]/N[i]))
t_I[,] <- d[i,j]-t_S[i,j]

# Update compartments
S_temp[] <- S[i] - sum(t_S[i,]) + sum(t_S[,i])
I_temp[] <- I[i] - sum(t_I[i,]) + sum(t_I[,i])
N_temp[] <- S_temp[i] + I_temp[i]

# 2. STEP EPIDEMICS--------------------------
new_I[] <- rbinom(S_temp[i], 1-exp(-beta*I_temp[i]/N_temp[i]))
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
