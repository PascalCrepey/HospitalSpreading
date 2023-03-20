##############################
# PARAMETERS
##############################
# User-specified parameters
beta <- user()
alpha <- user()

n_subpop <- user()
n_com_subpop <- user()

s_initial[] <- user()
i_initial[] <- user()
N[] <- user()
d[,] <- user()

com_p <- user()

# Dimension of variables
dim(S) <- n_subpop
dim(I) <- n_subpop
dim(S_temp) <- n_subpop
dim(I_temp) <- n_subpop
dim(new_S) <- n_subpop
dim(new_I) <- n_subpop
dim(s_initial) <- n_subpop
dim(i_initial) <- n_subpop
dim(N) <- n_subpop
dim(N_temp) <- n_subpop

dim(S_prev) <- n_com_subpop

dim(d) <- c(n_com_subpop, n_com_subpop)
dim(t_S_out) <- n_com_subpop
dim(t_S) <- c(n_com_subpop, n_com_subpop)
dim(t_I) <- c(n_com_subpop, n_com_subpop)

##############################
# MODEL
##############################
# 1. TRAVEL STEP----------------------------
# Prevalence of susceptible
S_prev[1] <- com_p
S_prev[2:(n_subpop+1)] <- S[i]/N[i]

# Vector of transferred susceptible
t_S_out[] <- rbinom(sum(d[i,]), S_prev[i])

# Number of transfers between hospitals
t_S[,] <- round(d[i,j]*S[i]/N[i])
t_I[,] <- d[i,j]-t_S[i,j]

# Update compartments
S_temp[] <- S[i] - sum(t_S[i,]) + sum(t_S[,i])
I_temp[] <- I[i] - sum(t_I[i,]) + sum(t_I[,i])
N_temp[] <- S_temp[i] + I_temp[i]

# 2. STEP EPIDEMICS--------------------------
new_I[] <- rbinom(S_temp[i], 1-exp(-beta*I_temp[i]/N_temp[i]))
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
