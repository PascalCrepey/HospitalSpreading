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
new_I[] <- rbinom(S[i], 1-exp(-beta*I[i]/N[i]))
update(S[]) <- S[i] - new_I[i] - t_S_out[i] + t_S_in[i]
update(I[]) <- I[i] + new_I[i] - t_I_out[i] + t_I_in[i]

#Initial condition
initial(S[]) <- s_initial[i]
initial(I[]) <- i_initial[i]

#parameters
beta <- user()

#initialize user input
n_hospitals <- user()
s_initial[] <- user()
i_initial[] <- user()
d[,] <- user()

#dimension of parameters
# dim(beta) #one dimension because beta is hospital-indepedent

#dimension of compartments
dim(N) <- n_hospitals
dim(S) <- n_hospitals
dim(I) <- n_hospitals
dim(new_I) <- n_hospitals
dim(s_initial) <- n_hospitals
dim(i_initial) <- n_hospitals

dim(d) <- c(n_hospitals, n_hospitals)
dim(t_S) <- c(n_hospitals, n_hospitals)
dim(t_I) <- c(n_hospitals, n_hospitals)

dim(t_S_out) <- n_hospitals
dim(t_I_out) <- n_hospitals

dim(t_S_in) <- n_hospitals
dim(t_I_in) <- n_hospitals

# print variables at each time step
output(t_S_out[]) <- TRUE
output(t_I_out[]) <- TRUE

output(t_S_in[]) <- TRUE
output(t_I_in[]) <- TRUE
