Ntot = 5
p_vec = c(0.2,0.2,0.6)

# Multinomial random generator
set.seed(123)
rmultinom(1,Ntot,p_vec)

# Binomial
set.seed(123)
x1 = rbinom(1, Ntot, p_vec[1])
x2 = rbinom(1, Ntot-x1, p_vec[2])
x3 = Ntot-x1-x2
c(x1,x2,x3)
