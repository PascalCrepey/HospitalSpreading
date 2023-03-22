######################################
# Test odin model outputs
######################################
rm(list = ls())

odin::odin_package(".")
pkgload::load_all()

######################################
# SI MODEL
######################################
# Input parameters
# df_transfer = matrix(c(0, 5, 2,
#                        3, 0, 3,
#                        4, 1, 0),
#                      ncol = 3, byrow = T)
n_hospitals = 5
df_transfer = make_fake_matrix(n_hospitals, 5)

s_initial = rep(100, n_hospitals)
i_initial = c(1, rep(0, n_hospitals-1))

# Run the model
model <- initialize_odin_binomial(beta = 0.16,
                                  n_subpop = n_hospitals,
                                  size_subpop = s_initial + i_initial,
                                  transfer_matrix = df_transfer,
                                  I_per_subpop = i_initial)
date_lim = 100
res <- model$run(0:date_lim, replicate = 1)
head(res[,,1],5)

summary(apply(res[,grepl("^I", dimnames(res)[[2]]),1], 2, min))
summary(apply(res[,grepl("^S", dimnames(res)[[2]]),1], 2, min))
summary(apply(res[,grepl("^t_I", dimnames(res)[[2]]),1], 2, min))

par(mfrow = c(2,2))
matplot(res[,grepl("^I", dimnames(res)[[2]]),1], type = "l", ylab = "Infected individuals")
matplot(res[,grepl("^S", dimnames(res)[[2]]),1], type = "l", ylab = "Susceptible individuals")
matplot(res[,grepl("^t_I", dimnames(res)[[2]]),1], type = "l", ylab = "Transferred infected individuals")

# Compare number of transitions to input transfer matrix
res_trans = res[,grepl("t_tot", dimnames(res)[[2]]),1]
res_trans = asplit(res_trans, MARGIN = 1)
res_trans = lapply(res_trans, function(x) matrix(x, ncol = n_hospitals, nrow = n_hospitals, byrow = F))
sum(!sapply(res_trans, function(x) identical(x, df_transfer)))

######################################
# SIS MODEL
######################################
n_hospitals = 3
df_transfer = matrix(c(0,1,2,
                       4,0,2,
                       3,8,0), byrow = T, ncol = 3)

size_subpop = rep(150, n_hospitals)
i_initial = c(10, rep(0, n_hospitals-1))

# Run the model
model <- initialize_sis(beta = 0.16,
                        alpha = 0.1,
                        size_subpop = size_subpop,
                        transfer_matrix = df_transfer,
                        I_initial_time = i_initial,
                        community_prev = 0.25
                        )

date_lim = 100
res <- model$run(1:date_lim, replicate = 1)

summary(apply(res[,grepl("^I", dimnames(res)[[2]]),1], 2, min))
summary(apply(res[,grepl("^S", dimnames(res)[[2]]),1], 2, min))
summary(apply(res[,grepl("^t_I", dimnames(res)[[2]]),1], 2, min))

par(mfrow = c(2,2))
matplot(res[,grepl("^I", dimnames(res)[[2]]),1], type = "l", ylab = "Infected individuals")
matplot(res[,grepl("^S", dimnames(res)[[2]]),1], type = "l", ylab = "Susceptible individuals")
matplot(res[,grepl("^t_I", dimnames(res)[[2]]),1], type = "l", ylab = "Transferred infected individuals")

# Compare number of transitions to input transfer matrix
res_trans = res[,grepl("t_tot", dimnames(res)[[2]]),1]
res_trans = asplit(res_trans, MARGIN = 1)
res_trans = lapply(res_trans, function(x) matrix(x, ncol = n_hospitals+1, nrow = n_hospitals+1, byrow = F)[1:n_hospitals, 1:n_hospitals])
sum(!sapply(res_trans, function(x) identical(x, df_transfer)))
