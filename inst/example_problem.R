######################################
# Test odin model outputs
######################################
rm(list = ls())

odin::odin_package(".")
pkgload::load_all()

# Input parameters
# df_transfer = matrix(c(0, 5, 2,
#                        3, 0, 3,
#                        4, 1, 0),
#                      ncol = 3, byrow = T)
n_hospitals = 20
df_transfer = make_fake_matrix(n_hospitals, 20)

s_initial = rep(100, n_hospitals)
i_initial = c(1, rep(0, n_hospitals-1))

#run the model
model <- initialize_odin_binomial(beta = 0.16,
                                  n_subpop = n_hospitals,
                                  size_subpop = s_initial + i_initial,
                                  transfer_matrix = df_transfer,
                                  I_per_subpop = i_initial)
date_lim = 100
res <- model$run(0:date_lim, replicate = 1)
summary(apply(res[,grepl("^I", dimnames(res)[[2]]),1], 2, min))
summary(apply(res[,grepl("^S", dimnames(res)[[2]]),1], 2, min))
summary(apply(res[,grepl("^t_I", dimnames(res)[[2]]),1], 2, min))
matplot(res[,grepl("^I", dimnames(res)[[2]]),1], type = "l", ylab = "I")
matplot(res[,grepl("^S", dimnames(res)[[2]]),1], type = "l", ylab = "S")
matplot(res[,grepl("^t_I", dimnames(res)[[2]]),1], type = "l", ylab = "Transferred I")
