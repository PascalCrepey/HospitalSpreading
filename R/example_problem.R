df_transfer = matrix(c(0, 5, 2,
                       3, 0, 3,
                       4, 1, 0),
                     ncol = 3, byrow = T)

n_hospitals = 3

s_initial = rep(10, n_hospitals)
i_initial = c(1, rep(0, n_hospitals-1))

#run the model
model <- initialize_odin_binomial(beta = 0.16,
                                  n_subpop = 3,
                                  size_subpop = s_initial + i_initial,
                                  transfer_matrix = df_transfer,
                                  I_per_subpop = i_initial)
date_lim = 30
res <- model$run(0:date_lim, replicate = 1)
res
