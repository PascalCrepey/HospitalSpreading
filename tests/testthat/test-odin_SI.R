test_that("constant subpopulation size", {
  df_transfer = matrix(c(0, 5, 2,
                         3, 0, 3,
                         4, 1, 0),
                       ncol = 3, byrow = T)

  n_hospitals = 3

  s_initial = rep(100, n_hospitals)
  i_initial = c(1, rep(0, n_hospitals-1))

  #run the model
  model <- initialize_odin_binomial(beta = 0.16,
                                    n_subpop = 3,
                                    size_subpop = s_initial + i_initial,
                                    transfer_matrix = df_transfer,
                                    I_per_subpop = i_initial)
  date_lim = 30
  res <- model$run(0:date_lim, replicate = 1)
  unique_subpopsize = lapply(1:n_hospitals, function(x) unique(rowSums(res[,,1][,c(1+x, 1+n_hospitals+x)])))
  out = sapply(unique_subpopsize, length)
  expect_equal(sum(out), n_hospitals)

  # plot(res[,,1][,1], res[,,1][,2], type = "l", xlab = "Time", ylab = "Number of individuals", col = "black")
  # lines(res[,,1][,1], res[,,1][,3], col = "red")
  # legend("topright", lwd = 1, col = c("black", "red"), legend = c("S", "I"), bty = "n")
})
