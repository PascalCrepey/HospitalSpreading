test_that("constant subpopulation size", {
  unique_subpopsize = lapply(1:n_hospitals, function(x) unique(rowSums(res[,,1][,c(1+x, 1+n_hospitals+x)])))
  out = sapply(unique_subpopsize, length)
  expect_equal(sum(out), n_hospitals)
})
