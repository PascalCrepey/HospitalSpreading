benchmark_siminf <- function(model, nRun) {
  result <- run(model)

  results <- microbenchmark(
    SimInfResult = run(model),
    times = nRun)

  return(results)
}



benchmark_odin <- function(model, times, nRun) {
  result <- run(model)

  results <- microbenchmark(
    odinResult = model$run(times, replicate = 1),
    times = nRun)

  return(results)
}



