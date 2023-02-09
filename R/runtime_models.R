#' Benchmark SimInf metapopulation model
#'
#' @param model a SimInf model
#' @param nRun the number of runs
#'
#' @return results of the benchmark
#' @export
#'
benchmark_siminf <- function(model, nRun) {
  result <- run(model)
  results <- microbenchmark(
    SimInfResult = run(model),
    times = nRun)

  return(results)
}


#' Benchmark Odin metatpopulation model
#'
#' @param model Odin model
#' @param times the time vector
#' @param nRun the number of runs
#'
#' @return the result of the benchmark
#' @export
#'
benchmark_odin <- function(model, times, nRun) {
  results <- microbenchmark(
    odinResult = model$run(times, replicate = 1),
    times = nRun)

  return(results)
}



