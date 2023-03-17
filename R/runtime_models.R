#' Benchmark SimInf metapopulation model
#'
#' @param model a SimInf model
#' @param nRun the number of runs
#'
#' @return results of the benchmark
#' @export
#'
#' @importFrom microbenchmark microbenchmark
#' @importFrom SimInf run
#'
benchmark_siminf <- function(model, nRun) {
  result <- run(model)
  results <- microbenchmark::microbenchmark(
    SimInfResult = SimInf::run(model),
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
#' @importFrom microbenchmark microbenchmark
#'
benchmark_odin <- function(model, times, nRun) {
  results <- microbenchmark::microbenchmark(
    odinResult = model$run(times, replicate = 1),
    times = nRun)

  return(results)
}



