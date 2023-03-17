
#' Creates a random matrix of transfers between metapopulations
#'
#' @param nmetapop number of metapopulations
#' @param scale average number of transfers for each pair of metapopulations
#'
#' @return transfer matrix
#' @export
#'
make_fake_matrix <- function(nmetapop, scale = 5){
  # set.seed(1)
  output <- matrix(0, nrow = nmetapop, ncol = nmetapop)
  i = 1
  for(i in 1:scale){
    A= diag(nmetapop)
    reorderIndex = sample(nrow(A))
    while(any(reorderIndex == 1:nmetapop)){
      # print("running")
      reorderIndex = sample(nrow(A))

    }
    output = output + A[reorderIndex, ]
    # print(output)

  }

  output
}

#' Converts a daily transfer matrix into a data frame of daily transfer events
#'
#' @param times vector of time steps
#' @param nmetapop number of metapopulations
#' @param transfer_matrix matrix of daily transfers, from the row to the column
#' @param select the state compartments in the source metapopulation which can be sampled: by default any state can be chosen at random
#' @param shift ignore for now
#'
#' @return data frame of events
#' @export
#'
make_siminf_events <- function(times, nmetapop, transfer_matrix, select = 1, shift = 0){
  events <- data.frame(
    event      = 3,  ## Event "extTrans" is a movement between nodes// 0) exit, 1) enter, 2) internal transfer, and 3) external transfer
    time       = rep(times, each = nmetapop*nmetapop), ## The time that the event happens

    node       = rep(rep(1:nmetapop, times = nmetapop), length(times)), ## In which node does the event occur
    dest       = rep(rep(1:nmetapop, each = nmetapop), length(times)), ## Which node is the destination node
    n          = rep(as.vector(transfer_matrix), length(times)), ## How many individuals are moved
    proportion = 0,
    select     = select,
    shift      = shift
  )

  events[events$node != events$dest, ]
}




#' Converts the output of siminf to a long data frame
#'
#' @param U the U object from the model output
#' @param times vector of time steps
#' @param comparts vector of compartment names e.g. S and I
#' @param nmetapop number of metapopulations
#'
#' @return data frame with columns for time, metapopulation, state compartment and the value of the variable
#' @export
#'
convert_siminfU <- function(U, times, comparts, nmetapop){
  data.frame(times = rep(times, times = nmetapop*length(comparts)),
             metapop = rep(1:nmetapop, each = length(comparts)*length(times)),
             state = rep(rep(comparts, each = length(times)), times = nmetapop),
             value = U |> t() |> as.vector())

}

