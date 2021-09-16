#' @title Alternator
#'
#' @description Strategy rules:
#'    1. Alternates between cooperation and defection at each round.
#'
alternator <- function(opponent, memory) {
  l <- nrow(memory) - sum(is.na(memory$play))

  if ((l %% 2) == 0) {
    "C"
  } else {
    "D"
  }
}
