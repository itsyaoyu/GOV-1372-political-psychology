#' @title Majority Player
#'
#' @description Strategy rules:
#'    1. Defects if the opponent defects more often than it cooperates.
#'
majorityPlayer <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)
  n_d <- sum(memory$opponent_play[idx] == "D")
  n_c <- sum(memory$opponent_play[idx] == "C")

  if (n_d > n_c) {
    "D"
  } else {
    "C"
  }
}
