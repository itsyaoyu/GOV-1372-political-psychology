#' @title Majority Scrounger
#'
#' @description Strategy rules:
#'    1. Defects if the opponent cooperates more often than it defects
#'
majorityScrounger <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)
  n_d <- sum(memory$opponent_play[idx] == "D")
  n_c <- sum(memory$opponent_play[idx] == "C")

  if (n_c > n_d) {
    "D"
  } else {
    "C"
  }
}
