#' @title Grudger
#'
#' @description Strategy rules:
#'    1. Cooperates until the opponent defects.
#'    2. Defects forever after that.
#'
grudger <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)

  if (any(memory$opponent_play[idx] == "D")) {
    "D"
  } else {
    "C"
  }
}
