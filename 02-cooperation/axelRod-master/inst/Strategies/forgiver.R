#' @title Forgiver
#'
#' @description Strategy rules:
#'    1. Cooperates the first time it plays a new opponent.
#'    2. After that, defects if opponent has defected more than 10% of the time.
#'
forgiver <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)

  if (length(idx) == 0) {
    "C"
  } else {
    r_d <- sum(memory$opponent_play[idx] == "D") / length(idx)

    if (r_d > 0.1) {
      "D"
    } else {
      "C"
    }
  }
}
