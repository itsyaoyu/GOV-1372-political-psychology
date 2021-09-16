#' @title Backstabber
#'
#' @description Strategy rules:
#'    1. Cooperates until the opponent defects 3 times.
#'    2. Defects forever after that.
#'    3. Always defects in the last 2 rounds.
#'
backstabber <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)
  n_d <- sum(memory$opponent_play[idx] == "D")
  rounds_left <- sum(is.na(memory$play))

  if (rounds_left <= 2) {
    "D"
  } else {
    if (n_d >= 3) {
      "D"
    } else {
      "C"
    }
  }
}
