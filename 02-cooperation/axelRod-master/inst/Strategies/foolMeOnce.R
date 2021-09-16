#' @title Fool Me Once
#'
#' @description Strategy rules:
#'    1. Cooperates the first time it plays a new opponent.
#'    2. Keeps cooperating after the first defection of the opponent.
#'    3. Defects systematically after the opponent defects a second time.
#'
foolMeOnce <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)
  n_d <- sum(memory$opponent_play[idx] == "D")

  if (n_d < 2) {
    "C"
  } else {
    "D"
  }
}
