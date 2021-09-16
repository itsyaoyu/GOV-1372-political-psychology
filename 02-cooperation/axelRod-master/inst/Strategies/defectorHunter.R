#' @title Defector Hunter
#'
#' @description Strategy rules:
#'    1. Cooperates for the first 4 rounds initially.
#'    2. After that, defects if opponent defects systematically.
#'
defectorHunter <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)
  n_d <- sum(memory$opponent_play[idx] == "D")

  if ((length(idx) >= 4) & (length(idx) == n_d)) {
    "D"
  } else {
    "C"
  }
}


