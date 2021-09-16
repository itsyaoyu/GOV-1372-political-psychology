#' @title Cooperator Hunter
#'
#' @description Strategy rules:
#'    1. Cooperates for the first 4 rounds initially.
#'    2. After that, defects if opponent cooperates systematically.
#'
defectorHunter <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)
  n_c <- sum(memory$opponent_play[idx] == "C")

  if ((length(idx) >= 4) & (length(idx) == n_c)) {
    "D"
  } else {
    "C"
  }
}
