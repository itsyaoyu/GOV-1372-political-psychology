#' @title Prober
#'
#' @description Strategy rules:
#'    1. Plays "D", "C", "C" initially.
#'    2. Defects forever if opponent cooperated in moves two and three.
#'    3. Otherwise plays tit-for-tat.
#'
prober <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)

  if (length(idx) == 0) {
    "D"
  } else if (length(idx) == 1) {
    "C"
  } else if (length(idx) == 2) {
    "C"
  } else {
    if (all(memory$opponent_play[2:3] == c("C", "C"))) {
      "D"
    } else {
      tail(memory$opponent_play[idx], 1)
    }
  }
}
