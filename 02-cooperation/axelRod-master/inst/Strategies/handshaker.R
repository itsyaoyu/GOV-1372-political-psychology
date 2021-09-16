#' @title Handshaker
#'
#' @description Strategy rules:
#'    1. Cooperates the first time it plays a new opponent.
#'    2. Defects the second time it plays the same opponent.
#'    3. If opponent matches its first 2 moves, cooperates forever. If not, defects forever.
#'
handshaker <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)

  if (length(idx) == 0) {
    "C"
  } else if (length(idx) == 1) {
    "D"
  } else if (all(memory$opponent_play[idx][1:2] == c("C", "D"))) {
    "C"
  } else {
    "D"
  }
}
