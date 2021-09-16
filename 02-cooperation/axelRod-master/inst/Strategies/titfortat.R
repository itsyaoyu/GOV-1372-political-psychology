#' @title Tit-For-Tat
#'
#' @description Strategy rules:
#'    1. Cooperates the first time it plays a new opponent.
#'    2. Mirrors the last move of each opponent in subsequent encounters.
#'
titfortat <- function(opponent, memory) {
  idx <- tail(which(memory$opponent == opponent), 1)  # Look in memory when was
                                                      # the last time this
                                                      # opponent was met.

  if (length(idx) == 0) {       # If this opponent was never met...
    "C"                         # ... then cooperate (i.e. return "C").
  } else {                      # Else...
    memory$opponent_play[idx]   # ... play whatever the opponent played last.
  }
}
