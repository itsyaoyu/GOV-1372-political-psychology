#' @title Appeaser
#'
#' @description Strategy rules:
#'    1. Cooperates the first time it plays a new opponent.
#'    2. Switches between cooperation and defection everytime the opponent
#'      defects.
#'
appeaser <- function(opponent, memory) {
  idx <- tail(which(memory$opponent == opponent), 1)

  if (length(idx) == 0) {
    "C"
  } else {
    if (memory$opponent_play[idx] == "C") {
      memory$play[idx]
    } else {
      if (memory$play[idx] == "C") {
        "D"
      } else {
        "C"
      }
    }
  }
}
