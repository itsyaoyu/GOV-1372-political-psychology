#' @title Inverser
#'
#' @description Strategy rules:
#'    1. Cooperates the first time it plays a new opponent.
#'    2. Defects with a probability that diminishes relative to how
#'      long ago the opponent last defected.
#'
inverser <- function(opponent, memory) {
  idx <- which((memory$opponent == opponent) & (memory$opponent_play == "D"))
  current_round <- sum(!is.na(memory$play)) + 1
  last_defection <- tail(idx, 1)
  p <- 1 / (current_round - last_defection)

  if (length(p) == 0) {
    "C"
  } else {
    sample(c("C", "D"), 1, prob = c(p, 1 - p))
  }
}
