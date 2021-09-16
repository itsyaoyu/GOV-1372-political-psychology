#' @title Average Copier
#'
#' @description Strategy rules:
#'    1. Starts with random decision.
#'    2. Cooperates with probability p corresponding to the the cooperation
#'      ratio of the opponent.
#'
averageCopier <- function(opponent, memory) {
  idx <- which(memory$opponent == opponent)

  if (length(idx) == 0) {
    sample(c("C", "D"), 1, prob = c(0.5, 0.5))
  } else {
    p <- sum(memory$opponent_play[idx] == "C") / length(idx)
    sample(c("C", "D"), 1, prob = c(p, 1 - p))
  }
}
