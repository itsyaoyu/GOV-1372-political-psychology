#' @title Cycler
#'
#' @description Strategy rules:
#'    1. Cooperates 3 times in a row, then defect once.
#'    2. Repeats sequence until the end of the tournament.
#'
cycler <- function(opponent, memory, cycle = c("C", "C", "C", "D")) {
  current_round <- sum(!is.na(memory$play)) + 1
  idx <- ifelse((current_round %% length(cycle)) == 0, length(cycle), current_round %% length(cycle))
  cycle[idx]
}
